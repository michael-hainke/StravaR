library(httr)       # make API requests
library(jsonlite)   # work with JSON files
library(httpuv)     # HTTP requests
library(plyr)       # data manipulation
library(ggmap)      # mapping functions
library(mapproj)    # mapping functions
library(config)     # config file for credentials
library(gganimate)  # animate maps
library(av)         # video rendering
library(googleway)  # decode polylines
library(tidyverse)  # data manipulation
library(rayshader)   
library(rayvista)   # devtools::install_github("h-a-graham/rayvista")
library(rgdal)      # rayshader
library(geosphere)  # calculate distances

#############################
### Strava Authentication ###
#############################

# Get credentials from .yaml file
strava <- config::get("strava")
app_scope = 'activity:read_all'
cache = TRUE

# Strava authentication function
# (Credit to https://fawda123.github.io/rStrava/)
strava_oauth <- function(app_name, app_client_id, app_secret, app_scope, cache){
  
  strava_app <- oauth_app(appname = app_name, key = app_client_id, secret = app_secret)  
  
  strava_end <- oauth_endpoint(
    request = "https://www.strava.com/oauth/authorize?",
    authorize = "https://www.strava.com/oauth/authorize",
    access = "https://www.strava.com/oauth/token")
  
  oauth2.0_token(endpoint = strava_end, 
                 app = strava_app, 
                 scope = app_scope, 
                 cache = cache)
}

# Authorize User
token <- config(token = strava_oauth(strava$app_name, strava$client_id, strava$secret, app_scope, cache))

#####################################
### Get Activity Data from Strava ###
#####################################

# Get User Activity List
# (Credit to https://bldavies.com/blog/accessing-strava-api/)
df_list <- list()
i <- 1
done <- FALSE
while (!done) {
  req <- GET(
    url = "https://www.strava.com/api/v3/athlete/activities",
    config = token,
    query = list(per_page = 200, page = i)
  )
  df_list[[i]] <- fromJSON(content(req, as = "text"), flatten = TRUE)
  if (length(content(req)) < 200) {
    done <- TRUE
  } else {
    i <- i + 1
  }
}

df <- rbind_pages(df_list)

###############################
### Map Route - Google Maps ###
###############################

activity_id = 1813153282

# Get Google API key and authorize
google <- config::get("google")
ggmap::register_google(key = google$api_key)

# Decode Polyline
route <- google_elevation(polyline = df[df$id==activity_id,'map.summary_polyline'], key = google$api_key)
route_df <- as.data.frame(flatten(route)) %>%
            mutate(id = row_number())

# define bounding box
bounding_box <- ggmap::make_bbox(route_df$location.lng, route_df$location.lat, f = 1.2)

zm = calc_zoom(bounding_box)

# Get Google map and overaly activity with elevation
p <- ggmap(get_googlemap(center = c(lon = mean(route_df$location.lng), lat = mean(route_df$location.lat)),
                         zoom = zm,
                         scale = 1,
                         maptype ='terrain',
                         color = 'color')) +
  geom_path(aes(x = location.lng, y = location.lat, colour = elevation), data = route_df, size = 1, alpha = 1) +
  scale_color_gradient(low = "yellow", high = "red")

# Animate map
q <- p + transition_reveal(id)

animate(q, renderer = av_renderer('animation.mp4'))

#############################
### Map Route - Rayshader ###
#############################

# Determine map center & radius
.lat <- (max(route_df$location.lat)-min(route_df$location.lat))/2 + min(route_df$location.lat)
.long <- (max(route_df$location.lng)-min(route_df$location.lng))/2 + min(route_df$location.lng)

.radius <- max(distm(c(.long, .lat), c(.long, max(route_df$location.lat)), fun = distHaversine),
               distm(c(.long, .lat), c(max(route_df$location.lng),.lat), fun = distHaversine))
.radius <- .radius * 1.5 # add some buffer

# Adjusted elevation (so sits above terrain - gps elevation errors sometimes make sections disappear)
route_df$elevation_adj <- (route_df$elevation + 20)

# Render 3d map
vancouver <- plot_3d_vista(lat = .lat, long = .long, radius=.radius, phi=30, zoom=0.5, fov = 30, theta = -15, zscale=5, windowsize = c(1200, 800))

# Add Strava Route
render_path(extent = attr(vancouver,"extent"),
            lat = route_df$location.lat, long = route_df$location.lng,
            altitude = route_df$elevation_adj, zscale=5,color="red", antialias=TRUE)

# Add Start and Finish Labels
render_label(vancouver,lat = route_df$location.lat[1], long = route_df$location.lng[1],
             extent = attr(vancouver, "extent"),
             zscale=5, textcolor="black", linecolor="black", text = "Start")

render_label(vancouver,lat = route_df$location.lat[length(route_df$location.lat)], long = route_df$location.lng[length(route_df$location.lng)],
             extent = attr(vancouver, "extent"),
             zscale=5, textcolor="black", linecolor="black", text = "Finish")

# Render Movie
render_movie(filename="vancouver", theta = -15, zoom=0.5, fov=30)

# ...or Render a snapshot
render_snapshot(clear=TRUE)

