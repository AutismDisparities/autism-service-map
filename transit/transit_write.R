library(tidyverse)
library(sf)
# Read transit data
cat_stops <- read_csv("./cat/stops.txt")%>%
  select(stop_name, stop_lat, stop_lon)
cobblinc_stops <- read_csv("./cobblinc/stops.txt")%>%
  select(stop_name, stop_lat, stop_lon)
gct_stops <- read_csv("./gct/stops.txt")%>%
  select(stop_name, stop_lat, stop_lon)
marta_stops <- read_csv("./marta/stops.txt")%>%
  select(stop_name, stop_lat, stop_lon)
xpress_stops <- read_csv("./xpress/stops.txt")%>%
  select(stop_name, stop_lat, stop_lon)

stops_all <- bind_rows(cat_stops,cobblinc_stops,gct_stops,marta_stops,xpress_stops)%>%
  st_as_sf(coords = c("stop_lon","stop_lat"))%>%
  st_set_crs(4326)%>%
  st_transform(crs=3519)%>%
  st_buffer(dist = 1320)%>% # Using the definition of 1/4 mi from stop = transit accessible. This does not take the road network into account, just a 1/4mi bubble. 
  st_transform('+proj=longlat +datum=WGS84')

st_write(stops_all,"transit/stops.shp")
# The app will read "stops.shp" and then add the transit accessibility tag to elements of the database. When updating stops.shp, remember to update the date and included carriers in the "About" tab of the map.