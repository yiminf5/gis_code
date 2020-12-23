#first install and library packages that we may use in the project

library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(tidyverse)
library(ggthemes)
library(mapview)
library(RColorBrewer)

#get the NYC Sub-Borough, community district Boundaries
NYCSubBoroughs <- st_read(here::here("final_project", "data", "raw", "NYC_Sub_borough_Area", "NYC_Sub_borough_Area.shp"))
NYCCDistric <- st_read(here::here("final_project", "data", "raw", "nycd_20c", "nycd.shp"))

#Or read in directly.
#NYCSubBoroughs <- st_read("url")

#plot separately and summarize
qtm(NYCSubBoroughs, title = "NYC Sub-Boroughs", borders = '#a6bddb', alpha = 0.5) 
qtm(NYCCDistric, title = "NYC Community Districts", border = '#fec44f', alpha = 0.5)

summary(NYCSubBoroughs)
summary(NYCCDistric)
glimpse(NYCSubBoroughs)
glimpse(NYCCDistric)

# compare the two zoning map, with nyc subb map in blue
#NYCSubBoroughs %>% 
#  ggplot() +
#  geom_sf(color = "#1F77B4") +
#  # nyc cd outlines overlaid in orange
#  geom_sf(data = NYCCDistric, color = "#FF7F0E", size = 1, fill = NA) +
#  ggtitle("NYC District Zoning Comparison") +
#  xlab("Longitude") +
#  ylab("Latitude")

#subway entrances/exit data:
EntExit <- read_csv(here::here("final_project", "data", "raw", "NYC_Entrance_Exit.csv"))

summary(EntExit)
glimpse(EntExit)

#subway line
Subway_Line <- st_read(here::here("final_project", "data", "raw", "Subway Lines", "geo_export_c15233ab-a834-476f-9e67-d2688b85344e.shp"))

summary(Subway_Line)
glimpse(Subway_Line)

#see multiple layers in one plot
tmap_mode("view")

tm_shape(NYCSubBoroughs) +
  tm_polygons(col = NA, alpha = 0.5, border.col = '#a6bddb') +
  tm_shape(NYCCDistric) +
    tm_polygons(col = NA, alpha = 0.5, border.col = '#fec44f') +
  tm_shape(Subway_Line) +
  tm_lines(col = "blue")

#####staten island needs to be removed

#change column names
colnames(EntExit) <- colnames(EntExit) %>% 
  str_to_lower() %>% 
  str_replace_all(" ", "_")

colnames(EntExit)

#####haven't check in detail
EntExit %>%
  count(line)

# number of unique subway station names:
length(unique(EntExit$station_name))
#  station names are often reused in NYC, not a good way

# what the columns look like:
EntExit %>% 
  select(division:station_name) %>% 
  distinct() %>% 
  head()

# count unique division, line, and station_name column combinations:
EntExit %>% 
  select(division:station_name) %>% 
  distinct() %>% 
  nrow()

EntExit %>% 
  select(station_name, station_latitude:station_longitude) %>% 
  distinct() %>% 
  nrow()

EntExit_key <- EntExit %>% 
  # convert the 3 columns to lowercase
  mutate_at(vars(division:station_name), str_to_lower) %>% 
  # create a unique key for each station
  unite("stat_name", division:station_name, sep = "_") %>% 
  # capitalize all of the route names (to fix the e issue)
  mutate_at(vars(route1:route11), str_to_upper) 
# result:
head(EntExit_key %>% select(stat_name:station_longitude))

# coordinates fix:
# To fix the issue of the same station having multiple geographical coordinates
# take the average of the latitude and longitude for each station 
EntExit_key <- EntExit_key %>% 
  # get rid of original coordinates:
  select(-c(station_latitude:station_longitude)) %>% 
  distinct() %>% 
  # join onto average coordinates:
  left_join(
    # get the average lat and average long for each station:
    EntExit_key %>% 
      select(stat_name:station_longitude) %>% 
      distinct() %>% 
      group_by(stat_name) %>% 
      summarize(
        avg_stat_lat = mean(station_latitude),
        avg_stat_long = mean(station_longitude)
      ),
    by = "stat_name"
  )
# resulting number of stations:
length(unique(EntExit_key$stat_name))

# number of unique geographical coordinates:
EntExit_key %>% 
  select(avg_stat_lat:avg_stat_long) %>% 
  distinct() %>% 
  nrow()

# 465-462 some stations have same coordinates


