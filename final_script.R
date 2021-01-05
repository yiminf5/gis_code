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
NYCNeighbor <- st_read(here::here("final_project", "data", "raw", "Neighborhood Tabulation Areas (NTA)", "geo_export_55f681a9-1f00-4361-9b5b-147a0b761591.shp"))

#Or read in directly.
#NYCSubBoroughs <- st_read("url")

#plot separately and summarize
qtm(NYCSubBoroughs, title = "NYC Sub-Boroughs", borders = '#a6bddb', alpha = 0.5) 
qtm(NYCCDistric, title = "NYC Community Districts", border = '#fec44f', alpha = 0.5)
qtm(NYCNeighbor, title = "NYC Neighborhoods", border = '#addd8e', alpha = 0.5)

summary(NYCSubBoroughs)
summary(NYCCDistric)
summary(NYCNeighbor)
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

#nyu neighborhood indicator data
ni_all <- read_csv(here::here("final_project", "data", "raw", "ni_202006.csv"))

summary(EntExit)
glimpse(EntExit)

#subway line
Subway_Line <- st_read(here::here("final_project", "data", "raw", "Subway Lines", "geo_export_c15233ab-a834-476f-9e67-d2688b85344e.shp"))

summary(Subway_Line)
glimpse(Subway_Line)

#subway line info
trunk_line_tidy <- st_read(here::here("final_project", "data", "raw", "nyc_trunk_line.csv"))
summary(trunk_line_tidy)
glimpse(trunk_line_tidy)

#see multiple layers in one plot
tmap_mode("view")

tm_shape(NYCSubBoroughs) +
  #tm_polygons(col = "NAME", alpha = 0.5, border.col = '#a6bddb') +
  tm_polygons(col = NA, alpha = 0.5, border.col = '#a6bddb') +
  #tm_shape(NYCCDistric) +
    #tm_polygons(col = NA, alpha = 0.5, border.col = '#fec44f') +
  tm_shape(Subway_Line) +
  tm_lines(col = "blue")

#####staten island needs to be removed
NYCSubBoroughs <- NYCSubBoroughs %>% 
  filter(bor_subb < 500)

NYCCDistric <- NYCCDistric %>% 
  filter(BoroCD < 500)

NYCNeighbor <- NYCNeighbor %>% 
  filter(boro_code < 5)

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

# relationship between the entrance type and the ADA-accessibility rating 
# count each entrance type:
table(EntExit_key$entrance_type)
# count ada ratings:
table(EntExit_key$ada)

# ADA rating on a per-station or per-entrance basis?
EntExit_key %>% 
  group_by(stat_name) %>% 
  summarize(
    # count total number of entrances per station:
    num_entry = n(),
    # ada is TRUE/FALSE - can sum to get number of ada = TRUE per station:
    num_ada = sum(ada),
    # % ada out of total num of entrances, per station:
    percent_ada = num_ada * 100 / num_entry
  ) %>% 
  {table(.$percent_ada)}
# ADA TRUE/FALSE rating is given to the entire station 
# not to the particular entrance/exit
# entrances/exits columns a lot less interesting

# plot the most common entrance/exit types for ADA-accessible and not accessible stations
EntExit_key %>% 
  group_by(entrance_type, ada) %>% 
  count() %>% 
  ggplot(aes(entrance_type, n, fill = ada)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Entrance/Exit type") +
  ylab("Count") +
  scale_fill_tableau(name = "ADA Status") +
  ggtitle("Entrance/Exit count by ADA Status and facility type")

# table
EntExit_key %>% 
  group_by(entrance_type, ada) %>% 
  count() 

# switch perspectives
EntExit_key %>% 
  group_by(entrance_type, ada) %>% 
  count() %>% 
  ggplot(aes(entrance_type, n, fill = ada)) +
  geom_bar(stat = "identity", position = "fill") +
  xlab("Entrance/Exit type") +
  ylab("Proportion") +
  scale_fill_tableau(name = "ADA Status") +
  ggtitle("Entrance/Exit count by ADA Status and type\n(Bars fill position)")

#tidying 
EntExit_s <- EntExit_key %>% 
  # select relevant columns, discard all others from this point on
  select(stat_name, avg_stat_lat:avg_stat_long, route1:route11, ada, ada_notes) %>% 
  distinct() %>% 
  # reformat the route columns into a long format
  gather("route_num", "route_name", route1:route11) %>% 
  # get rid of the many NAs in the route column that were there due to the formatting
  filter(!is.na(route_name)) %>% 
  select(-route_num) %>% 
  distinct()
# new format dimensions:
dim(EntExit_s)
head(EntExit_s)

# number of unique stations and train route combinations:
#some station may support different route, kept since represents the accessibility of the region
EntExit_s %>% 
  select(stat_name, route_name) %>% 
  distinct() %>% 
  nrow()

# missing values check:
sapply(EntExit_s, anyNA)

EntExit_s %>%
  count(route_name)
#  each route now in its own row instead of in columns

# check NAs in ada notes
EntExit_s %>% 
  select(stat_name, ada, ada_notes) %>% 
  distinct() %>% 
  filter(!(is.na(ada_notes))) %>% 
  arrange(stat_name, ada)
# some stations are not fully accessible for all train routes
# some in planning

# get the slice of F train stations that are missing from the G route: 
EntExit_s %>% 
  filter(route_name == "F") %>% 
  arrange(avg_stat_lat) %>% 
  filter(avg_stat_lat > 40.62976 & avg_stat_lat < 40.68030)

# extra station for the connected 4 Av-9 St stations (the stop after Smith-Ninth Streets):
# G train is considered IND - grab that 4th ave station
EntExit_s <- EntExit_s %>% 
  # create a copy of the F train station stops for this section of track
  bind_rows(
    EntExit_s %>% 
      filter(route_name == "F") %>% 
      arrange(avg_stat_lat) %>% 
      filter(avg_stat_lat > 40.63612 & avg_stat_lat < 40.67358 & str_detect(stat_name, "ind")) %>% 
      # change the route name for this section 
      mutate(route_name = "G")
  )
# example at Church Ave
EntExit_s %>%
  filter(stat_name == "ind_6 avenue_church av")

# new number of G train stations:
EntExit_s %>%
  filter(route_name == "G") %>% 
  nrow()

#tidying again, with wach change a seperate mutate
EntExit_ada_updt <- EntExit_s %>% 
  # change world trade center station ada
  mutate(ada = ifelse((stat_name == "ind_8 avenue_world trade center" & route_name != "E"), FALSE, ada)) %>% 
  # change times square shuttle ada, filter out extra "S" route
  filter(route_name != "S") %>% 
  mutate(ada = ifelse(route_name == "GS", FALSE, ada)) %>% 
  # change 4/5/6 at union square to ada = FALSE
  mutate(ada = ifelse(
    ((stat_name == "bmt_broadway_union square" | stat_name == "bmt_canarsie_union square") & (route_name == "4" | route_name == "5" | route_name == "6")),
    FALSE, ada
  )) %>% 
  # change kingsbridge rd ada = TRUE
  mutate(ada = ifelse(stat_name == "ind_concourse_kingsbridge rd", TRUE, ada)) %>% 
  # change Lex / 23rd St stop to TRUE
  mutate(ada = ifelse(stat_name == "irt_lexington_23rd st", TRUE, ada)) %>% 
  # change J/Z at Brooklyn Bridge / City Hall to ada = FALSE
  mutate(ada = ifelse(
    (stat_name == "irt_lexington_brooklyn bridge-city hall" & (route_name == "J" | route_name == "Z")),
    FALSE, ada
  )) %>% 
  # change irt_lexington_canal st to ada = TRUE for 6 train only
  mutate(ada = ifelse(
    (stat_name == "irt_lexington_canal st" & route_name != "6"), FALSE, ada
  )) %>% 
  # change rt 6 hunts point av to ada = TRUE
  mutate(ada = ifelse(stat_name == "irt_pelham_hunts point av", TRUE, ada)) %>% 
  # convert the forst hills / 71st ave station to ada = TRUE for all lines
  mutate(ada = ifelse(stat_name == "ind_queens boulevard_forest hills-71st av", TRUE, ada)) %>% 
  # can get rid of the ada_notes column now
  select(-ada_notes) %>% 
  distinct()

# original length:
dim(EntExit_s)

# updated version slightly smaller:
dim(EntExit_ada_updt)

# original S route:
EntExit_s %>% 
  filter(route_name == "S")

# Now S route is gone:
EntExit_ada_updt %>% 
  filter(route_name == "S")

# GS still there, but has duplicate stops:
EntExit_ada_updt %>% 
  filter(route_name == "GS")

### GS (Manhattan)
EntExit_ada_updt %>% 
  filter(route_name == "GS") %>% 
  arrange(stat_name)

# GS has 2 stations for each of its stops, keep only the "42nd st shuttle" stops:
EntExit_num_updt <- EntExit_ada_updt %>% 
  filter(!(route_name == "GS" & (str_detect(stat_name, "flushing") | str_detect(stat_name, "lexington"))))

### FS (Brooklyn)
EntExit_num_updt %>% 
  filter(route_name == "FS") %>% 
  arrange(avg_stat_lat)

# problem: 3 franklin ave stations, when there should only be one
# solution: remove the ind and irt stations - those are the extras
EntExit_num_updt <- EntExit_num_updt %>% 
  filter(!(route_name == "FS" & (str_detect(stat_name, "irt") | str_detect(stat_name, "ind"))))
EntExit_num_updt %>% 
  filter(route_name == "FS")


### G
EntExit_num_updt <- EntExit_num_updt %>%
  filter(route_name != "G") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      filter(route_name == "G" & str_detect(stat_name, "ind") & stat_name != "ind_queens boulevard_23rd st-ely av")
  )
# Check:
EntExit_num_updt %>% 
  filter(route_name == "G") %>% 
  nrow()

### Z
EntExit_num_updt <- EntExit_num_updt %>% 
  filter(route_name != "Z") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      filter(route_name == "Z" & str_detect(stat_name, "bmt") & stat_name != "bmt_broadway_canal st (ul)") %>% 
      # add Z train to the broadway junction stop in Queens (was A/C/J/L)
      bind_rows(
        EntExit_num_updt %>% 
          filter(str_detect(stat_name, "broadway junction")) %>% 
          mutate(route_name = "Z") %>% 
          distinct()
      ) %>% 
      # add Z train to the Alabama Ave stop in Queens (was J only)
      bind_rows(
        EntExit_num_updt %>% 
          filter(str_detect(stat_name, "alabama")) %>% 
          mutate(route_name = "Z")
      ) %>% 
      # add back in the jamaica center and jfk airport stops that were filtered out earlier based on the "bmt" filter
      bind_rows(
        EntExit_num_updt %>% 
          filter(route_name == "Z" & avg_stat_long > -73.82829)
      )
  )
# Check:
EntExit_num_updt %>% 
  filter(route_name == "Z") %>% 
  nrow()

### 7
EntExit_num_updt <-  EntExit_num_updt %>%
  filter(route_name != "7") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      filter(route_name == "7" & str_detect(stat_name, "irt")) %>% 
      # eliminate station copies at times square and grand central
      filter(!(
        stat_name %in% c(
          "irt_42nd st shuttle_times square", "irt_42nd st shuttle_grand central",
          "irt_lexington_grand central-42nd st"
        )
      )) %>% 
      # convert ADA = TRUE at the court sq station (was incorrectly FALSE)
      mutate(ada = ifelse(stat_name == "irt_flushing_45 rd-court house sq", TRUE, ada))
  )
# Check:
EntExit_num_updt %>% 
  filter(route_name == "7") %>% 
  nrow()

### E
EntExit_num_updt <- EntExit_num_updt %>% 
  filter(route_name != "E") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      filter(route_name == "E" & str_detect(stat_name, "ind") &  stat_name != "ind_8 avenue_chambers st") %>% 
      # ADA fix
      mutate(ada = ifelse(stat_name == "ind_archer av_jamaica-van wyck", TRUE, ada)) %>% 
      # E train to Briarwood station
      bind_rows(
        EntExit_num_updt %>% 
          filter(str_detect(stat_name, "briarwood")) %>% 
          mutate(route_name = "E")
      )
  )
# Check:
EntExit_num_updt %>% 
  filter(route_name == "E") %>% 
  nrow()

### L
EntExit_num_updt <-  EntExit_num_updt %>% 
  filter(route_name != "L") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      # remove extra stations
      filter(route_name == "L" & str_detect(stat_name, "bmt") & stat_name != "bmt_broadway_union square") %>% 
      # correct ADA status
      mutate(ada = ifelse(stat_name == "bmt_canarsie_wilson av", TRUE, ada)) %>% 
      # add L train to broadway junction
      bind_rows(
        EntExit_num_updt %>% 
          filter(str_detect(stat_name, "broadway junction") & route_name == "L")
      )
  )
# Check:
EntExit_num_updt %>% 
  filter(route_name == "L") %>% 
  nrow()

### B
EntExit_num_updt <- EntExit_num_updt %>% 
  filter(route_name != "B") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      filter(route_name == "B" & (str_detect(stat_name, "bmt") | str_detect(stat_name, "ind"))) %>% 
      # convert non-ada stations to those that are ada = TRUE now
      mutate(ada = ifelse(
        stat_name %in% c(
          "bmt_brighton_kings highway", "ind_6 avenue_broadway-lafayette st", "ind_8 avenue_125th st"
        ),
        TRUE, ada)) %>% 
      # stations to exclude: 
      # atlantic ave /barclays duplicates and stops between barclays and brighton where B does not stop
      filter(!(avg_stat_lat > 40.60867 & avg_stat_lat < 40.63508)) %>% 
      filter(!(
        stat_name %in% c(
          "bmt_broadway_34th st", "bmt_brighton_parkside av", 
          "bmt_4 avenue_pacific st", "bmt_brighton_atlantic av", 
          "bmt_brighton_av u", "bmt_brighton_neck rd", 
          "bmt_brighton_beverly rd", "bmt_brighton_cortelyou rd"
        )
      ))
  )
# Check:
EntExit_num_updt %>% 
  filter(route_name == "B") %>% 
  nrow()

### 4
EntExit_num_updt <- EntExit_num_updt %>% 
  filter(route_name != "4") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      filter(route_name == "4" & str_detect(stat_name, "irt")) %>% 
      filter(!(
        stat_name %in% c(
          "irt_flushing_grand central-42nd st", "irt_42nd st shuttle_grand central", 
          "irt_clark_fulton st", "irt_clark_borough hall"
        )
      )) %>% 
      mutate(ada = ifelse(stat_name == "irt_lexington_fulton st", TRUE, ada))
  )
# Check:
EntExit_num_updt %>% 
  filter(route_name == "4") %>% 
  nrow()

### J
EntExit_num_updt <- EntExit_num_updt %>% 
  filter(route_name != "J") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      filter(route_name == "J" & str_detect(stat_name, "bmt") & stat_name != "bmt_broadway_canal st (ul)") %>%
      # add back in jamaica center and the jfk airport stop
      bind_rows(
        EntExit_num_updt %>% 
          filter(route_name == "J" & avg_stat_long > -73.82829)
      ) %>%
      # add J train to broadway junction
      bind_rows(
        EntExit_num_updt %>% 
          filter(str_detect(stat_name, "broadway junction") & route_name == "J")
      ) %>% 
      mutate(ada = ifelse(stat_name == "bmt_nassau_fulton st", TRUE, ada))
  )
# Check:
EntExit_num_updt %>% 
  filter(route_name == "J") %>% 
  nrow()

### A
EntExit_num_updt <- EntExit_num_updt %>% 
  filter(route_name != "A") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      filter(route_name == "A" & str_detect(stat_name, "ind")) %>%
      # remove stations where the A does not stop
      filter(!(
        stat_name %in% c(
          "ind_8 avenue_world trade center", "ind_8 avenue_broadway-nassau", 
          "ind_fulton_franklin av", "ind_fulton_kingston-throop",
          "ind_fulton_ralph av", "ind_fulton_rockaway av",
          "ind_fulton_liberty av", "ind_fulton_van siclen av", 
          "ind_fulton_shepherd av"
        )
      )) %>% 
      # add in the fulton st stop in manhattan
      bind_rows(
        EntExit_num_updt %>% 
          filter(str_detect(stat_name, "fulton st") & route_name == "4") %>% 
          mutate(route_name = "A")
      ) %>% 
      # ada fixes
      mutate(ada = ifelse(
        stat_name %in% c(
          "ind_8 avenue_125th st", "ind_rockaway_far rockaway-mott av",
          "ind_rockaway_aqueduct racetrack", "ind_fulton_jay st - borough hall",
          "ind_fulton_utica av", "ind_liberty_lefferts blvd"
        ), 
        TRUE, ada
      )) %>% 
      # add in rockaway beach stops that A makes during rush hour
      bind_rows(
        EntExit_num_updt %>% 
          filter(route_name == "H" & !(str_detect(stat_name, "broad channel"))) %>% 
          mutate(route_name = "A")
      )
  )
# Check:
EntExit_num_updt %>% 
  filter(route_name == "A") %>% 
  nrow()

### C
EntExit_num_updt <- EntExit_num_updt %>% 
  filter(route_name != "C") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      filter(route_name == "C" & str_detect(stat_name, "ind")) %>% 
      filter(!(stat_name %in% c("ind_8 avenue_broadway-nassau", "ind_8 avenue_world trade center"))) %>% 
      bind_rows(
        EntExit_num_updt %>% 
          filter(str_detect(stat_name, "fulton st") & route_name == "A") %>% 
          mutate(route_name = "C")
      ) %>%
      mutate(ada = ifelse(
        stat_name %in% c("ind_8 avenue_125th st", "ind_fulton_jay st - borough hall", "ind_fulton_utica av"),
        TRUE, ada))
  )
# Check:
EntExit_num_updt %>% 
  filter(route_name == "C") %>% 
  nrow()

### 5
EntExit_num_updt <- EntExit_num_updt %>% 
  filter(route_name != "5") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      filter(route_name == "5" & str_detect(stat_name, "irt")) %>% 
      filter(!(
        stat_name %in% c(
          "irt_clark_borough hall", "irt_clark_fulton st", 
          "irt_flushing_grand central-42nd st", "irt_42nd st shuttle_grand central", 
          "irt_white plains road_wakefield-241st st"
        )
      )) %>%
      mutate(ada = ifelse(
        stat_name %in% c(
          "irt_lexington_fulton st", "irt_white plains road_east 180th st",
          "irt_white plains road_gun hill rd"
        ),
        TRUE, ada))
  )
# Check
EntExit_num_updt %>% 
  filter(route_name == "5") %>% 
  nrow()

### 6
EntExit_num_updt <- EntExit_num_updt %>% 
  filter(route_name != "6") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      filter(route_name == "6" & str_detect(stat_name, "irt")) %>% 
      filter(!(stat_name %in% c("irt_flushing_grand central-42nd st", "irt_42nd st shuttle_grand central"))) %>% 
      mutate(ada = ifelse(stat_name %in% c("irt_lexington_bleecker st"), TRUE, ada))
  )
# Check:
EntExit_num_updt %>% 
  filter(route_name == "6") %>% 
  nrow()

### D
EntExit_num_updt <- EntExit_num_updt %>% 
  filter(route_name != "D") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      # the D train stops are a mix of ind and bmt 
      filter(route_name == "D" & (str_detect(stat_name, "ind") | str_detect(stat_name, "bmt"))) %>% 
      filter(!(
        stat_name %in% c(
          "bmt_broadway_34th st", "bmt_4 avenue_pacific st", 
          "bmt_brighton_atlantic av", "bmt_sea beach_new utrecht av", 
          "bmt_brighton_stillwell av")
      )) %>% 
      # add in the brooklyn 36th st stop:
      bind_rows(
        EntExit_num_updt %>% 
          filter(str_detect(stat_name, "4 avenue_36")) %>% 
          mutate(route_name = "D") %>% 
          distinct()
      ) %>% 
      mutate(ada = ifelse(
        stat_name %in% c("ind_8 avenue_125th st", "ind_6 avenue_broadway-lafayette st", "bmt_west end_bay parkway"),
        TRUE, ada))
  )
# is the number of stations correct now?
EntExit_num_updt %>% 
  filter(route_name == "D") %>% 
  nrow()

### F
EntExit_num_updt <- EntExit_num_updt %>% 
  filter(route_name != "F") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      filter(route_name == "F" & (str_detect(stat_name, "ind") | str_detect(stat_name, "bmt"))) %>% 
      filter(!(
        stat_name %in% c(
          "bmt_broadway_34th st", "bmt_canarsie_6th av", 
          "bmt_nassau_essex st", "bmt_broadway_lawrence st", 
          "bmt_4 avenue_9th st", "bmt_brighton_stillwell av", 
          "bmt_brighton_west 8th st"
        )
      )) %>% 
      mutate(ada = ifelse(
        stat_name %in% c("ind_6 avenue_broadway-lafayette st", "ind_fulton_jay st - borough hall"),
        TRUE, ada))
  )
# Check:
EntExit_num_updt %>% 
  filter(route_name == "F") %>% 
  nrow()

### M
EntExit_num_updt <- EntExit_num_updt %>% 
  filter(route_name != "M") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      filter(route_name == "M" & (str_detect(stat_name, "ind") | str_detect(stat_name, "bmt"))) %>% 
      filter(!(stat_name %in% c("bmt_broadway_34th st", "bmt_canarsie_6th av", "bmt_nassau_essex st"))) %>% 
      mutate(ada = ifelse(stat_name %in% c("ind_6 avenue_broadway-lafayette st"), TRUE, ada))
  )
# Check:
EntExit_num_updt %>% 
  filter(route_name == "M") %>% 
  nrow()

### 1
EntExit_num_updt <- EntExit_num_updt %>% 
  filter(route_name != "1") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      filter(route_name == "1" & str_detect(stat_name, "irt")) %>%
      filter(!(stat_name %in% c("irt_42nd st shuttle_times square", "irt_clark_park place"))) %>% 
      # add in the re-opened WTC Cortlandt station (doesn't exist in dataset, coord from wikipedia)
      bind_rows(
        tibble(
          stat_name = "irt_broadway-7th ave_wtc cortlandt", ada = TRUE, 
          avg_stat_lat = 40.7115, avg_stat_long = -74.012, route_name = "1")
      ) %>% 
      mutate(ada = ifelse(
        stat_name %in% c("irt_broadway-7th ave_dyckman st", "irt_broadway-7th ave_168th st"),
        TRUE, ada))
  )
# Check:
EntExit_num_updt %>% 
  filter(route_name == "1") %>% 
  nrow()

### 2
EntExit_num_updt <- EntExit_num_updt %>% 
  filter(route_name != "2") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      filter(route_name == "2" & str_detect(stat_name, "irt")) %>% 
      filter(!(
        stat_name %in% c(
          "irt_42nd st shuttle_times square", "irt_lexington_fulton st",
          "irt_lexington_borough hall"
        )
      )) %>% 
      mutate(ada = ifelse(
        stat_name %in% c(
          "irt_white plains road_gun hill rd", "irt_white plains road_east 180th st",
          "irt_clark_fulton st"
        ),
        TRUE, ada))
  )
# Check:
EntExit_num_updt %>% 
  filter(route_name == "2") %>% 
  nrow()

### 3
EntExit_num_updt <- EntExit_num_updt %>% 
  filter(route_name != "3") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      filter(route_name == "3" & str_detect(stat_name, "irt")) %>% 
      filter(!(
        stat_name %in% c("irt_42nd st shuttle_times square", "irt_lexington_fulton st", "irt_lexington_borough hall")
      )) %>%
      mutate(ada = ifelse(stat_name %in% c("irt_clark_fulton st"), TRUE, ada))
  )
# Check:
EntExit_num_updt %>% 
  filter(route_name == "3") %>% 
  nrow()

### R
EntExit_num_updt <- EntExit_num_updt %>% 
  filter(route_name != "R") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      filter(route_name == "R" & (str_detect(stat_name, "bmt") | str_detect(stat_name, "ind"))) %>% 
      filter(!(stat_name %in% c(
        "ind_6 avenue_smith-9th st", "bmt_4 avenue_pacific st", 
        "bmt_brighton_atlantic av", "ind_fulton_jay st - borough hall", 
        "bmt_nassau_canal st", "bmt_canarsie_union square", 
        "ind_6 avenue_34th st", "ind_8 avenue_42nd st"
      ))) %>% 
      mutate(ada = ifelse(stat_name %in% c("bmt_broadway_lawrence st", "bmt_broadway_cortlandt st"), TRUE, ada))
  )
# Check:
EntExit_num_updt %>% 
  filter(route_name == "R") %>% 
  nrow()

### N
EntExit_num_updt <- EntExit_num_updt %>% 
  filter(route_name != "N") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      filter(route_name == "N" & str_detect(stat_name, "bmt")) %>% 
      filter(!(stat_name %in% c(
        "bmt_brighton_stillwell av", "bmt_west end_62nd st", 
        "bmt_brighton_atlantic av", "bmt_4 avenue_pacific st", 
        "bmt_nassau_canal st", "bmt_canarsie_union square"
      ))) %>% 
      # add back in queensboro plaza, only route that has irt division instead of bmt
      bind_rows(EntExit_num_updt %>% filter(route_name == "N" & str_detect(stat_name, "queensboro")))
  )
# Check:
EntExit_num_updt %>% 
  filter(route_name == "N") %>% 
  nrow()

### W
EntExit_num_updt <- EntExit_num_updt %>% 
  bind_rows(
    EntExit_num_updt %>% 
      filter(route_name == "N" & avg_stat_lat > 40.68367) %>% 
      bind_rows(
        EntExit_num_updt %>% 
          filter(route_name == "R") %>% 
          filter(avg_stat_lat > 40.69410 & avg_stat_lat < 40.71952)
      ) %>% 
      mutate(route_name = "W")
  )
# Check:
EntExit_num_updt %>% 
  filter(route_name == "W") %>% 
  nrow()

### Q
EntExit_num_updt <- EntExit_num_updt %>% 
  filter(route_name != "Q") %>% 
  bind_rows(
    EntExit_num_updt %>% 
      filter(route_name == "Q" & !str_detect(stat_name, "astoria") & str_detect(stat_name, "bmt")) %>%
      # add in 3 new 2nd Ave stops at 72nd St, 86th St, and 96th St
      bind_rows(
        tibble(
          # assign names to match existing pattern
          stat_name = c("ind_2 avenue_72nd st", "ind_2 avenue_86th st", "ind_2 avenue_96th st"), 
          # all are ADA = TRUE
          ada = rep(TRUE, 3), 
          # lat and long from wikipedia pages for eachs tation
          avg_stat_lat = c(40.768889, 40.777861, 40.7841), 
          avg_stat_long = c(-73.958333, -73.95175, -73.9472),
          # only the Q stops at these stations on a regular schedule
          route_name = rep("Q", 3)
        )
      ) %>% 
      # add in the 63rd St, where only the F used to stop, but now the Q also stops there
      bind_rows(
        EntExit_num_updt %>% 
          filter(stat_name == "ind_63rd street_lexington av") %>% 
          mutate(route_name = "Q")
      ) %>% 
      filter(!(stat_name %in% c(
        "bmt_broadway_5th av", "bmt_broadway_lexington av", 
        "bmt_broadway_49th st", "bmt_canarsie_union square", 
        "bmt_nassau_canal st", "bmt_4 avenue_pacific st", 
        "bmt_brighton_atlantic av", "bmt_coney island_stillwell av", 
        "bmt_coney island_west 8th st"
      ))) %>%
      mutate(ada = ifelse(stat_name %in% c("bmt_brighton_av h", "bmt_brighton_kings highway"), TRUE, ada))
  )
# Check:
EntExit_num_updt %>% 
  filter(route_name == "Q") %>% 
  nrow()

# original station / train route count:
nrow(EntExit_ada_updt)
#982

# new station / train route count:
nrow(EntExit_num_updt)
#750

##### As a result of the data cleaning process:

##### Over 200 duplicate or outdated subway station-train route associations have been removed.
##### Some station ADA-accessibility ratings have been updated

### Analysis and Visualization

# convert the subway data to a spatial object and assign it the most likely CRS:
sub_nyc_crs <- st_as_sf(EntExit_num_updt, coords = c("avg_stat_long", "avg_stat_lat"), crs = "+init=epsg:4326")
head(sub_nyc_crs)

# change the CRS to match the map shapefiles:
sub_nyc_crs <- st_transform(sub_nyc_crs, crs = st_crs(NYCSubBoroughs))
head(sub_nyc_crs)

trunk_nyc_crs <- sub_nyc_crs %>%
  mutate(route_name = ifelse(route_name %in% c("FS", "GS", "H"), "S", route_name)) %>% 
  left_join(trunk_line_tidy, by = "route_name")

summary(trunk_nyc_crs) 

trunk_nyc_crs %>%
  count(route_name)

# map with line color
tmap_mode("view")

tm_shape(NYCSubBoroughs) +
  tm_polygons(col = NA, alpha = 0.5, border.col = '#a6bddb') +
tm_shape(NYCCDistric) +
  tm_polygons(col = NA, alpha = 0.5, border.col = '#fec44f') +
tm_shape(trunk_nyc_crs)+
  tm_dots(
    col = "primary_trunk_line", 
    palette = c("#fccc0a", "#a7a9ac", "#996633", "#6cbe45", "#2850ad", "#ff6319", "#ee352e", "#b933ad", "#00933c", "#808183"),
    alpha= 0.5)+
  tm_layout(legend.outside = TRUE)

### stations by route
stat_by_route <- EntExit_num_updt %>% 
  group_by(route_name) %>% 
  summarize(
    # total number of stations by route:
    total_stat = n(),
    # ada is TRUE/FALSE - use this to get total number of ada stations by route:
    num_ada = sum(ada),
    # percent of total num of station stops that are ada, by route:
    per_ada = round(num_ada * 100 / total_stat, 2)
  ) %>% 
  # convert the 3 shuttles into route_name == "S", to match other datasets
  mutate(rt_mod = ifelse(route_name == "GS" | route_name == "FS" | route_name == "H", "S", route_name)) %>% 
  # join to get subway route groupings, colors
  left_join(trunk_line_tidy, by = c("rt_mod" = "route_name")) %>% 
  arrange(primary_trunk_line, route_name)
# create factor for nice route order in plots
stat_by_route$rt_order <- factor(stat_by_route$route_name, levels = stat_by_route$route_name)

# average number of subway stations per route
mean(stat_by_route$total_stat)

# plot total subway number
stat_by_route %>% 
  ggplot(aes(rt_order, total_stat, fill = primary_trunk_line)) +
  geom_bar(stat = "identity") +
  # dashed line for mean number of total stations
  geom_hline(yintercept = mean(stat_by_route$total_stat), linetype = "dashed") +
  scale_fill_manual(values = c("#fccc0a", "#a7a9ac", "#996633", "#6cbe45", "#2850ad", "#ff6319", "#ee352e", "#b933ad", "#00933c", "#808183"), 
                    name = "Trunk Line") +
  ylab("Total number of stations") +
  xlab("Subway route") +
  ggtitle("Total number of subway stations per route")

# average number of ada-accessible stations per route:
mean(stat_by_route$num_ada)

# ADA per route
stat_by_route %>% 
  ggplot(aes(rt_order, num_ada, fill = primary_trunk_line)) +
  geom_bar(stat = "identity") +
  # dashed line for mean number of accessible stations per route
  geom_hline(yintercept = mean(stat_by_route$num_ada), linetype = "dashed") +
  scale_fill_manual(values = c("#fccc0a", "#a7a9ac", "#996633", "#6cbe45", "#2850ad", "#ff6319", "#ee352e", "#b933ad", "#00933c", "#808183"), 
                    name = "Trunk Line") +
  ylab("Number of ADA stations") +
  xlab("Subway route") +
  ggtitle("Number of ADA-accessible subway stations per route")

# average percent accessible statious out of total
round(mean(stat_by_route$per_ada), 2)

# percent ada
stat_by_route %>% 
  ggplot(aes(rt_order, per_ada, fill = primary_trunk_line)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(stat_by_route$per_ada), linetype = "dashed") +
  scale_fill_manual(values = c("#fccc0a", "#a7a9ac", "#996633", "#6cbe45", "#2850ad", "#ff6319", "#ee352e", "#b933ad", "#00933c", "#808183"), 
                    name = "Trunk Line") +
  ylab("Percent of stations that are ADA") +
  xlab("Subway route") +
  ggtitle("Percent of ADA-accessible subway stations per route")

# lower accessibility
stat_by_route %>% 
  filter(per_ada < 20) %>% 
  select(route_name:per_ada) %>% 
  arrange(desc(per_ada))

# higher accessibility
stat_by_route %>% 
  filter(per_ada > 40) %>% 
  select(route_name:per_ada) %>% 
  arrange(desc(per_ada))

# barbell
stat_by_route <- stat_by_route %>% 
  arrange(total_stat, num_ada) %>% 
  mutate(total_stat_order = factor(route_name, levels = route_name))
stat_by_route %>% 
  ggplot(aes(total_stat_order, total_stat)) +
  geom_segment(aes(x = total_stat_order, xend = total_stat_order, y = num_ada, yend = total_stat), size = 1, alpha= 0.5, color = "gray60") +
  # total number of station stops on route in orange
  geom_point(size = 3, color = "#E69F00") +
  # number of ada-accessible station stops on route in blue
  geom_point(aes(total_stat_order, num_ada), size = 3, color = "#56B4E9") +
  xlab("Route name") +
  ylab("Number of stations") +
  ggtitle("Total number of stations (orange) vs number of ADA-accessible stations (blue) per route") +
  coord_flip()

######next compare between subb
head(trunk_nyc_crs)

nyc_subb_subw <- st_join(trunk_nyc_crs, NYCSubBoroughs)
head(nyc_subb_subw)

nyc_subb_subw %>%
  count(NAME)

# group by and summarize station-route counts by borough:
subb_stat_count <- nyc_subb_subw %>%
  group_by(NAME) %>% 
  summarize(
    # total number of station stops
    tot_stat = n(),
    # ada station stop count
    ada_stat = sum(ada),
    not_ada = tot_stat - ada_stat,
    # percet of stations that are ada:
    ada_percent = ada_stat * 100 / tot_stat
  ) %>% 
  # add in neighborhoods with no subway stations
  bind_rows(
    NYCSubBoroughs %>% 
      as_tibble() %>% 
      filter(!(NAME %in% nyc_subb_subw$NAME)) %>% 
      select(NAME) %>% 
      mutate(
        tot_stat = 0,
        ada_stat = 0,
        not_ada = 0, 
        ada_percent = NA
      )
  ) %>% 
  # order by station count, from most to least
  arrange(desc(tot_stat)) %>% 
  mutate(tot_plot_ord = factor(NAME, levels = NAME))

# BAR PLOT BY subb
subb_stat_count %>% 
  ggplot(aes(tot_plot_ord, tot_stat, fill = tot_plot_ord)) + 
  geom_bar(stat= "identity") +
  scale_fill_tableau() +
  xlab("Sub Borough") +
  ylab("Total number of stations") +
  theme(legend.position = "none") +
  ggtitle("Subway station stop count per sub borough")

# barbell
subb_stat_count <- subb_stat_count %>% 
  arrange(tot_stat, ada_stat) %>% 
  mutate(tot_stat_order = factor(NAME, levels = NAME))
subb_stat_count %>% 
  ggplot(aes(tot_stat_order, tot_stat)) +
  geom_segment(aes(x = tot_stat_order, xend = tot_stat_order, y = ada_stat, yend = tot_stat), size = 1, alpha= 0.5, color = "gray60") +
  # total number of station stops on route in orange
  geom_point(size = 3, color = "#E69F00") +
  # number of ada-accessible station stops on route in blue
  geom_point(aes(tot_stat_order, ada_stat), size = 3, color = "#56B4E9") +
  xlab("Sub-Borough name") +
  ylab("Number of stations") +
  ggtitle("Total number of stations (orange) vs number of ADA-accessible stations (blue) per Sub-Borough") +
  coord_flip()



# put into categories
subb_map_count_prep <- subb_stat_count %>% 
  # create column to fill subb on map by
  mutate(
    subb_group = case_when(
      # has subway stations, but no ADA stations:
      tot_stat > 0 & ada_stat < 1 ~ "No ADA stations", 
      # has at least 1 ADA station:
      tot_stat > 0 & ada_stat > 0 ~ "At least one ADA station", 
      # else statement: 
      TRUE ~ "No Stations"
    )
  )

## resulting breakdown:
table(subb_map_count_prep$subb_group)

NYCSubBoroughs_cate <- NYCSubBoroughs %>%
  st_join(subb_map_count_prep, by = "NAME")

NYCSubBoroughs_cate <- NYCSubBoroughs_cate %>%
  select(bor_subb, NAME.x, tot_stat, ada_stat, not_ada, ada_percent, subb_group, geometry) 

NYCSubBoroughs_cate <- NYCSubBoroughs_cate %>% 
  rename(NAME = NAME.x) 

NYCSubBoroughs_cate$subb_group[is.na(NYCSubBoroughs_cate$subb_group)] <- "No stations"

NYCSubBoroughs_cate[is.na(NYCSubBoroughs_cate)] <- 0


# Map:
NYCSubBoroughs_cate %>% 
  ggplot() +
  geom_sf(aes(fill = subb_group), color = "#BFBFBF") +
  scale_fill_manual(values = c("#2c7fb8", "#7fcdbb", "#edf8b1"), name = "Sub-Borough Type") + 
  # overlay subway:
  #geom_sf(data = sub_nyc_crs, color = "grey80", fill = "black", size = 2, pch = 21) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  ggtitle("Sub-Borough with no accessible subway station")

# tmap
tmap_mode("view")

tm_shape(NYCSubBoroughs_cate) +
  tm_polygons(col = "subb_group", alpha = 0.8, border.col = '#a6bddb')
  #tm_shape(NYCCDistric) +
  #tm_polygons(col = NA, alpha = 0.5, border.col = '#fec44f') +
  #tm_shape(Subway_Line) +
  #tm_lines(col = "blue")

########Community District
nyc_cd_subw <- st_join(trunk_nyc_crs, NYCCDistric)
head(nyc_cd_subw)

# group by and summarize station-route counts by cd:
cd_stat_count <- nyc_cd_subw %>%
  group_by(BoroCD) %>% 
  summarize(
    # total number of station stops
    tot_stat = n(),
    # ada station stop count
    ada_stat = sum(ada),
    not_ada = tot_stat - ada_stat,
    # percet of stations that are ada:
    ada_percent = ada_stat * 100 / tot_stat
  ) %>% 
  # add in cd with no subway stations
  bind_rows(
    NYCCDistric %>% 
      as_tibble() %>% 
      filter(!(BoroCD %in% nyc_cd_subw$BoroCD)) %>% 
      select(BoroCD) %>% 
      mutate(
        tot_stat = 0,
        ada_stat = 0,
        not_ada = 0, 
        ada_percent = NA
      )
  ) %>% 
  # order by station count, from most to least
  arrange(desc(tot_stat)) %>% 
  mutate(tot_plot_ord = factor(BoroCD, levels = BoroCD))

# BAR PLOT BY cd
cd_stat_count %>% 
  ggplot(aes(tot_plot_ord, tot_stat, fill = tot_plot_ord)) + 
  geom_bar(stat= "identity") +
  scale_fill_tableau() +
  xlab("Community District") +
  ylab("Total number of stations") +
  theme(legend.position = "none") +
  ggtitle("Subway station stop count per Community District")

# put into categories
cd_map_count_prep <- cd_stat_count %>% 
  # create column to fill neighborhoods on map by
  mutate(
    cd_group = case_when(
      # has subway stations, but no ADA stations:
      tot_stat > 0 & ada_stat < 1 ~ "No ADA stations", 
      # has at least 1 ADA station:
      tot_stat > 0 & ada_stat > 0 ~ "At least one ADA station", 
      # else statement: 
      TRUE ~ "No Stations"
    )
  )

## resulting breakdown:
table(cd_map_count_prep$cd_group)

NYCCD_cate <- NYCCDistric %>%
  st_join(cd_map_count_prep, by = "BoroCD")

NYCCD_cate <- NYCCD_cate %>% 
  rename(BoroCD = BoroCD.x) 

summary(NYCCD_cate)

NYCCD_cate <- NYCCD_cate %>% 
  select(BoroCD, Shape_Leng, Shape_Area, tot_stat, ada_stat, not_ada, ada_percent, tot_plot_ord, cd_group, geometry)  

NYCCD_cate$cd_group[is.na(NYCCD_cate$cd_group)] <- "No stations"

NYCCD_cate[is.na(NYCCD_cate)] <- 0

# Map:
NYCCD_cate %>% 
  ggplot() +
  geom_sf(aes(fill = cd_group), color = "#BFBFBF") +
  scale_fill_manual(values = c("#2c7fb8", "#7fcdbb", "#edf8b1"), name = "Community District Type") + 
  # overlay subway:
  #geom_sf(data = sub_nyc_crs, color = "grey80", fill = "black", size = 2, pch = 21) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  ggtitle("Community District with no accessible subway station")

# tmap
tmap_mode("view")

tm_shape(NYCCD_cate) +
  tm_polygons(col = "cd_group", alpha = 0.8, border.col = '#a6bddb') 
  #tm_shape(Subway_Line) +
  #tm_lines(col = "blue")

##### neighborhood
# retrieve the CRS:
st_crs(NYCNeighbor) = 4326

# change the CRS:
NYCNeighbor <- st_transform(NYCNeighbor, crs = st_crs(NYCSubBoroughs))
head(NYCNeighbor)

nyc_nbh_subw <- st_join(trunk_nyc_crs, NYCNeighbor)
head(nyc_nbh_subw)

# group by and summarize station-route counts by cd:
nbh_stat_count <- nyc_nbh_subw %>%
  group_by(ntaname) %>% 
  summarize(
    # total number of station stops
    tot_stat = n(),
    # ada station stop count
    ada_stat = sum(ada),
    not_ada = tot_stat - ada_stat,
    # percet of stations that are ada:
    ada_percent = ada_stat * 100 / tot_stat
  ) %>% 
  # add in cd with no subway stations
  bind_rows(
    NYCNeighbor %>% 
      as_tibble() %>% 
      filter(!(ntaname %in% nyc_nbh_subw$ntaname)) %>% 
      select(ntaname) %>% 
      mutate(
        tot_stat = 0,
        ada_stat = 0,
        not_ada = 0, 
        ada_percent = NA
      )
  ) %>% 
  # order by station count, from most to least
  arrange(desc(tot_stat)) %>% 
  mutate(tot_plot_ord = factor(ntaname, levels = ntaname))

# BAR PLOT BY nbh
nbh_stat_count %>% 
  ggplot(aes(tot_plot_ord, tot_stat, fill = tot_plot_ord)) + 
  geom_bar(stat= "identity") +
  scale_fill_tableau() +
  xlab("Neighborhood") +
  ylab("Total number of stations") +
  theme(legend.position = "none") +
  ggtitle("Subway station stop count per Neighborhood")

# put into categories
nbh_map_count_prep <- nbh_stat_count %>% 
  # create column to fill neighborhoods on map by
  mutate(
    nbh_group = case_when(
      # has subway stations, but no ADA stations:
      tot_stat > 0 & ada_stat < 1 ~ "No ADA stations", 
      # has at least 1 ADA station:
      tot_stat > 0 & ada_stat > 0 ~ "At least one ADA station", 
      # else statement: 
      TRUE ~ "No Stations"
    )
  )

## resulting breakdown:
table(nbh_map_count_prep$nbh_group)

NYCNeighbor_cate <- NYCNeighbor %>%
  st_join(nbh_map_count_prep, by = "ntaname")

NYCNeighbor_cate <- NYCNeighbor_cate %>% 
  rename(ntaname = ntaname.x) 

summary(NYCNeighbor_cate)

NYCNeighbor_cate <- NYCNeighbor_cate %>% 
  select(boro_code, boro_name, ntacode, ntaname, shape_area, shape_leng, tot_stat, ada_stat, not_ada, ada_percent, tot_plot_ord, nbh_group, geometry)  

NYCNeighbor_cate$nbh_group[is.na(NYCNeighbor_cate$nbh_group)] <- "No stations"

NYCNeighbor_cate[is.na(NYCNeighbor_cate)] <- 0

# Map:
NYCNeighbor_cate %>% 
  ggplot() +
  geom_sf(aes(fill = nbh_group), color = "#BFBFBF") +
  scale_fill_manual(values = c("#2c7fb8", "#7fcdbb", "#edf8b1"), name = "Neighborhood Type") + 
  # overlay subway:
  #geom_sf(data = sub_nyc_crs, color = "grey80", fill = "black", size = 2, pch = 21) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  ggtitle("Neighborhood with no accessible subway station")

# tmap
tmap_mode("view")

tm_shape(NYCNeighbor_cate) +
  tm_polygons(col = "nbh_group", alpha = 0.8, border.col = '#a6bddb') 
#tm_shape(Subway_Line) +
#tm_lines(col = "blue")


###### density
#subb total
subb_den_joined <- NYCSubBoroughs_cate %>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per subb
  mutate(density=tot_stat/area)

tm_shape(subb_den_joined) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("NAME", "density"),
              title="Sub-Borough Station Density")

#subb ada
subb_ada_den_joined <- NYCSubBoroughs_cate %>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per subb
  mutate(ada_density=ada_stat/area)

tm_shape(subb_ada_den_joined) +
  tm_polygons("ada_density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("NAME", "ada_density"),
              title="Sub-Borough ADA Station Density")


#cd
cd_den_joined <- NYCCD_cate %>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per cd
  mutate(density=tot_stat/area)

tm_shape(cd_den_joined) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("BoroCD", "density"),
              title="Community District Station Density")

#cd ada
cd_ada_den_joined <- NYCCD_cate %>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per cd
  mutate(ada_density=ada_stat/area)

tm_shape(cd_ada_den_joined) +
  tm_polygons("ada_density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("BoroCD", "ada_density"),
              title="Community District ADA Station Density")

#nbh
nbh_den_joined <- NYCNeighbor_cate %>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per nbh
  mutate(density=tot_stat/area)

tm_shape(nbh_den_joined) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("ntaname", "density"),
              title="Neighborhood Station Density")

#nbh ada
nbh_ada_den_joined <- NYCNeighbor_cate %>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per nbh
  mutate(ada_density=ada_stat/area)

tm_shape(nbh_ada_den_joined) +
  tm_polygons("ada_density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("ntaname", "ada_density"),
              title="Neighborhood ADA Station Density")

###population density
#cleaning data
spec(ni_all)

ni_subb_d <- ni_all %>%
  select(region_id, region_name, region_type, year, pop_num, pop_65p_pct, pop_disabled_pct) 

ni_subb_d <- ni_subb_d %>%
  filter(region_type == "Sub-Borough Area")

#2018
ni_subb_2018 <- ni_subb_d %>%
  filter(year == "2018") 

ni_subb_2018 <- ni_subb_2018 %>%
  select(region_id, pop_num, pop_65p_pct, pop_disabled_pct) 

ni_subb_2018 <- ni_subb_2018 %>% 
  rename(bor_subb = region_id) 

#2010
ni_subb_2010 <- ni_subb_d %>%
  filter(year == "2010") 

ni_subb_2010 <- ni_subb_2010 %>%
  select(region_id, pop_num, pop_65p_pct, pop_disabled_pct) 

ni_subb_2010 <- ni_subb_2010 %>% 
  rename(bor_subb = region_id) 

#write.csv(ni_subb_2010, "ni_subb_2010.csv", row.names = FALSE)

#nyu neighborhood indicator data cleaned (change M, Bronx)
subb_pop <- read_csv(here::here("final_project", "data", "cleaned", "ni_subb_2018.csv"))
subb_pop_2010 <- read_csv(here::here("final_project", "data", "cleaned", "ni_subb_2010.csv"))

#subb pop den 2018
#join population related columns to subb shapefile
NYCSubBoroughs_pop <- NYCSubBoroughs_cate %>%
  left_join(subb_pop, by = "bor_subb")

NYCSubBoroughs_pop <- NYCSubBoroughs_pop %>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the population per subb
  mutate(pop_density=pop_num/area)

tm_shape(NYCSubBoroughs_pop) +
  tm_polygons("pop_density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("bor_subb", "pop_density"),
              title="2018 Population Density")  
  
summary(NYCSubBoroughs_pop$pop_disabled_pct)

#disabled pop 2018
NYCSubBoroughs_pop <- NYCSubBoroughs_pop %>%
  #density of the disabled population 
  mutate(d_density=pop_density*pop_disabled_pct)

tm_shape(NYCSubBoroughs_pop) +
  tm_polygons("d_density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("bor_subb", "d_density"),
              title="2018 Disabled Population Density")  

#subb pop den 2010
#join population related columns to subb shapefile
NYCSubBoroughs_pop_10 <- NYCSubBoroughs_cate %>%
  left_join(subb_pop_2010, by = "bor_subb")

NYCSubBoroughs_pop_10 <- NYCSubBoroughs_pop_10 %>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the population per subb
  mutate(pop_density=pop_num/area)

tm_shape(NYCSubBoroughs_pop_10) +
  tm_polygons("pop_density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("bor_subb", "pop_density"),
              title="2010 Population Density")  

summary(NYCSubBoroughs_pop_10$pop_disabled_pct)

#disabled pop 2010
NYCSubBoroughs_pop_10 <- NYCSubBoroughs_pop_10 %>%
  #density of the disabled population 
  mutate(d_density=pop_density*pop_disabled_pct)

tm_shape(NYCSubBoroughs_pop_10) +
  tm_polygons("d_density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("bor_subb", "d_density"),
              title="2010 Disabled Population Density")  


###subb kernel den analysis
#now set a window as the subb boundary
window <- as.owin(NYCSubBoroughs_cate)
plot(window)

#create a ppp object
sub_nyc_crs_spat<- sub_nyc_crs %>%
  as(., 'Spatial')

sub_nyc_crs.ppp <- ppp(x=sub_nyc_crs_spat@coords[,1],
                          y=sub_nyc_crs_spat@coords[,2],
                          window=window)
sub_nyc_crs.ppp %>%
  plot(.,pch=16,cex=0.5, 
       main="NYC stations")

sub_nyc_crs.ppp %>%
  density(., sigma=1500) %>%
  plot(main="NYC stations")

###ada kernel
ada_nyc_crs <- sub_nyc_crs %>%
  filter(ada == TRUE) 

window <- as.owin(NYCSubBoroughs_cate)
plot(window)

#create a ppp object
ada_nyc_crs_spat<- ada_nyc_crs %>%
  as(., 'Spatial')

ada_nyc_crs.ppp <- ppp(x=ada_nyc_crs_spat@coords[,1],
                       y=ada_nyc_crs_spat@coords[,2],
                       window=window)
ada_nyc_crs.ppp %>%
  plot(.,pch=16,cex=0.5, 
       main="NYC ADA stations")

ada_nyc_crs.ppp %>%
  density(., sigma=1500) %>%
  plot(main="NYC ADA stations")

