###autocorrelation

points_sf_joined <- NYCSubBoroughs_pop%>%
  st_join(ada_nyc_crs)%>%
  add_count(NAME)%>%
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per ward
  mutate(density=n/area)%>%
  #select density and some other variables 
  dplyr::select(density, name, bor_subb, n, tot_stat, ada_stat, pop_num, pop_disabled_pct, area)

points_sf_joined<- points_sf_joined %>%                    
  group_by(name) %>%         
  summarise(density = first(density),
            name= first(name),
            count= first(n))

tm_shape(points_sf_joined) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("name", "density"),
              title="ADA Density")

#First calculate the centroids of all Wards in London

coordsW <- points_sf_joined%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

#create a neighbours list

ada_nb <- points_sf_joined %>%
  poly2nb(., queen=T)

#plot them
plot(ada_nb, st_geometry(coordsW), col="red")
#add a map underneath
plot(points_sf_joined$geometry, add=T)





