###autocorrelation

NYCSubB <- st_read(here::here("final_project", "data", "raw", "subb", "subb_test2.shp"))
NYCSubB <- NYCSubB %>% 
  filter(bor_subb < 500)

NYCSubB <- st_transform(NYCSubB, crs = "NAD83")
head(ada_nyc_crs)
qtm(NYCSubB)

ada_nyc_crs_2 <- st_transform(ada_nyc_crs, crs = "NAD83")

points_sf_joined_subb <- NYCSubB%>%
  st_join(ada_nyc_crs_2)%>%
  add_count(bor_subb)%>%
  #janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per ward
  mutate(density=n/area)%>%
  #select density and some other variables 
  dplyr::select(density, bor_subb, n, area)

points_sf_joined_subb<- points_sf_joined_subb %>%                    
  group_by(bor_subb) %>%         
  summarise(density = first(density),
            bor_subb= first(bor_subb),
            count= first(n))

tm_shape(points_sf_joined_subb) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("bor_subb", "density"),
              title="ADA Density")

#First calculate the centroids of all Wards in London

coordsW <- points_sf_joined_subb%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

#create a neighbours list

ada_nb <- points_sf_joined_subb %>%
  poly2nb(., queen=T)

#or nearest neighbours
knn_subb <-coordsW %>%
  knearneigh(., k=4)

subb_knn <- knn_subb %>%
  knn2nb()

#plot them
plot(ada_nb, st_geometry(coordsW), col="red")

plot(subb_knn, st_geometry(coordsW), col="blue")

#add a map underneath
plot(points_sf_joined_subb)

#create a spatial weights matrix object from these weights for regression residual

subb.queens_weight <- ada_nb %>%
  nb2listw(., style="C")

subb.knn_4_weight <- subb_knn %>%
  nb2listw(., style="C")

points_sf_joined_subb <- points_sf_joined_subb %>%
  mutate(model1resids = residuals(model1))

Queen <- points_sf_joined_subb %>%
  st_drop_geometry()%>%
  dplyr::select(model1resids)%>%
  pull()%>%
  moran.test(., subb.queens_weight)%>%
  tidy()

Nearest_neighbour <- points_sf_joined_subb %>%
  st_drop_geometry()%>%
  dplyr::select(model1resids)%>%
  pull()%>%
  moran.test(., subb.knn_4_weight)%>%
  tidy()

Queen

Nearest_neighbour






'''
we can see that the Moran’s I statistic is somewhere between 0.05 and 0.06. 
Moran’s I ranges from between -1 and +1 (0 indicating no spatial autocorrelation) 
we can conclude that there is some weak to moderate spatial autocorrelation in our residuals.
'''


#create a spatial weights object from these weights
ada.lw <- ada_nb %>%
  nb2listw(., style="C")

head(ada.lw$neighbours)

#  Moran’s I
I_ada_Global_Density <- points_sf_joined_subb %>%
  pull(density) %>%
  as.vector()%>%
  moran.test(., ada.lw)

I_ada_Global_Density

#  Geary’s C 
C_ada_Global_Density <- 
  points_sf_joined_subb %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., ada.lw)

C_ada_Global_Density

#add a map underneath
plot(points_sf_joined_subb)

#use the localmoran function to generate I for each ward in the city

I_ada_Global_Count <- points_sf_joined_subb %>%
  pull(count) %>%
  as.vector()%>%
  localmoran(., ada.lw)%>%
  as_tibble()

I_ada_Global_Density <- points_sf_joined_subb %>%
  pull(density) %>%
  as.vector()%>%
  localmoran(., ada.lw)%>%
  as_tibble()

#what does the output (the localMoran object) look like?
slice_head(I_ada_Global_Density, n=5)

points_sf_joined_subb <- points_sf_joined_subb %>%
  mutate(count_I = as.numeric(I_ada_Global_Count$Ii))%>%
  mutate(count_Iz =as.numeric(I_ada_Global_Count$Z.Ii))%>%
  mutate(density_I =as.numeric(I_ada_Global_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_ada_Global_Density$Z.Ii))

breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)

MoranColours<- rev(brewer.pal(8, "RdGy"))

tmap_mode("plot")

tm_shape(points_sf_joined_subb) +
  tm_graticules(col = "grey")+
  tm_polygons("count_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, ADA Stations in NYC")+
  #tm_compass(position = c("right", "bottom"))+
  #tm_scale_bar(position = c("right", "bottom"))+
  tm_layout(legend.outside = TRUE)


Gi_subb_Local_Density <- points_sf_joined_subb %>%
  pull(density) %>%
  as.vector()%>%
  localG(., ada.lw)

head(Gi_subb_Local_Density)

points_sf_joined_subb <- points_sf_joined_subb %>%
  mutate(density_G = as.numeric(Gi_subb_Local_Density))



GIColours<- rev(brewer.pal(8, "RdBu"))

#now plot on an interactive map
tm_shape(points_sf_joined_subb) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, ADA Stations in London")+
  tm_compass(position = c("right", "bottom"))+
  tm_scale_bar(position = c("right", "bottom"))+
  tm_layout(legend.outside = TRUE)




'''
