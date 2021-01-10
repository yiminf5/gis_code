library(spatialreg)
library(spgwr)
library(broom)
library(car)
library(scales)
library(BAMMtools)



##########start here
Regressiondata<- subb_pop_ada_den %>%
  dplyr::select(ada_density, 
                d_density,
                name,
                bor_subb,
                pop_num,
                pop_disabled_pct)

#standardize
Regressiondata$ada_density_scaled <- scale(Regressiondata$ada_density, center = FALSE, scale = TRUE) 

Regressiondata$d_density_scaled <- scale(Regressiondata$d_density, center = FALSE, scale = TRUE)


par(mfrow=c(1,1))

q2 <- qplot(x = as.numeric(d_density_scaled), 
           y = as.numeric(ada_density_scaled), 
           data=Regressiondata)

#plot with a regression line - note, I've added some jitter here as the x-scale is rounded
q2 + stat_smooth(method="lm", se=FALSE, size=1) + 
  xlab("Disabled Population Density")+
  ylab("ADA Station Density")+
  theme_bw()+
  geom_jitter()

summary(Regressiondata$ada_density_scaled)
summary(Regressiondata$d_density_scaled)


#now model
model1 <- Regressiondata %>%
  lm(ada_density_scaled ~ 
       d_density_scaled,
     data=.)

#show the summary of those outputs
summary(model1)

tidy(model1)

#save the residuals into your dataframe

model_data <- model1 %>%
  augment(., Regressiondata)

#ggplot
getJenksBreaks(Regressiondata$ada_density_scaled, 4)
getJenksBreaks(Regressiondata$d_density_scaled, 4)

ggplot(Regressiondata, aes(x=d_density_scaled, y=ada_density_scaled, label = bor_subb)) +
  geom_point(size=2)+ 
  #stat_smooth(method="lm", se=FALSE, size=1)+
  xlab("Disabled Population Density")+
  ylab("ADA Station Density")+
  theme_bw()+  
  #geom_label(size=3, hjust=-0.6, vjust=0)+
  geom_hline(yintercept=4.9553,  color = "red")+
  geom_vline(xintercept=2.4293,  color = "red")+
  geom_hline(yintercept=1.8335, linetype="dashed", color = "red")+
  geom_vline(xintercept=1.2289, linetype="dashed", color = "red")+
  geom_hline(yintercept=0, color = "blue")+
  geom_vline(xintercept=0.1130, color = "blue")+
  geom_hline(yintercept=0.5737, linetype="dashed", color = "blue")+
  geom_vline(xintercept=0.5692, linetype="dashed", color = "blue")+
  geom_jitter()


#show in map
# put into categories
Regressiondata <- Regressiondata %>% 
  mutate(
    supply_group = case_when(
      ada_density_scaled > 4.9553  ~ 5, 
      ada_density_scaled > 1.8335 & ada_density_scaled <= 4.9553 ~ 4, 
      ada_density_scaled > 0.5737  & ada_density_scaled <= 1.8335 ~ 3, 
      ada_density_scaled > 0 & ada_density_scaled <= 0.5737  ~ 2, 
      # else statement: 
      TRUE ~ 1
    )
  )

Regressiondata <- Regressiondata %>% 
  mutate(
    demand_group = case_when(
      d_density_scaled > 2.4293  ~ 5, 
      d_density_scaled > 1.2289 & d_density_scaled <= 2.4293 ~ 4, 
      d_density_scaled > 0.5692  & d_density_scaled <= 1.2289 ~ 3, 
      d_density_scaled > 0.1130   & d_density_scaled <= 0.5692 ~ 2, 
      # else statement: 
      TRUE ~ 1
    )
  )

Regressiondata <- Regressiondata %>% 
  mutate(
    gap_group = case_when(
      demand_group == 5 &  supply_group == 1 ~ "Very High Demand, Very Low Supply", 
      demand_group == 5 &  supply_group == 2 ~ "Very High Demand, Low Supply",
      demand_group == 4 &  supply_group == 1 ~ "High Demand, Very Low Supply",
      demand_group == 4 &  supply_group == 2 ~ "High Demand, Low Supply",
      # else statement: 
      TRUE ~ "Gap not Significant"))

#filter special polygon
poly_gap<- Regressiondata %>%
  dplyr::select(ada_density, 
                d_density,
                name,
                bor_subb,
                gap_group,
                pop_num,
                pop_disabled_pct)

poly_gap<- poly_gap %>%
  filter(gap_group != "Gap not Significant")


Regressiondata %>% 
  ggplot() +
  geom_sf(aes(fill = gap_group), color = "white") +
  scale_fill_manual(values = c("#fff7bc","#fed98e", "#fe9929", "#993404"), name = "Deficient Areas") + 
  # overlay subway:
  #geom_sf(data = sub_nyc_crs, color = "grey80", fill = "black", size = 2, pch = 21) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  theme_bw()+
  geom_sf_label(aes(label = name), data = poly_gap, nudge_x=5,nudge_y=2)

tmap_mode("view")

tm_basemap(leaflet::providers$OpenStreetMap.Mapnik, alpha = 0.5) +
  tm_shape(Regressiondata)+ 
  tm_graticules(col = "grey", alpha=0.3)+
  tm_polygons("gap_group", palette = c("#fef0d9", "#fdcc8a", "#fc8d59", "#d7301f"), alpha = 0.9, title = "Deficient Areas")+

  tm_layout(main.title = "NYC Map", legend.outside = TRUE)
  #tm_compass(north=0)+
  #tm_scale_bar(text.size=0.6)+
  
  #tm_shape(poly_gap)+
  #tm_text("name",  size = 0.7, shadow = TRUE)

#culculate people affected
poly_gap<- poly_gap %>%
  mutate(d_num <- poly_gap$pop_disabled_pct*poly_gap$pop_num)

summary(poly_gap$d_num)
sum(poly_gap$d_num)

#plot residuals
model_data%>%
  dplyr::select(.resid)%>%
  pull()%>%
  qplot()+ 
  geom_histogram() 

#print some model diagnositcs. 
par(mfrow=c(1,1))    #plot to 2 by 2 array
plot(model1)




'''
cor(subb_pop_ada_den$d_density, subb_pop_ada_den$ada_density, method = c("pearson"))
cor(subb_pop_ada_den$d_density, subb_pop_ada_den$ada_density, method = c("kendall"))
cor(subb_pop_ada_den$d_density, subb_pop_ada_den$ada_density, method = c("spearman"))


symbox(~ada_density_scaled, 
       Regressiondata, 
       na.rm=T,
       powers=seq(-3,3,by=.5))

symbox(~d_density_scaled, 
       Regressiondata, 
       na.rm=T,
       powers=seq(-3,3,by=.5))

#ada: y  log issue########
ggplot(Regressiondata, aes(x=log(ada_density_scaled))) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.1) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

#disabled density: x
ggplot(Regressiondata, aes(x=log(d_density_scaled)))+ 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.25) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

#logged
q3 <- qplot(x = log(d_density_scaled), 
            y = log(ada_density_scaled), 
            data=Regressiondata)

#plot with a regression line - added some jitter here as the x-scale is rounded
q3 + stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()


#now model
model2 <- Regressiondata %>%
  lm(as.numeric(log(ada_density_scaled)) ~ 
       as.numeric(log(d_density_scaled)),
     data=.)

#show the summary of those outputs
summary(model2)

tidy(model2)
'''