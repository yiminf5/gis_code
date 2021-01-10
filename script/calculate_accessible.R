###accessible population
subb_accesible <- subb_ada_den_joined %>%
  left_join(subb_pop, by = "bor_subb")

summary(subb_accesible)

subb_accesible <- subb_accesible %>%
  select(bor_subb, name, tot_stat, ada_stat, pop_num)

subb_accesible <- subb_accesible %>%
  mutate(station_group = case_when(
    tot_stat > 0 ~ 1, TRUE ~ 0))

subb_accesible <- subb_accesible %>%
  mutate(ada_group = case_when(
    ada_stat > 0 ~ 1, TRUE ~ 0))


#percentage of people having access to subway, subb
sum((subb_accesible$pop_num)*(subb_accesible$station_group))/sum(subb_accesible$pop_num)    
#0.9853911

#percentage of people having access to ADA subway, subb
sum((subb_accesible$pop_num)*(subb_accesible$ada_group))/sum(subb_accesible$pop_num)    
#0.8226429

#cd
nta_pop <- st_read(here::here("final_project", "data", "raw", "New_York_City_Population_By_Neighborhood_Tabulation_Areas.csv"))
summary(nta_pop)

nta_pop <- nta_pop %>%
  janitor::clean_names()

colnames(nta_pop) <- colnames(nta_pop) %>% 
  str_replace_all("_", "")

nta_pop <- nta_pop %>%
  filter(year == "2010")

nbh_accesible <- nbh_ada_den_joined %>%
  left_join(nta_pop, by = "ntacode")

summary(nbh_accesible)

nbh_accesible <- nbh_accesible %>%
  select(ntacode, ntaname.x, tot_stat, ada_stat, population)

nbh_accesible$population <- as.numeric(nbh_accesible$population)

nbh_accesible <- nbh_accesible %>%
  mutate(station_group = case_when(
    tot_stat > 0 ~ 1, TRUE ~ 0))

nbh_accesible <- nbh_accesible %>%
  mutate(ada_group = case_when(
    ada_stat > 0 ~ 1, TRUE ~ 0))

#percentage of people having access to subway, subb
sum(nbh_accesible$population)
sum((nbh_accesible$population)*(nbh_accesible$station_group))/sum(nbh_accesible$population)    
#0.7930456

#percentage of people having access to ADA subway, subb
sum((nbh_accesible$population)*(nbh_accesible$ada_group))/sum(nbh_accesible$population)    
#0.4227055