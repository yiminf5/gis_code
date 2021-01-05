library(spdep)

#check the distribution of these variables first
#ada: y
ggplot(NYCSubBoroughs_pop, aes(x=ada_stat)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 5) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

#disabled density: x
ggplot(NYCSubBoroughs_pop, aes(x=as.numeric(d_density*100000))) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 5) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

#scatterplot
q <- qplot(x = as.numeric(d_density*100000), 
           y = ada_stat, 
           data=NYCSubBoroughs_pop)

#plot with a regression line - note, I've added some jitter here as the x-scale is rounded
q + stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()


cor(NYCSubBoroughs_pop$d_density, NYCSubBoroughs_pop$ada_stat, method = c("pearson"))
cor(NYCSubBoroughs_pop$d_density, NYCSubBoroughs_pop$ada_stat, method = c("kendall"))
cor(NYCSubBoroughs_pop$d_density, NYCSubBoroughs_pop$ada_stat, method = c("spearman"))


