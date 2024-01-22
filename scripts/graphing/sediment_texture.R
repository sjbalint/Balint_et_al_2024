
rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(progress)
library(RColorBrewer)
library(ggsci)
library(ggtern) #for trangle graphics
library(ggnewscale) #for separate fill scales by layer



# import data -------------------------------------------------------------

load("Rdata/grainsize.Rdata")

load("Rdata/dating.Rdata")

texture.df <- left_join(grain.df,dating.df) %>%
  select(location,depth.cm,class.pct,class.rough,year.mean) %>%
  group_by(location,depth.cm,class.rough,year.mean) %>%
  summarize(percent=sum(class.pct)) %>%
  pivot_wider(names_from="class.rough",values_from="percent") %>%
  ungroup() %>%
  mutate(location=factor(location))

data(USDA) #from ggtern

USDA <- USDA

USDA_text <- USDA  %>% group_by(Label) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

# make graph --------------------------------------------------------------

ggplot(data = USDA, aes(y = Clay,x = Sand, z = Silt))+
  coord_tern(L = "x", T = "y", R = "z")+
  geom_polygon(aes(group=Label),alpha = 0, size = 0.5, color = "black", show.legend=FALSE)+
  geom_point(data = texture.df,aes(x = Sand, y = Clay, shape=location, fill=year.mean))+
  geom_text(data = USDA_text, aes(label = Label), color = 'black',size = 2)+
  theme_showarrows()+
  theme_clockwise()+
  theme(text = element_text(family = "Helvetica"))+
  #guides(fill=FALSE, color=FALSE)+
  scale_fill_viridis_c()+
  scale_shape_manual(values=c(21:25))

