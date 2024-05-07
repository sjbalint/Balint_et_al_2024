
rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(basemaps) #for sat imagry
library(marmap) #for noaa bathy
library(tigris) #for census data
library(metR) #for contour functions. better than ggplot
library(ggspatial) #for north arrow, distance scale, etc.
library(ggnewscale) #for fill scales
library(cowplot)
library(sf)

sf_use_s2(FALSE)


# import data ------------------------------------------------------------

#location boundaries for new england
xmin=-72.5
xmax=-70.5
ymin=41
ymax=42.5

ne_ext <- st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin), crs = st_crs(4326))

USA.sf <- states() %>%
  filter(STUSPS %in% c('RI', "MA","CT")) %>%
  erase_water(area_threshold = 0.995) %>%
  st_transform(crs=4326) %>%
  st_crop(ne_ext)

#the location boundaries
xmin=-71.47
xmax=-71.41
ymin=41.54
ymax=41.60

base_ext <- st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin), crs = st_crs(4326))

load("Rdata/Nbay_GIS.Rdata")

base.sf <- bay.sf %>%
  st_transform(crs=4326) %>%
  st_crop(base_ext) %>%
  st_transform(crs=3857)

base_sat <- basemap_gglayer(base_ext, map_service = "esri", map_type = "world_imagery")

#the location boundaries
xmin=-71.458
xmax=-71.45
ymin=41.567
ymax=41.573

ac_ext <- st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin), crs = st_crs(4326))

ac.sf <- USA.sf %>%
  st_transform(crs=4326) %>%
  st_crop(ac_ext) %>%
  st_transform(crs=3857)

ac_sat <- basemap_gglayer(ac_ext, map_service = "esri", map_type = "world_imagery")

cores.sf <- data.frame("name"=c("North","Middle","South"),
                       "latitude"=c(41.570800, 41.570284, 41.569286),
                       "longitude"=c(-71.455144, -71.454574, -71.455166)) %>%
  st_as_sf(coords=c("longitude","latitude"),crs=4326) %>%
  st_transform(crs=3857) %>%
  mutate(name=factor(name, levels=c("North","Middle","South")))


# base map ----------------------------------------------------------------

p1 <- ggplot() + 
  theme_bw()+
  labs(y=NULL, x=NULL)+
  base_sat+
  scale_fill_identity() + 
  geom_sf(data=base.sf, color=NA, fill="black", alpha=0.6)+
  theme(panel.border = element_rect(color="black", linewidth=2))+
  annotation_north_arrow(location = "tl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering(
                           text_col="white", line_col="white",
                           fill = c("black", "black")))+
  annotation_scale(location = "bl", width_hint = 0.3, text_col="white")+
  geom_rect(aes(xmin=st_bbox(ac.sf)[1],
                ymin=st_bbox(ac.sf)[2],
                xmax=st_bbox(ac.sf)[3],
                ymax=st_bbox(ac.sf)[4]),
            fill=NA, color="white", linewidth=1)+
  coord_sf(expand=0)

# map of new england ------------------------------------------------------

p2 <- ggplot(USA.sf) +
  theme_void()+
  theme(panel.border = element_rect(color="white", linewidth=2),
        panel.background = element_rect(fill="darkgrey"))+
  geom_sf(fill="lightgrey", color="black")+
  geom_point(aes(x=mean(xmin,xmax), y=mean(ymin,ymax)), shape=4, color = "darkred",size=2, stroke=3)+
  coord_sf(expand=0)


# make academy cove map ---------------------------------------------------

p3 <- ggplot() + 
  theme_void()+
  ac_sat+
  scale_fill_identity() +
  geom_sf(data=cores.sf, aes(shape=name), color="black", fill="darkred",
          size=3, show.legend=FALSE)+
  geom_sf_label(data=cores.sf, aes(label=name), alpha=0.7, size=3,
                hjust=0, vjust=0, nudge_x=20, nudge_y=20)+
  scale_shape_manual(values=c(21:26))+
  coord_sf(expand=0)+
  theme(panel.border = element_rect(color="white", linewidth=2))+
  annotation_scale(location = "br", width_hint = 0.8, text_col="white")

# combine the plots -------------------------------------------------------

final <- ggdraw() +
  draw_plot(p1) +
  draw_plot(p2,
            scale = 0.3,
            x = 0.68,
            y = 0.8,
            hjust = 0.5,
            vjust = 0.5)+
  draw_plot(p3,
            scale = 0.41,
            x = 0.65,
            y = 0.26,
            hjust = 0.5,
            vjust = 0.5)

ggsave("figures/Fig1.pdf")

