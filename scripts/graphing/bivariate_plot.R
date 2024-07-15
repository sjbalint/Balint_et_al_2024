
rm(list = ls()) #clear environment

library(tidyverse)
library(ggsci)

load("Rdata/grainstats.Rdata")

# graphing parameters -----------------------------------------------------

update_geom_defaults("point", list(shape = 21, fill="grey"))

basetheme <- list(
  theme_classic(),
  theme(
    text=element_text(size=12),
    strip.background = element_blank(),
    strip.placement = "outside",
    #strip.text.y.left = element_text(angle = 0,size=12),
    strip.text.x.bottom = element_text(size=12),
    #axis.title.y = element_text(angle = 0,vjust=0.5,size=12),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    legend.position = "right",
    legend.key.height = unit(0.45, "in")
    #legend.title=element_blank()
  ),
  scale_shape_manual(values=c(21:25)),
  scale_fill_viridis_c(option="rocket",
                       guide = guide_colorbar(frame.colour = "black",
                                              ticks.colour = "black"))
)


# graphing ----------------------------------------------------------------

lines.df <- data.frame(x1=c(1,2.5),
                       y1=c(1.5,1),
                       x2=c(2.5,10),
                       y2=c(5,5))

polygon.df <- data.frame(x1=c(1,2.5,10,2.5,1),
                         y1=c(1.5,5,5,1,1))

curves.df <- data.frame(x1=c(1.5),
                        y1=c(5),
                        x2=c(7),
                        y2=c(1))

text.df <- data.frame(x=c(2.5, 7),
                      y=c(2.5, 1.25),
                      text=c(
                        "Fluvial and\nstorm episodes",
                        "Closed\nBasin"))

ggplot()+
  basetheme+
  geom_curve(data=curves.df,aes(x=x1,y=y1,xend=x2,yend=y2), color="black",curvature=0.1)+
  geom_polygon(data=polygon.df,aes(x=x1,y=y1),color=NA,fill="grey90")+
  geom_segment(data=lines.df,aes(x=x1,y=y1,xend=x2,yend=y2), color="black")+
  #geom_ellipse(aes(x0 = 5.5, y0 = 3, a = 0.3, b = 0.8, angle = 45), fill="white")+
  geom_text(data=text.df,aes(x=x,y=y,label=text))+
  geom_point(data=grainstats.df,aes(x=mean.phi, y=abs(sd.phi),fill=year.mean, color=year.mean, shape=location),
             color="black", size=2, alpha=0.8)+
  scale_x_continuous(trans="log2",limits=c(1,10), breaks=c(1:10),expand = c(0, 0))+
  scale_y_continuous(trans="log2",limits=c(1,5), breaks=c(1:10),expand = c(0, 0))+
  labs(x=bquote("Mean"~"("*phi*")"),
       y=bquote("Sorting"~"("*phi*")"),
       fill="Year",shape="Location")

ggsave("figures/Fig.4.jpg", width=6, height=5)


