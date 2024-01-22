
rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(progress)
library(RColorBrewer)
library(ggsci)

# import data -------------------------------------------------------------

data.df <- read_excel("raw/dating/raw_dating.xlsx") %>%
  as.data.frame() %>%
  mutate(location=factor(location,levels=c("North","Middle","South")))

load("Rdata/dating.Rdata")

# graphing parameters -----------------------------------------------------

mywidth=6
myheight=10

legend_title <- NULL

#import base graphing theme
load("Rdata/basetheme.Rdata")

#import ylabels.df for the plot_longer function
load("Rdata/graphing_labels.Rdata")

plot_longer <- function(data.df,long_cols){
  plot.df <- data.df %>%
    pivot_longer(long_cols)
  
  plot.df <- left_join(plot.df,ylabels.df)
  
  factor_names <- plot.df %>%
    pull(factor) %>%
    unique()
  
  plot.df$factor <- factor(plot.df$factor,levels=factor_names,ordered=TRUE)
  
  return (plot.df)
}

# dating ---------------------------------------------------------------

temp.df <- plot_longer(data.df,c("137Cs_activity.bqkg","210Pb_excess.bqkg")) %>%
  select(c("location","depth.cm","factor","value"))

error.df <- plot_longer(data.df,c("137Cs_uncertainty.bqkg","210Pb_uncertainty.bqkg")) %>%
  select(c("location","depth.cm","depth.min","depth.max","factor","value")) %>%
  rename("uncertainty"="value")

temp.df <- full_join(temp.df,error.df) %>%
  mutate(max=value+uncertainty,
         min=value-uncertainty)

ggplot(temp.df)+
  basetheme+
  geom_area(aes(y=depth.cm,x=value, fill=location),orientation="y", alpha=0.3)+
  geom_line(aes(y=depth.cm,x=value),orientation="y")+
  geom_errorbarh(aes(x=value,y=depth.cm,
                     xmin=min, xmax=max),alpha=0.8)+
  geom_point(aes(y=depth.cm, x=value, fill=location, shape=location),
           size=2.5,color="black",alpha=0.7)+
  facet_grid(location~factor,scales="free_x",labeller = label_parsed)+
  labs(x=NULL,y="Depth\ncm",shape=legend_title,color=legend_title,fill=legend_title,linetype=legend_title)+
  scale_color_jco()+
  scale_fill_jco()+
  scale_shape_manual(values=c(21:24))+
  theme(legend.position="none")+
  scale_y_reverse()+
  geom_vline(xintercept=0)

ggsave("figures/dating.png",width=mywidth, height=myheight)


# dating ---------------------------------------------------------------

mywidth=10
myheight=6

dating.df <- dating.df %>%
  filter(depth.cm>=0) %>%
  mutate(location=factor(location,levels=c("North","Middle","South")))

ggplot(dating.df)+
  basetheme+
  geom_line(aes(y=depth.cm,x=year.mean),orientation="y")+
  geom_ribbon(aes(x=year.mean,y=depth.cm,
                     xmin=year.min, xmax=year.max, fill=location),alpha=0.5)+
  facet_wrap(~location, strip.position="bottom")+
  labs(x="Year",y="Depth\n(cm)",shape=legend_title,color=legend_title,fill=legend_title,linetype=legend_title)+
  scale_color_jco()+
  scale_fill_jco()+
  scale_shape_manual(values=c(21:24))+
  theme(legend.position="none")+
  scale_y_reverse()

ggsave("figures/chronology.png",width=mywidth, height=myheight)
