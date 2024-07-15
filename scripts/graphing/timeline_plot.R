rm(list = ls()) #clear environment


# install packages --------------------------------------------------------

library(scales)
library(lubridate)
library(tidyverse)
library(knitr)
library(timevis)
library(readxl)
library(ggsci)
library(cowplot)
library(ggrepel) #for managing the text


# load data ---------------------------------------------------------------

#import population data
load("Rdata/population.Rdata")

#import historical timeline data
timeline.df <- read_excel("raw/timeline_data.xlsx") %>%
  arrange(date)

#set type as a factor for graphing
timeline.df$type <- factor(timeline.df$type, 
                           #levels=c("Physical Modification","Ecology","Nutrient Cycling"),
                           ordered=TRUE)


# set random direction ----------------------------------------------------

random_direction<-TRUE

if (random_direction){
  
  # Set the heights we will use for our milestones.
  positions <- rep(c(1:5),times=10)
  
  
  # Set the directions we will use for our milestone (above and below)
  directions <- c(1, -1) 
  
  
  # Assign the positions & directions to each date from those set above.
  line_pos.df <- data.frame(
    "date"=unique(timeline.df$date),
    "position"=rep(positions, length.out=length(unique(timeline.df$date))),
    "direction"=rep(directions, length.out=length(unique(timeline.df$date))))
  
  #set position to positive and negative based on direction
  line_pos.df$position <- line_pos.df$position * line_pos.df$direction
  
  timeline.df$position <- NULL
  
  # Create columns with the specified positions and directions for each milestone event
  timeline.df <- full_join(timeline.df, line_pos.df, by="date") 
}


# use defined direction ---------------------------------------------------

mindate <- 1700
maxdate <- 2030
breaks <- 50

wholenumber <- function(x,n){
  
  result.list <- list()
  
  for (i in 1:length(x)){
    
    y <- x[i]/n
    
    if (y-round(y)==0){
      
      result <- TRUE
      
    } else {
      
      result <- FALSE
      
    }
    result.list <- append(result.list, result)
  }
  
  return(result.list)
}

#define dates of x axis
date_range.df <- data.frame(date_range=c(mindate:maxdate)) %>%
  mutate(date_label=ifelse(wholenumber(date_range, breaks), date_range, NA),
         date_breaks=ifelse(wholenumber(date_range, 10), date_range, NA))


# plot --------------------------------------------------------------------

# offset the labels 0.2 away from scatter points
text_offset <- 0.3 

# Let's use the absolute value since we want to add the text_offset and increase space away from the scatter points 
timeline.df$text_position <- (text_offset*ceiling((nchar(timeline.df$description)/20)) + abs(timeline.df$position)) * sign(timeline.df$position)

#new variable to match population x axis
timeline.df$Year <- timeline.df$date

plot.df <- population.df %>%
  drop_na() %>%
  filter(Region!="Washington County") %>%
  arrange(Region, Year)

#write.csv(plot.df, "raw/census.csv")

#plot of population
p1 <- ggplot(plot.df,aes(Year,Population/1000,fill=Region))+
  geom_col(color="black")+
  labs(y=bquote("Population"~"("*10^3*")"))+
  theme_classic()+
  #remove x axis
  theme(axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank(),
        legend.position=c(.2,.8),
        panel.grid.major.y = element_line())+
  scale_x_continuous(limits=c(mindate, maxdate))+
  scale_fill_grey(start=0.9,end=0.3)+
  labs(fill=NULL)

#timeline
p2 <- ggplot(timeline.df,aes(x=Year,y=position))+
  geom_hline(yintercept=0, color = "black", linewidth=0.7)+
  geom_segment(data=date_range.df, aes(y=-0.2, yend=0.2, x=date_label, xend=date_label))+
  geom_segment(data=date_range.df, aes(y=-0.1, yend=0.1, x=date_breaks, xend=date_breaks))+
  geom_segment(aes(yend=0,xend=date), color='black', size=0.2)+
  geom_point(aes(shape=type,fill=type),size=3)+
  geom_label(data=date_range.df, aes(x=date_range,y=-0.5, label=date_label),size=3.5, color='black', angle=0)+
  geom_text(aes(y=text_position, color=type, label = str_wrap(description, 20)), size=3, direction="y", show.legend=FALSE)+
  theme_classic()+
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank(),
        legend.position = c(.4,.2),)+
  scale_shape_manual(values=21:25)+
  labs(fill=NULL, shape=NULL)+
  scale_color_uchicago(palette="dark")+
  scale_fill_uchicago(palette="dark")
 
plot <- plot_grid(p1,p2,ncol=1, align = "v", axis="tb", rel_heights=c(1,2))

ggdraw(plot)

ggsave(plot=plot,"figures/Fig2.pdf",width=10, height=8)
