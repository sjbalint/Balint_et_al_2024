rm(list=ls())

# load packages -----------------------------------------------------------

library(tidyverse)
library(cluster)
library(factoextra) #for optimixing clusters
library(dendextend) #for graphing dendrogram

update_geom_defaults("point", list(shape = 21, fill="grey"))

# import data -------------------------------------------------------------

load("Rdata/compiled_data.Rdata")

pred.list <- c("%N","d15N.permil","%C.organic","d13C.organic", "P.pct.total","SiO2.prct",
               "kurtosis.um","mean.phi", "sd.phi", "accretion.rate.gcm2yr")

data <- data.df %>%
  filter(outlier==FALSE) %>%
  select(all_of(c("year.mean", "location", pred.list))) %>%
  drop_na()

row.names(data) <- paste(data$location, round(data$year.mean))

# perform clustering ------------------------------------------------------

scaled <- data %>%
  select(pred.list)

#define linkage methods
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

#function to compute agglomerative coefficient
ac <- function(x) {
  agnes(scaled, method = x)$ac
}

#calculate agglomerative coefficient for each clustering linkage method
sapply(m, ac) #do ward

#perform hierarchical clustering using Ward's minimum variance
clust <- agnes(scaled, method = "ward")

#produce dendrogram
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 

#calculate gap statistic for each number of clusters (up to 10 clusters)
gap_stat <- clusGap(scaled, FUN = hcut, K.max = 10)

#produce plot of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

dist <- dist(scale(scaled))
clust <- hclust(dist,method="ward.D2")
smalltree <- cutree(clust,k=4)

data$cluster <- as.factor(smalltree)

data.df <- left_join(data.df, data)

# graph dendrogram --------------------------------------------------------

dendro <- clust %>%
  as.dendrogram() %>%
  set("branches_k_color", value = as.character(c(1, 3, 4, 2)), k=4) %>%
  set("labels_cex", c(.5)) %>%
  #set("highlight_branches_lwd") %>%
  as.ggdend()

ggplot(dendro, horiz = TRUE)+
  scale_color_manual(values=c(scales::viridis_pal(option="cividis", dir=-1)(4), "black"))+
  theme(axis.line.x.bottom = element_line(),
        axis.ticks.x.bottom = element_line())+
  geom_hline(yintercept=10, linetype="dashed")

ggsave("figures/PCA/dendrogram.png")
