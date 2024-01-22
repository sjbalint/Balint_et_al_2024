rm(list=ls())

# load packages -----------------------------------------------------------

library(vegan)
library(tidyverse)
library(grid)
library(ggsci)
library(cluster)
library(factoextra) #for optimixing clusters

update_geom_defaults("point", list(shape = 21, fill="grey"))

# import data -------------------------------------------------------------

load("Rdata/compiled_data.Rdata")

# format plots ------------------------------------------------------------

theme_set(theme_classic())

mywidth=8
myheight=4.5

mytheme <- list(
  theme(
    text=element_text(size=12),
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0,size=12),
    strip.text.x.bottom = element_text(size=12),
    axis.title.y = element_text(angle = 0,vjust=0.5,size=12),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    legend.position = "right"),
  scale_fill_viridis_d(option="cividis", direction = -1, aesthetics = c("fill","color")),
  scale_shape_manual(values=c(21:25))
)

# perform PCA ------------------------------------------------------------

pred.list <- c("%N","d15N.permil","%C.organic","d13C.organic", "P.pct.total","SiO2.prct",
               "kurtosis.um","mean.phi", "sd.phi", "accretion.rate.gcm2yr")
resp.list <- c("year.mean")
cat.list <- c("location", "century")

data <- data.df %>%
  filter(outlier==FALSE) %>%
  select(all_of(c(pred.list,resp.list,cat.list))) %>%
  drop_na(all_of(c(pred.list,resp.list))) %>%
  mutate(ID=row_number())

pca <- princomp(data[c(pred.list)], cor=TRUE)
plot(pca)

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
plot(clust)
smalltree <- cutree(clust,k=4)

# graph and save results --------------------------------------------------

plot.df <- data.frame(pca$scores, year.mean=data$year.mean, location=data$location)
plot.df$cluster <- as.factor(smalltree)

ggplot(plot.df, aes(Comp.1, Comp.2, fill=location, color=location, shape=location))+
  mytheme+
  geom_point(color="black", size=3)

ggsave("figures/pca/pca2.png")

ggplot(plot.df, aes(Comp.1, Comp.2, fill=year.mean, shape=location))+
  mytheme+
  geom_point(color="black", size=3)+
  scale_fill_viridis_c()+
  scale_color_viridis_c()

ggsave("figures/pca/pca3.png")

ggplot(plot.df, aes(year.mean, Comp.1, fill=location, color=location, shape=location))+
  mytheme+
  geom_line()+
  geom_point(color="black")

ggsave("figures/pca/pca4.png")

ggplot(plot.df, aes(Comp.1, Comp.2, fill=cluster, color=cluster, shape=location))+
  mytheme+
  geom_point(color="black", size=3)+
  labs(x="PC1 (51%)",y="PC2\n(17%)", fill="Cluster", shape="Location")

ggsave("figures/pca/pca5.png")

ggplot(plot.df, aes(year.mean, Comp.1, fill=cluster, color=cluster, shape=location))+
  mytheme+
  geom_line()+
  geom_point(color="black")+
  labs(x="Year",y="PC1\n(51%)", fill="Cluster", color="Cluster", shape="Location")

ggsave("figures/pca/pca6.png")

