rm(list=ls())

# load packages -----------------------------------------------------------

library(vegan)
library(tidyverse)
library(grid)
library(ggsci)
library(cluster)
library(factoextra) #for optimixing clusters
library(dendextend) #for plotting dendrogran

update_geom_defaults("point", list(shape = 21, fill="grey"))

# import data -------------------------------------------------------------

load("Rdata/compiled_data.Rdata")

# format plots ------------------------------------------------------------

update_geom_defaults("point", list(shape = 21, fill="grey"))

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

row.names(data) <- paste(data$location, round(data$year.mean))

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


dist <- dist(scale(scaled))
clust <- hclust(dist,method="ward.D2")
smalltree <- cutree(clust,k=4)

# graph dendrogram --------------------------------------------------------

dendro <- clust %>%
  as.dendrogram() %>%
  set("branches_k_color", value = as.character(c(1, 3, 4, 2)), k=4) %>%
  set("labels_cex", c(.5)) %>%
  #set("highlight_branches_lwd") %>%
  as.ggdend()

outline <- dendro$segments

ggplot(dendro, horiz = TRUE)+
  geom_segment(data=outline, aes(x=x, y=y, xend=xend, yend=yend), color="black", linewidth=1.2, lineend = "square")+
  geom_segment(data=outline, aes(x=x, y=y, xend=xend, yend=yend, color=col), lineend = "square")+
  scale_color_manual(values=c(scales::viridis_pal(option="cividis", dir=-1)(4), "black"))+
  theme(axis.line.x.bottom = element_line(),
        axis.ticks.x.bottom = element_line())+
  geom_hline(yintercept=10, linetype="dashed")

ggsave("figures/FigS1.png")

# graph PCA ---------------------------------------------------------------

plot.df <- data.frame(pca$scores, year.mean=data$year.mean, location=data$location)
plot.df$cluster <- as.factor(smalltree)

ggplot(plot.df, aes(Comp.1, Comp.2, fill=cluster, color=cluster, shape=location))+
  mytheme+
  geom_point(color="black", size=3)+
  labs(x="PC1 (51%)",y="PC2\n(17%)", fill="Cluster", shape="Location")

ggsave("figures/FigS2.png")

ggplot(plot.df, aes(year.mean, Comp.1, fill=cluster, color=cluster, shape=location))+
  mytheme+
  geom_line(color="black", linewidth=1)+
  geom_line(alpha=0.8)+
  geom_point(color="black", size=2)+
  labs(x="Year",y="PC1\n(51%)", fill="Cluster", color="Cluster", shape="Location")

ggsave("figures/FigS3.png")

