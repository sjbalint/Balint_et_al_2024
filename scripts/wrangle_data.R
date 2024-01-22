rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(cluster)
library(factoextra) #for optimixing clusters

# import data -------------------------------------------------------------

load("Rdata/silica.Rdata")

load("Rdata/isotopes.Rdata")

load("Rdata/dating.Rdata")

load("Rdata/grainsize.Rdata")

load("Rdata/grainstats.Rdata")

load("Rdata/outliers.Rdata")

P.df <- read.csv("raw/phosphorus.csv")

bulk.df <- read.csv("raw/bulk_density.csv")

# summarize grainsize data ------------------------------------------------

grain.df <- grain.df %>%
  select(location,depth.cm,class.pct,class.rough) %>%
  group_by(location,depth.cm,class.rough) %>%
  summarize_all(sum) %>%
  ungroup() %>%
  pivot_wider(names_from=class.rough,values_from=class.pct)

colnames(grain.df) <- c("location","depth.cm","clay.pct","pebbles.pct","sand.pct","silt.pct")


# combine data ------------------------------------------------------------

data.df <- full_join(isotopes.df,P.df) %>%
  select(-n)

data.df <- full_join(data.df,silica.df)

data.df <- full_join(data.df,bulk.df)

data.df <- full_join(data.df,grain.df)

data.df <- full_join(data.df,grainstats.df)

data.df <- left_join(data.df,dating.df)

data.df <- left_join(data.df,outlier.df)


# final calculations ------------------------------------------------------

data.df["P.pct.total"][data.df["P.pct.total"]<0] <- NA

data.df <- data.df %>%
  mutate(
    N.P.ratio=`%N`/P.pct.total,
    C.N.ratio=`%C.organic`/`%N`,
    Si.P.ratio=SiO2.prct/P.pct.total,
    Si.N.ratio=SiO2.prct/`%N`,
    C.P.ratio=`%C.organic`/`P.pct.total`,
    P.total.pct.e2 = P.pct.total*100
  )

data.df <- data.df %>%
  mutate(location=factor(location,levels=c("North","Middle","South"))) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate_if(is.integer,as.numeric) %>%
  as.data.frame

# N storage ---------------------------------------------------------------

data.df <- data.df %>%
  mutate(N.storage=0.24*bulk.density.gcm3*`%N`,
         median.grainsize.phi=log2(median.grainsize.um),
         accretion.rate.gcm2yr=accretion.rate.cmyr*bulk.density.gcm3)


data.df$century <- factor(round(data.df$year.mean/100)*100) #determine century for stats

# perform clustering ------------------------------------------------------

subset.df <- data.df %>%
  filter(outlier==FALSE) %>%
  select(all_of(c("%N", "d15N.permil", "%C.organic", "d13C.organic","P.pct.total",
                  "SiO2.prct","kurtosis.um","mean.phi","sd.phi",
                  "accretion.rate.gcm2yr"))) %>%
  drop_na()

#define linkage methods
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

#function to compute agglomerative coefficient
ac <- function(x) {
  agnes(subset.df, method = x)$ac
}

#calculate agglomerative coefficient for each clustering linkage method
sapply(m, ac) #do ward

#perform hierarchical clustering using Ward's minimum variance
clust <- agnes(subset.df, method = "ward")

#produce dendrogram
pltree(clust, cex = 0.6, hang = -1, main = "Dendrogram") 

#calculate gap statistic for each number of clusters (up to 10 clusters)
gap_stat <- clusGap(subset.df, FUN = hcut, K.max = 10)

#produce plot of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

dist <- dist(scale(subset.df))
clust <- hclust(dist,method="ward.D2")
plot(clust)
smalltree <- cutree(clust,k=4)

subset.df$cluster <- as.factor(smalltree)

data.df <- left_join(data.df, subset.df)

# export data -------------------------------------------------------------

save(data.df,file="Rdata/compiled_data.Rdata")

write.csv(data.df,"output/compiled_data_BALINT.csv",row.names=FALSE)
