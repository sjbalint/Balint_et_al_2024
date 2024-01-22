
rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(ggsci)
library(cowplot)
library(ggsignif)
library(ggpubr) #to add stats to ggplot
library(ggtukey) #for cld
library(rstatix) #for dunn's
#library(devtols)
#devtools::install_github("https://github.com/ethanbass/ggtukey/")

# import data -------------------------------------------------------------

load("Rdata/compiled_data.Rdata")

data.df <- data.df %>%
  #filter(cluster!=4) %>%
  drop_na(cluster)
  #mutate(cluster=factor(cluster, levels=c(1:4), labels=c("A","B","C","D")))

date_lines.df <- data.df %>%
  select(location,year.mean,end.pb,cs.peak) %>%
  filter(!is.na(end.pb) | !is.na(cs.peak))

for (row in 1:nrow(date_lines.df)){
  if (!is.na(date_lines.df[row,"end.pb"])){
    date_lines.df[row,"type"] <- "Pb-210 Minimum"
  }
  if (!is.na(date_lines.df[row,"cs.peak"])){
    date_lines.df[row,"type"] <- "Cs-237 Maximum"
  }
}

# graphing parameters -----------------------------------------------------

mywidth=10
myheight=8

legend_title <- NULL

#import base graphing theme
load("Rdata/basetheme.Rdata")

#import ylabels.df for the plot_longer function
load("Rdata/graphing_labels.Rdata")

plot_longer <- function(data.df,long_cols){
  plot.df <- data.df %>%
    select(all_of(c("year.mean", "cluster", "location", long_cols))) %>%
    pivot_longer(long_cols)
  
  plot.df <- left_join(plot.df,ylabels.df)
  
  factor_names <- plot.df %>%
    pull(factor1) %>%
    unique()
  
  plot.df <- plot.df %>%
    mutate(factor =factor(factor1,levels=factor_names),
           name=factor(name))
  
  return (plot.df)
}

make_plot <- function(long.df){
  
  update_geom_defaults("point", list(shape = 21, fill="grey"))
  
  combos <- combn(x = unique(as.character(long.df$cluster)), m = 2, simplify = FALSE)
  
  p1 <- ggplot(long.df)+
    basetheme+
    geom_hline(data=date_lines.df,aes(yintercept=year.mean,group=location, linetype=type))+
    geom_smooth(aes(y=year.mean, x=value),se=FALSE, color="black", orientation="y", show.legend = FALSE)+
    geom_point(aes(y=year.mean, x=value, fill=cluster, shape=location), alpha=0.7, size=2.5,color="black")+
    facet_wrap(~factor,nrow=1,scales="free_x",strip.position = "top",labeller = label_parsed)+
    labs(x=NULL,y="Year",shape=legend_title,color=legend_title,fill=legend_title,linetype=legend_title)+
    scale_y_continuous(breaks=c(2000,1900,1800,1700),
                       labels=c("2000","1900","1800","Before\n1800"))+
    theme(legend.position="top")
  
  p2 <- ggplot(data=long.df, aes(x=cluster, y=value, fill=cluster))+
    geom_boxplot(outlier.shape=21, alpha=0.7, na.rm=TRUE, show.legend=FALSE)+
    basetheme+
    scale_x_continuous(position = "bottom")+
    labs(y=NULL, x="Cluster")+
    theme(strip.text = element_blank())+
    scale_x_discrete(limits=rev)+
    facet_wrap(~factor,nrow=1,scales="free_x",strip.position = "bottom")+
    geom_tukey(test="kruskalmc",type="one-way",where="whisker", threshold=0.05, vjust = 0.4, hjust=-0.2)+
    coord_flip()
  
  p3 <- plot_grid(p1, p2, ncol=1, align="v", axis="tb", rel_heights = c(1,0.2))
  
  return(p3)
  
}

print_stats <- function(long.df, hide.ns=TRUE){
  
  stat.test <- long.df %>%
    group_by(name) %>%
    dunn_test(value~cluster,p.adjust.method="bonferroni") %>%
    mutate(comparison=paste0(group1,"-",group2),
           p.adj=p.adj) %>%
    mutate(ntotal=n1+n2) %>%
    select(name, comparison, n1, n2, ntotal, p.adj)
  
  if (hide.ns){
    stat.test <- stat.test %>%
      filter(p.adj<0.05)
  }
  
  print(stat.test, n=nrow(stat.test))
}

# HCA results -------------------------------------------------------------

plot.df <- data.df %>%
  select(location, year.mean, cluster) %>%
  group_by(location, cluster) %>%
  mutate(year.max=max(year.mean),
         year.min=min(year.mean)) %>%
  ungroup() %>%
  select(-year.mean) %>%
  unique()

write.csv(plot.df, "output/HCA_clusters.csv", row.names=FALSE)

ggplot(plot.df, aes(x=location, xend=location, y=year.min, yend=year.max, color=cluster))+
  basetheme+
  scale_x_discrete(position = "top")+
  geom_segment(linewidth=2)+
  geom_segment(linewidth=15, show.legend=FALSE)+
  #scale_color_viridis_d(option="cividis", direction = -1, aesthetics = c("colour", "fill"))+
  labs(x=NULL, y="Year", color="Cluster")

ggsave("figures/HCA.png",width=6, height=4)

# grainsize ---------------------------------------------------------------


temp.df <- plot_longer(data.df,c("sand.pct","mean.phi","accretion.rate.gcm2yr","sd.phi"))

make_plot(temp.df)

print_stats(temp.df)

ggsave("figures/grainsize.png",width=mywidth, height=myheight)

# elemental composition again ---------------------------------------------

temp.df <- plot_longer(data.df,c("%C.organic","%N","P.total.pct.e2"))

make_plot(temp.df)

print_stats(temp.df)


ggsave("figures/elements.png",width=mywidth, height=myheight)

# elemental ratios --------------------------------------------------------

temp.df <- plot_longer(data.df,c("d15N.permil","SiO2.prct","d13C.organic"))

line_factors <- temp.df$factor %>%
  unique()

make_plot(temp.df)

print_stats(temp.df)

ggsave("figures/isotopes.png",width=mywidth, height=myheight)

# elemental ratios --------------------------------------------------------

temp.df <- plot_longer(data.df,c("SiO2.prct","Si.P.ratio","Si.N.ratio"))

line_factors <- temp.df$factor %>%
  unique()

make_plot(temp.df)

print_stats(temp.df)

ggsave("figures/Si_ratios.png",width=mywidth, height=myheight)


# additional ratios -------------------------------------------------------

temp.df <- plot_longer(data.df,c("C.N.ratio","C.P.ratio", "N.P.ratio"))

line_factors <- temp.df$factor %>%
  unique()

lines.df <- data.frame(factor=line_factors,x=c(6.625,106,16))

make_plot(temp.df)+
  geom_vline(data=lines.df,aes(xintercept=x),linetype="dashed")

print_stats(temp.df)

ggsave("figures/element_ratios.png",width=mywidth, height=myheight)
