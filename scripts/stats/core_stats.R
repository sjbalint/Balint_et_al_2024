rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(progress)
library(knitr)
library(car)
library(dunn.test)
library(pgirmess) #for kruskalmc
library(heplots) #for mvn qq plots

# import data -------------------------------------------------------------

load("Rdata/compiled_data.Rdata")

data.df <- data.df %>%
  #mutate(outlier=as.factor(outlier))
  drop_na(cluster)

force_nonparametric <- TRUE

# configure stats ---------------------------------------------------------

alpha=0.05

predictor.list <- c("location", "cluster")
response.list <- c("mean.phi","sd.phi","accretion.rate.gcm2yr","%C.organic","%N","P.pct.total",
                   "SiO2.prct","C.N.ratio","N.P.ratio","C.P.ratio","d15N.permil","d13C.organic")


# perform summary statistics ----------------------------------------------

calculate_mean <- function(data.df,predictor){
  
  summary.df <- data.df %>%
    select(c(predictor,response.list)) %>%
    group_by_at(predictor) %>%
    mutate(n=n()) %>%
    summarize_all(mean,na.rm=TRUE) %>%
    #mutate_if(is.numeric, round, 1) %>%
    ungroup() %>%
    drop_na()
  
  return(summary.df)
  
}

calculate_n <- function(data.df, predictor, response, space=FALSE){
  
  n1 <- data.df %>%
    select(c("depth.cm",predictor, response)) %>%
    drop_na() %>%
    group_by_at(predictor) %>%
    count() %>%
    rename("group"=predictor)
  
  n2 <- n1
  
  if (space){
    n <- cross_join(n1, n2) %>%
      mutate(comparisons=paste(group.x,"-", group.y),
             n=n.x+n.y) %>%
      select(comparisons, n) %>%
      as.data.frame()
  } else {
    n <- cross_join(n1, n2) %>%
      mutate(comparisons=paste0(group.x,"-", group.y),
             n=n.x+n.y) %>%
      select(comparisons, n) %>%
      as.data.frame()
  }
  
  return(n)
  
}

summary_century.df <- calculate_mean(data.df,"century") %>%
  arrange(desc(century))

summary_location.df <- calculate_mean(data.df,"location")

summary_cluster.df <- calculate_mean(data.df,"cluster")

mean.df <- bind_rows(summary_century.df,summary_location.df)

mean.df <- bind_rows(mean.df,summary_cluster.df)

calculate_count <- function(data.df,predictor){
  
  count.df <- data.df %>%
    select(c(response.list,predictor)) %>%
    pivot_longer(response.list) %>%
    drop_na() %>%
    group_by_at(c("name",predictor)) %>%
    count() %>%
    pivot_wider(names_from="name",values_from="n")
  
  return(count.df)
}

count_century.df <- calculate_count(data.df,"century")

count_location.df <- calculate_count(data.df,"location")

count_cluster.df <- calculate_count(data.df,"cluster")

count.df <- bind_rows(count_century.df,count_location.df)

count.df <- bind_rows(count.df,count_cluster.df)

pdf("figures/QQplot.pdf", width=7, height=7)

#cqplot(data.df[response.list]) #doesn't look very normal
cqplot(data.df[response.list], detrend = TRUE,
       main=NA, pch=21, fill.color="grey", env.col=NA, ref.col="black") #ouch

dev.off() 

# test for normality ------------------------------------------------------

shapiro.df <- data.frame(matrix(ncol=0,nrow=length(response.list)))

for (row in 1:nrow(shapiro.df)){
  response <- response.list[row]
  shapiro.df[row,"response"] <- response
  shapiro.df[row,"shapiro.p.value"] <- shapiro.test(data.df[,response])$p.value
  shapiro.df[row,"shapiro.log.p.value"] <- shapiro.test(log(abs(data.df[,response])))$p.value
  if (shapiro.df[row,"shapiro.p.value"]>alpha){
    shapiro.df[row,"normal"] <- TRUE
  } else {
    shapiro.df[row,"normal"] <- FALSE
  }
  if (shapiro.df[row,"shapiro.log.p.value"]>alpha){
    shapiro.df[row,"lognormal"] <- TRUE
  } else {
    shapiro.df[row,"lognormal"] <- FALSE
  }
}

normal.list <- shapiro.df %>%
  filter(normal==TRUE) %>%
  select(response) %>%
  pull

abnormal.list <- shapiro.df %>%
  filter(normal==FALSE) %>%
  select(response) %>%
  pull

kable(shapiro.df)


# normal data -------------------------------------------------------------

if (length(normal.list)>0){
  
  new.response.list <- normal.list
  
  anova.df <- data.frame(matrix(ncol=0,nrow=length(response.list)*length(predictor.list)))
  
  row <- 0
  
  for (response in new.response.list){
    for (predictor in predictor.list){
      row <- row+1
      anova.df[row,"response"] <- response
      anova.df[row,"predictor"] <- predictor
      anova.df[row,"anova.p.value"] <- summary(aov(data.df[,response] ~ data.df[,predictor]))[[1]][1, 5]
      if (anova.df[row,"anova.p.value"]<alpha){
        anova.df[row,"anova.significance"] <- TRUE
      } else {
        anova.df[row,"anova.significance"] <- FALSE 
      }
    }
  }
  kable(anova.df) 
}


# post-hoc testing --------------------------------------------------------

if (length(normal.list)>0){
  tukey.df <- anova.df %>%
    filter(anova.significance==TRUE) %>%
    select(c("response","predictor"))
  
  if (nrow(tukey.df)>0){
    tukey.results.df <- tukey.df
    
    n.df <- calculate_n(data.df, predictor, response)
    
    for (row in 1:nrow(tukey.df)){
      response <- tukey.df[row,"response"]
      predictor <- tukey.df[row,"predictor"]
      thsd <- TukeyHSD(aov(data.df[,response] ~ data.df[,predictor]))
      temp.df <- data.frame(dimnames(thsd$`data.df[, predictor]`)[1], thsd$`data.df[, predictor]`[,4])
      colnames(temp.df) <- c("comparisons","tukey.p.value")
      temp.df$response <- tukey.df[row,"response"]
      temp.df$predictor <- tukey.df[row,"predictor"]
      temp.df <- left_join(temp.df, n.df)
      tukey.results.df <- full_join(tukey.results.df,temp.df)
    }
    
    tukey.df <- tukey.results.df %>% 
      drop_na(tukey.p.value)
    
    for (row in 1:nrow(tukey.df)){
      if (tukey.df[row,"tukey.p.value"]<alpha/2){
        tukey.df[row,"tukey.significance"] <- TRUE
      } else {
        tukey.df[row,"tukey.significance"] <- FALSE
      }
    }
    
    tukey.results.df <- tukey.df %>%
      filter(tukey.significance==TRUE)
    
    kable(tukey.results.df) 
  }
}

# abnormal data -----------------------------------------------------------

if (length(abnormal.list)>0){
  
  if (force_nonparametric){
    new.response.list <- response.list
  } else {
    new.response.list <- abnormal.list 
  }
  
  kruskal.df <- data.frame(matrix(ncol=0,nrow=length(response.list)*length(predictor.list)))
  
  row <- 0
  
  for (response in new.response.list){
    for (predictor in predictor.list){
      row <- row+1
      n <- data.df %>%
        select(c("depth.cm",response)) %>%
        drop_na() %>%
        count() %>%
        as.numeric()
      kruskal.df[row,"response"] <- response
      kruskal.df[row,"predictor"] <- predictor
      kruskal.df[row,"n"] <- n
      kruskal.df[row,"levene.p.value"] <- leveneTest(data.df[,response], data.df[,predictor])$`Pr(>F)`[1]
      
      if (kruskal.df[row,"levene.p.value"]>alpha){
        kruskal.df[row,"equal.variance"] <- TRUE
      } else {
        kruskal.df[row,"equal.variance"] <- FALSE
      }
      
      kruskal.df[row,"kruskal.p.value"] <- kruskal.test(data.df[,response],data.df[,predictor])$p.value
      if (kruskal.df[row,"kruskal.p.value"]<alpha){
        kruskal.df[row,"kruskal.significance"] <- TRUE
      } else {
        kruskal.df[row,"kruskal.significance"] <- FALSE 
      }
      if (kruskal.df[row,"equal.variance"]==FALSE){
        #kruskal.df[row,"kruskal.p.value"] <- NA 
        #kruskal.df[row,"kruskal.significance"] <- NA
      }
    }
  }
  
  
  kable(kruskal.df)
}


# post-hoc testing --------------------------------------------------------

if (length(abnormal.list)>0){
  dunn.df <- kruskal.df %>%
    filter(kruskal.significance==TRUE) %>%
    select(c("response","predictor"))
  
  dunn.results.df <- dunn.df
  
  n.df <- calculate_n(data.df, predictor, response, space=TRUE)
  
  for (row in 1:nrow(dunn.df)){
    response <- dunn.df[row,"response"]
    predictor <- dunn.df[row,"predictor"]
    #dt <- kruskalmc(data.df[,response],data.df[,predictor], alpha=alpha)$dif.com
    dt <- dunn.test(data.df[,response],data.df[,predictor], method="bonferroni", table=FALSE, altp=TRUE)
    
    #comparisons <- rownames(dt)
    
    #temp.df <- data.frame(comparisons, dt$obs.dif, dt$critical.dif, dt$stat.signif)
    temp.df <- data.frame(dt$comparisons, dt$altP.adjusted)
    colnames(temp.df) <- c("comparisons","dunn.p.value")
    #colnames(temp.df) <- c("comparison","dunn.F.stat", "dunn.F.crit", "dunn.significance")
    temp.df$response <- dunn.df[row,"response"]
    temp.df$predictor <- dunn.df[row,"predictor"]
    temp.df <- left_join(temp.df, n.df)
    dunn.results.df <- full_join(dunn.results.df,temp.df)
  }
  
  dunn.results.df <- dunn.results.df %>%
    mutate(dunn.signif=ifelse(dunn.p.value<alpha, TRUE, FALSE))
  
  kable(dunn.results.df)
}


# regressions -------------------------------------------------------------

regression.df <- data.frame(matrix(ncol=0,nrow=length(response.list)))

for (row in 1:nrow(regression.df)){
  response <- response.list[row]
  regression.df[row,"response"] <- response
  lm <- lm(get(response)~depth.cm,data=data.df)
  n <- data.df %>%
    select(c("depth.cm",response)) %>%
    drop_na() %>%
    count() %>%
    as.numeric()
  regression.df[row,"regression.n"] <- n
  regression.df[row,"regression.r.squared"] <- summary(lm)$r.squared
  regression.df[row,"regression.p.value"] <- summary(lm)$coefficients[2,4]
  regression.df[row,"regression.slope"] <- summary(lm)$coefficients[2,1]
  if (regression.df[row,"regression.p.value"]<alpha){
    regression.df[row,"regression.significance"] <- TRUE
  } else {
    regression.df[row,"regression.significance"] <- FALSE
  }
}

kable(regression.df)


# export results ----------------------------------------------------------

write.csv(mean.df,"output/mean.csv",row.names=FALSE)

write.csv(count.df,"output/count.csv",row.names=FALSE)

write.csv(anova.df,"output/anova.csv",row.names=FALSE)

write.csv(tukey.results.df,"output/tukey.csv",row.names=FALSE)

write.csv(kruskal.df,"output/kruskal.csv",row.names=FALSE)

write.csv(dunn.results.df,"output/dunn.csv",row.names=FALSE)

write.csv(regression.df,"output/regression.csv")
