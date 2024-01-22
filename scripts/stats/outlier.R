rm(list = ls()) #clear environment

# import packages ---------------------------------------------------------

library(tidyverse)
library(readxl)
library(progress)
library(knitr)
library(car)
library(dunn.test)

# import data -------------------------------------------------------------

load("Rdata/compiled_data.Rdata")

#data.df <- data.df %>%
  #mutate(outlier=as.factor(outlier))
  #filter(outlier==FALSE)


# configure outlier detection ---------------------------------------------

response.list <- c("%C.organic","%N")
predictor <- "depth.cm"

long.df <- data.df %>%
  select(c("location","depth.cm",all_of(response.list))) %>%
  pivot_longer(response.list)

tukey.df <- data.frame(matrix(ncol=0,nrow=length(response.list)))
myrow <- 0

for (response in response.list){
  temp.df <- long.df %>%
    filter(name==response)
  Q <- quantile(temp.df$value, probs=c(.25, .75), na.rm = TRUE)
  iqr <- IQR(temp.df$value,na.rm = TRUE)
  upper <- Q[2]+3*iqr
  lower <- Q[1]-3*iqr
  
  myrow <- myrow+1
  tukey.df[myrow,"name"] <- response
  tukey.df[myrow,"upper"] <- upper
  tukey.df[myrow,"lower"] <- lower
}

long.df <- full_join(long.df,tukey.df) %>%
  mutate(outlier=ifelse(value>upper,1,0),
         outlier=ifelse(value<lower,1,outlier))

temp.df <- long.df %>%
  select(location,depth.cm,outlier) %>%
  group_by(location,depth.cm) %>%
  summarise(outlier=max(outlier)) %>%
  ungroup() %>%
  mutate(outlier=ifelse(outlier>0,TRUE,FALSE))

outlier.df <- temp.df

save(outlier.df,file="Rdata/outliers.Rdata")
