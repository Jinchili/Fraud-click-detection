library(data.table)
library(dplyr)
library(xgboost)
library(ggplot2)
library(caret)
pacman::p_load(knitr, tidyverse, highcharter, data.table, lubridate, pROC, tictoc, DescTools, lightgbm)
pacman::p_load(data.table,ggplot2,DT,magrittr,corrplot,Rmisc,ggalluvial,caret,ModelMetrics,scales,irlba,forcats,forecast,TSA,zoo)

sm_data_1=read.csv("D:/Thesis/5_6_2017/simulation with commitment plan/sim_result_matrix_1.csv",row.names=str(c(1:1000)))
set.seed(123456)
train <- fread("D:/DataMining/competition/mnt/ssd/kaggle-talkingdata2/competition_files/train.csv", select =c("ip", "app", "device", "os", "channel", "click_time", "is_attributed"),showProgress=T,colClasses=c("ip"="numeric","app"="numeric","device"="numeric","os"="numeric","channel"="numeric","click_time"="character","is_attributed"="numeric"))
train_sub=sample_n(tbl =train ,size = 6e+06,replace = F)
write.csv(x = train_sub,file = 'train_sub.csv')
train_p=train[which(train$is_attributed == 1)]
list_attributed=train$is_attributed
train_n_1=train[which(train[1:(10^7),]$is_attributed == 0)]
train_n_2=train[which(train[(6*10^7):(12*10^7),]$is_attributed == 0)]
train_n_3=train[which(train[(12*10^7):184903890,]$is_attributed == 0)]
write.csv(x = train_n_1,file = 'train_n_1.csv')
write.csv(x = train_n_2,file = 'train_n_2.csv')
train_day_2 <- fread("D:/DataMining/competition/mnt/ssd/kaggle-talkingdata2/competition_files/train.csv",col.names = c("ip", "app", "device", "os", "channel", "click_time","attribute_time", "is_attributed"),showProgress=T,skip=9308569,nrows=68942879-9308569)
#names(train_day_2)
train_day_3 <- fread("D:/DataMining/competition/mnt/ssd/kaggle-talkingdata2/competition_files/train.csv",col.names = c("ip", "app", "device", "os", "channel", "click_time","attribute_time", "is_attributed"),showProgress=T,skip=68941879 ,nrows=131886954-68941879)

write.csv(x = train_n_1,file = 'train_n_1.csv')



X <- copy(train_day_3[,c("click_time","is_attributed")])[, `:=`(hour = hour(click_time),
                          is_attributed = factor(is_attributed))]

data_rate_0=X[X$is_attributed==0,]
data_rate_0_n=data_rate_0[, .N, by = c("hour")]
data_rate_1=X[X$is_attributed==1,]
data_rate_1_n=data_rate_1[, .N, by = c("hour")]
data_rate_f=NULL

#data_rate=X[, .N, by = c("hour", "mday", "is_attributed")][, dt := as.POSIXct(paste0("2017-11-", mday, " ", hour), format="%Y-%m-%d %H")]

data_rate_main=NULL
data_rate_main$hour=0:23
data_rate_main$click=data_rate_0_n[,N]
data_rate_main$Download=data_rate_1_n[,N]
data_rate_main$ratio=data_rate_main$Download/data_rate_main$click
  
plot(data_rate_main$hour,data_rate_main$ratio)
write.csv(x=data_rate_main,file="day3.csv")


###
set.seed(123456)
train_sub=sample_n(tbl =train_day_3,size = 6*10^6,replace = F)
write.csv(x = train_sub,file = 'train_sub.csv')
