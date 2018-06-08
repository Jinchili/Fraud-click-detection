library(dplyr)
library(xgboost)
library(caret)
library(data.table)

sm_data=read.csv("train_sample.csv")
summary(sm_data)
summary(sm_data[sm_data$is_attributed==1,])
pacman::p_load(knitr, tidyverse, highcharter, data.table, lubridate, pROC, tictoc, DescTools, lightgbm)
head(sm_data[sm_data$is_attributed==1,],n=10)
summary(sm_data)

#checking number of rows
n_r=nrow(sm_data)
#n_r_os=aggregate(sm_data$device,by=list(c(0,1,2)),FUN=nrow)
grp_os <- group_by(sm_data, is_attributed)
summarise(grp_os, nr=n())


%n_r_os=

#count for group
sm_data %>%
  group_by(device) %>%
  summarise(n = n())


count(sm_data[sm_data$is_attributed==1,])


#Datatator2
trainData<-fread('train_sample.csv',drop=c(1,6,7))

#trainData<-fread('../input/train.csv',drop = c(1,6,7),nrows=80000000)

# Balanceamos clases
trainDataDwnSmpl<-downSample(trainData[,-5],as.factor(trainData$is_attributed))
# Como mas abajo hacemos rbind de trainDataDwnSmpl y z vamos a almacenar
# donde terminan los datos de trainig
endOfTrainData<-dim(trainDataDwnSmpl)[1]

# Descartamos las columnas "ip" (2) y "click time" (7)
testData<-fread('test_supplement.csv',drop = c(2,7))
# Todas las columnas de testData menos la primera ("click_id")
z<-testData[,-1]

# allData es para que no haya discrepancias en la variables binarias
# de train y test cuando hacemos one hot encode
allData<-rbind(trainDataDwnSmpl,z,fill =T)

# one hot encode app
apps<-as.factor(allData$app)
apps_dummy<-Matrix::sparse.model.matrix(~0+apps)

# one hot encode devices
devices<-as.factor(allData$device)
devices_dummy<-Matrix::sparse.model.matrix(~0+devices)
count(devices_dummy==2)
# one hot encode oss
oss<-as.factor(allData$os)
oss_dummy<-Matrix::sparse.model.matrix(~0+oss)

# one hot encode channels
channels<-as.factor(allData$channel)
channels_dummy<-Matrix::sparse.model.matrix(~0+channels)