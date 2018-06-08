library(caret)
library(RWeka) # For J48 
library(e1071) # For NB
library(Boruta)
library(DMwR)
library(data.table)
library(dplyr)
library(xgboost)
library(Matrix)
library(zoo)

##read data and make subset
set.seed(123456)
train_day_3 <- fread("D:/DataMining/competition/mnt/ssd/kaggle-talkingdata2/competition_files/train.csv",col.names = c("ip", "app", "device", "os", "channel", "click_time","attribute_time", "is_attributed"),showProgress=T,skip=68941879 ,nrows=131886954-68941879)
train_sub=sample_n(tbl =train_day_3,size = 6*10^6,replace = F)
write.csv(x = train_sub,file = 'train_sub.csv')
train_sub=fread("train_sub.csv",select =c("ip", "app", "device", "os", "channel", "click_time", "is_attributed"),showProgress=T,colClasses=c("ip"="factor","app"="factor","device"="factor","os"="factor","channel"="factor","click_time"="character","is_attributed"="numeric"))
set.seed(123456)
##
inTraining_id <- createDataPartition(train_sub$is_attributed, p = .677,list = FALSE,times =1) #sampleing without replacement

test=fread("test.csv",select =c("ip", "app", "device", "os", "channel", "click_time"),showProgress=T,colClasses=c("ip"="factor","app"="factor","device"="factor","os"="factor","channel"="factor","click_time"="character"))

train_sub$hour=hour(train_sub$click_time)
train_sub_n=train_sub[,-6]
train_sub_lab=train_sub$is_attributed

##separate them
trainingData <- train_sub_n[inTraining_id,]
trainingLab <- train_sub_lab[inTraining_id]
#trainingLab <- as.integer(train_sub_lab[inTraining_id])-1
testData<-train_sub_n[-inTraining_id,]
testData=test
testData$hour=hour(testData$click_time)

testLab <- train_sub_lab[-inTraining_id]

##============================================================================================================##
##I origninally read labels as factors
## I just leave those unfunctional codes for reference purpose
##============================================================================================================##
#testLab <- as.integer(train_sub_lab[-inTraining_id])-1

#train_m=sparse.model.matrix(is_attributed ~ .-1,data=trainingData[])
#test_m=sparse.model.matrix(is_attributed~ .-1,data=testData[])

#train_matrix=xgb.DMatrix(data=train_m,label=trainingLab)
#test_matrix=xgb.DMatrix(data=test_m,label=testLab)
#watchlist<-list(train=train_matrix, test=test_matrix)

##

##Parameter

# bstSparse <- xgboost(data = train_m, label = trainingLab, max.depth = 2, eta = 1, nthread = 2, nround = 100, objective = "binary:logistic")
# 
# #confusionMatrix(predict(bstSparse,test_m),testLab)
# 
# confusionMatrix(as.numeric(predict(bstSparse,test_m)>0.5),testLab)
# 
# ##down sample
# #train_down=downSample(y=trainingData$is_attributed,x=trainingData)
# #train_down_Lab=train_down$is_attributed
# 
# ##multi:softprob
# nc=2
# xgb_params<-list("objective"="multi:softprob",
#                  "eval_metric"="mlogloss",
#                  "num_class"=nc
# )
# bst_model <- xgb.train(params = xgb_params,
#                        data = train_matrix,
#                        nrounds = 100,
#                        eta = 0.001,
#                        max.depth = 3,
#                        watchlist=watchlist,
#                        gamma = 0,
#                        subsample = 1,
#                        colsample_bytree = 1,
#                        missing = NA,
#                        seed = 123456)
# ##binary:logistic
# summary(testLab)
# xgb_b_l_params<-list("objective"="binary:logistic","eval_metric"="rmse","scale_pos_weight"=400,"max_depth"=11,"max_delta_step"=9)
# b_l_model<-xgb.train(params = xgb_b_l_params,
#                      data = train_matrix,
#                      nrounds = 200,
#                      eta = (1:5)*0.1,
#                      watchlist=watchlist,
#                      gamma = c(1:4),
#                      subsample = 1,
#                      colsample_bytree = 1,
#                      missing = NA,
#                      seed = 123456)
# 
# confusionMatrix(as.numeric(predict(b_l_model,test_m)>0.0057),testLab)
# ##select the best threshold value
# 
# Pred=predict(b_l_model,train_m)
# bestAccuracy<-matrix(nrow=1,ncol=2)
# bestAccuracy[1,1]<-0
# bestAccuracy[1,2]<-0
# 
# for(i in 1:1000){
#   newPred<-NULL
#   newPred$pred<-ifelse(Pred > (i*.0001), 1, 0)
#   #newPred$pred<-as.factor(newPred$pred)
#   
#   confM<-confusionMatrix(newPred$pred,trainingLab)
#   
#   if((confM$table[2,2]/(confM$table[1,2]+confM$table[2,2]))>.85){
#     if(confM$overall["Accuracy"]>bestAccuracy[1,2]){
#       bestAccuracy[1,1]<-i*.0001
#       bestAccuracy[1,2]<-confM$overall["Accuracy"]
#     }
#   }
#   
# }
# ##use AUC curve
# xgb_b_auc_params<-list("objective"="binary:logistic","eval_metric"="auc" ,"max_depth"=7)
#  b_l_model_auc<-xgb.train(params = xgb_b_auc_params,
#                         data = train_matrix,
#                         nrounds = 200,
#                         min_child_weight=c(1:20),
#                         eta = (1:5)*0.06,
#                         watchlist=watchlist,
#                        gamma = c(1:4),
#                        subsample = 0.8,
#                       colsample_bytree = 0.8,
#                       missing = NA,
#                     seed = 123456,print_every_n = 50, early_stopping_rounds = 150)
#  
# 
# 
#  confusionMatrix(as.numeric(predict(b_l_model_auc,test_m)>0.472),testLab)
##seems that need some regulization for preventing overfitting


 ##=======================================================================================##
 ##factor dataset approach
##=======================================================================================##

apps_dummy<-Matrix::sparse.model.matrix(~0+trainingData$app)
 devices_dummy<-Matrix::sparse.model.matrix(~0+trainingData$device)
 oss_dummy<-Matrix::sparse.model.matrix(~0+trainingData$os)
 channels_dummy<-Matrix::sparse.model.matrix(~0+trainingData$channel)
 
 ip_dummy<-Matrix::sparse.model.matrix(~0+trainingData$nip_h)
 app_dummy<-Matrix::sparse.model.matrix(~0+trainingData$napp_h)
 ip_app_dummy<-Matrix::sparse.model.matrix(~0+trainingData$nip_h_app)
 
 hour<-Matrix::sparse.model.matrix(~0+trainingData$hour)
 allData_dummified =cbind(apps_dummy,devices_dummy,oss_dummy,channels_dummy,ip_dummy,app_dummy,ip_app_dummy,hour)
 
 testData<-train_sub_n[-inTraining_id,]
 testLab <- train_sub_lab[-inTraining_id]
 
 t_apps_dummy<-Matrix::sparse.model.matrix(~0+testData$app)
 t_devices_dummy<-Matrix::sparse.model.matrix(~0+testData$device)
 t_oss_dummy<-Matrix::sparse.model.matrix(~0+testData$os)
 t_channels_dummy<-Matrix::sparse.model.matrix(~0+testData$channel)
 
 t_ip_dummy<-Matrix::sparse.model.matrix(~0+testData$nip_h)
 t_app_dummy<-Matrix::sparse.model.matrix(~0+testData$napp_h)
 t_ip_app_dummy<-Matrix::sparse.model.matrix(~0+testData$nip_h_app)
 
 t_hour<-Matrix::sparse.model.matrix(~0+testData$hour)
 t_allData_dummified =cbind(t_apps_dummy,t_devices_dummy,t_oss_dummy,t_channels_dummy,t_ip_dummy,t_app_dummy,t_ip_app_dummy,t_hour)
 
 trainData_s=xgb.DMatrix(data=allData_dummified,label=trainingLab)
 #testData_s=xgb.DMatrix(data=t_allData_dummified,label=as.numeric(testLab)-1)
 testData_s=xgb.DMatrix(data=t_allData_dummified,label=testLab)
 
 watchlist_f<-list(train=trainData_s, test=testData_s)
 #model <- xgboost(trainData_s, nrounds = 2000, eta = 0.07, gamma = 4,lambda = 5,scale_pos_weight = 100,objective = "binary:logistic",eval_metric="auc")
 
 start_time5=Sys.time()
 p5 <- list(objective = "binary:logistic",
            booster = "gbtree",
            eval_metric = "auc",
            nthread = 4,
            eta = 0.03,
            max_depth = 4,
            subsample = 0.7,
            min_child_weight = 20,
            gamma = 4,
            lambda = 5,
            max_delta_step = 4,
            scale_pos_weight = 100,
            nrounds = 1500)
 
 m5_xgb <- xgb.train(p5, trainData_s, p5$nrounds,watchlist=watchlist_f, print_every_n = 20, early_stopping_rounds = 150)
 
 end_time5=Sys.time()
 r_time_5=end_time5-start_time5
 
 ##it seems that the overfitting is a crucial problem with our model training 
 ##=======================================================================================##
 ##sparse with larger regulizer and nother parameter to prevent overfitting
 ##=======================================================================================##
 
 xgb_b_auc_params_r<-list("objective"="binary:logistic","eval_metric"="auc" ,"max_depth"=11)
 b_l_model_auc_r<-xgb.train(params = xgb_b_auc_params_r,
                            data = trainData_s,
                            nrounds = 180,
                            min_child_weight=40,
                            eta = 0.06,
                            watchlist=watchlist_f,
                            gamma = 10,
                            lambda = 10,
                            subsample = 0.6,
                            colsample_bytree = 0.8,
                            scale_pos_weight = 100,
                            seed = 123456)
 
 
 
 #calculation for accuracy
 confusionMatrix(as.numeric(predict(b_l_model_auc_r,t_allData_dummified)>0.5),testLab)
 confusionMatrix(as.numeric(predict(b_l_model_auc_r,allData_dummified)>0.5),trainingLab)

 importance_matrix_f <- xgb.importance(colnames(allData_dummified),model = b_l_model_auc_r)
 print(importance_matrix_f[c(1:20),])
 xgb.plot.importance(importance_matrix = importance_matrix)
  ##It helps a little bit. 
 
 ##=======================================================================================##
  ##Try numeric data matrix (without one hot encoding)
 ##=======================================================================================##
 
 train_sub_num=fread("train_sub.csv",select =c("ip", "app", "device", "os", "channel", "click_time", "is_attributed"),showProgress=T)
 
 trainingLab_num=train_sub_num[inTraining_id,7]
 testLab_num=train_sub_num[-inTraining_id,7]
 #train_sub_num$hour=hour(train_sub_num$click_time)
 
# train_sub_num[, hour := hour(click_time)
#       ][, click_time := NULL
#         ][, ip_f := .N, by = "ip"
#           ][, app_f := .N, by = "app"
#             ][, channel_f := .N, by = "channel"
#               ][, device_f := .N, by = "device"
#                 ][, os_f := .N, by = "os"
#                   ][, app_f := .N, by = "app"
#                     ][, ip_app_f := .N, by = "ip,app"
#                       ][, ip_dev_f := .N, by = "ip,device"
#                         ][, ip_os_f := .N, by = "ip,os"
#                           ][, ip_chan_f := .N, by = "ip,channel"
#                             ][, c("ip", "is_attributed") := NULL]
 
 trainingData_num=train_sub[inTraining_id,-c(6,7)]
 trainingData_num[]=lapply(trainingData_num,as.numeric)
 testData_num=train_sub[-inTraining_id,-c(6,7)]
 testData_num[]=lapply(testData_num,as.numeric) 
 
 
 
 dtrain <- xgb.DMatrix(data = data.matrix(trainingData_num),label=data.matrix(trainingLab_num))
 dtest <- xgb.DMatrix(data = data.matrix(testData_num), label = data.matrix(testLab_num))
 watchlist<-list(train=dtrain, test=dtest)
 ##============================================================================================================##
 ##parameter1     (we use part of the parameters from kaggle kernel, but delete those parts we feel incorrect)
 ##============================================================================================================##
 
 p <- list(objective = "binary:logistic",
           booster = "gbtree",
           eval_metric = "auc",
           nthread = 4,
           eta = 0.07,
           max_depth = 4,
           min_child_weight = 24,
           gamma = 36.7126,
           subsample = 0.9821,
           colsample_bytree = 0.3929,
           colsample_bylevel = 0.6818,
           alpha = 72.7519,
           lambda = 5.4826,
           max_delta_step = 5.7713,
           scale_pos_weight = 94,
           nrounds = 2000)
 
 m_xgb <- xgb.train(p, dtrain, p$nrounds,watchlist=watchlist, print_every_n = 50, early_stopping_rounds = 150)
 
confusionMatrix(as.numeric(predict(m_xgb,dtest)>0.5),testLab_num$is_attributed)

##it seems that numerical model have a close performance
##============================================================================================================##
###parameter2  try out with more general case with higher eta value to reduce the convergence time
##============================================================================================================##
p2 <- list(objective = "binary:logistic",
          booster = "gbtree",
          eval_metric = "auc",
          nthread = 8,
          eta = 0.09,
          max_depth = 3,
          min_child_weight = 24,
          gamma = 4,
          subsample = 0.8,
          lambda = 5,
          max_delta_step = 6,
          scale_pos_weight = 100,
          nrounds = 1500)

m2_xgb <- xgb.train(p2, dtrain, p$nrounds,watchlist=watchlist, print_every_n = 20, early_stopping_rounds = 150)

confusionMatrix(as.numeric(predict(m2_xgb,dtest)>0.5),testLab_num$is_attributed)
##it seems that we can achieve close performance in this setting as well.

##============================================================================================================##
## parameter3   test out the performance og larger scale_pos_weight to see if we can achieve higher accuracy in minority class
##============================================================================================================##
p3 <- list(objective = "binary:logistic",
           booster = "gbtree",
           eval_metric = "auc",
           nthread = 4,
           eta = 0.06,
           max_depth = 3,
           subsample = 0.8,
           min_child_weight = 24,
           gamma = 4,
           lambda = 5,
           max_delta_step = 6,
           scale_pos_weight = 200,
           nrounds = 1500)

m3_xgb <- xgb.train(p3, dtrain, p$nrounds,watchlist=watchlist, print_every_n = 20, early_stopping_rounds = 150)

confusionMatrix(as.numeric(predict(m3_xgb,dtest)>0.5),testLab_num$is_attributed)
##importance matrix
importance_matrix <- xgb.importance(colnames(trainingData_num),model = m3_xgb)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

##
xgb.dump(m3_xgb, with_stats = T)
 ##the influence isn't that significant

##============================================================================================================##
##parameter4  try slightly differnt parameters and recorde trainning time
##============================================================================================================##
start_time4=Sys.time()
p4 <- list(objective = "binary:logistic",
           booster = "gbtree",
           eval_metric = "auc",
           nthread = 4,
           eta = 0.03,
           max_depth = 4,
           subsample = 0.7,
           min_child_weight = 20,
           gamma = 4,
           lambda = 5,
           max_delta_step = 4,
           scale_pos_weight = 100,
           nrounds = 1500)

m4_xgb <- xgb.train(p4, dtrain, p$nrounds,watchlist=watchlist, print_every_n = 20, early_stopping_rounds = 150)

confusionMatrix(as.numeric(predict(m4_xgb,dtest)>0.5),testLab_num$is_attributed)
end_time4=Sys.time()
r_time_4=end_time4-start_time4

##sampling
#train_down=downSample(y=trainingData$is_attributed_f,x=trainingData)

#downsampling
# trainingData$is_attributed_f=as.factor(trainingData$is_attributed)
# trainingData$hour=hour(trainingData$click_time)
# train_down=downSample(y=trainingData$is_attributed_f,x=trainingData)
# train_down_r=train_down[,-c(1,7,8,10,12)]
# table(train_down$is_attributed)
# #train_down_r$hour=hour(train_down_r$click_time)
# train_down_r[]= lapply(train_down_r[],factor)
# testData <- train_sub[-inTraining_id,]

