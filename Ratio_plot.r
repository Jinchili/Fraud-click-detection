library(caret)
library(RWeka) # For J48 
library(e1071) # For NB
library(Boruta)
library(DMwR)
library(data.table)
library(dplyr)
library(xgboost)
library(Matrix)
day1=read.csv("D:/DL/day1.csv")
day2=read.csv("D:/DL/day2.csv")
day3=read.csv("D:/DL/day3.csv")
day4=read.csv("D:/DL/day4.csv")
total=NULL
total$hour=c(day1$hour,day2$hour,day3$hour,day4$hour)
total$ratio=c(day1$ratio,day2$ratio,day3$ratio,day4$ratio)
plot(total$hour,total$ratio)

##read subset of data
train_sub=fread("train_sub.csv")
#rf_r_10=train(is_attributed~.,data=train_sub,method='rf')
#egb_r=train(is_attributed~.,data=train_sub,method='xgbTree')
set.seed(123456)
inTraining_id <- createDataPartition(train_sub$is_attributed, p = .677, list = FALSE,times =1) #sampleing without replacement
trainingData <- train_sub[inTraining_id,]
testData <- train_sub[-inTraining_id,]
summary(trainingData)

#downsampling
trainingData$is_attributed_f=as.factor(trainingData$is_attributed)
trainingData$hour=hour(trainingData$click_time)

train_down=downSample(y=trainingData$is_attributed_f,x=trainingData)
write.csv(x = train_down,file = 'train_down.csv')

train_down_r=train_down[,-c(1,7,8,10,12)]
table(train_down$is_attributed)
#train_down_r$hour=hour(train_down_r$click_time)
train_down_r[]= lapply(train_down_r[],factor)
#train_down_r$hour=hour(train_down_r$is_attributed)

##convert to sparse matrix
train_m=sparse.model.matrix(is_attributed ~ .-1,data=train_down_r)
train_lab=as.numeric(train_down_r$is_attributed)-1
train_matrix=xgb.DMatrix(data=train_m,label=train_lab)

testData$hour=hour(testData$click_time)
test_m=sparse.model.matrix(is_attributed ~ .-1,data=testData)
head(test_m)
test_lab=as.integer(testData$is_attributed)
test_matrix=xgb.DMatrix(data=test_m,label=test_lab)

watchlist<-list(train=train_matrix, test=test_matrix)

##Parameter
nc=2
xgb_params<-list("objective"="multi:softprob",
                 "eval_metric"="mlogloss",
                 "num_class"=nc
)
bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = 100,
                       eta = 0.001,
                       max.depth = 3,
                       watchlist=watchlist,
                       gamma = 0,
                       subsample = 1,
                       colsample_bytree = 1,
                       missing = NA,
                       seed = 123456)

confusionMatrix(predict(bst_model,test_matrix$data),test_matrix$test_lab)

xgb_linear_params<-list("objective"="reg:linear","eta"=
                      gama=)

##



##test other way
test_nb=NULL
test_nb$index=1:6
test_nb$cool=as.factor(c(1,0,1,0,3,2))
test_nb$is_attributed=c(1,0,0,1,0,1)
test_nb$some=as.factor(c(1,178,10,3,50,250))
test_nb$is_f=as.factor(c(1,178,10,3,50,250))

#test_nb[]=lapply(test_nb[],factor)

sparse.model.matrix(is_attributed ~ .-1,data=test_nb)[1,]

train_down_r[c(1:10),]
train_m_p=data.matrix(train_down_r[c(1:10),])
train_m_p=Matrix(train_m_p,sparse=TRUE)
train_m=sparse.model.matrix(is_attributed~.,data=train_down_r[c(1),])


data(agaricus.train, package='xgboost')
train <- agaricus.train
t_1=train[1]
dtrain <- xgb.DMatrix(train$data, label=train$label)

dtrain =xgb.DMatrix(data = data.matrix(train_down_r), label = is_attributed)
egb_r=xgboost(data = data.matrix(train_down_r[,-7]), label = as.numeric(train_down_r$is_attributed), max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic")

##
set.seed(0)
ctrl$sampling <- "down"
grid$nrounds <- 35
m_xgb_down <- train(is_attributed ~ ., data = train_down_r,
                    method = "xgbTree",
                    nthread = 8,
                    metric = "ROC",
                    tuneGrid = grid,
                    trControl = ctrl)

