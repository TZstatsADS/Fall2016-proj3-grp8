library(data.table)
library(dplyr)
library(randomForest)
library(EBImage)
set.seed(7)
setwd("G:/Columbia/study/3rd semester/5243/project3/Project3_poodleKFC_train/")
dat_train=fread("sift_features.csv")
label_train=c(t(data.frame(rep(1,1000))),t(data.frame(rep(0,1000))))
dat_train=tbl_df(t(dat_train))

## random forest train
rf_train=function(dat_train, label_train,ntree,mtry)
{
  ### Train a Decision using processed features from training images
  
  ### Input: 
  ###  -  processed features from images 
  ###  -  class labels for training images
  ### Output: training model specification

  library(data.table)
  library(dplyr)
  library(randomForest)
  
  ### Train with decision model
  dat_train=data.frame(dat_train)
  data=mutate(dat_train,label=factor(label_train))
  rf_fit <- randomForest(label~ .,
                         data=data,
                         importance=TRUE, 
                         ntree=ntree,
                         mtry=mtry)
  return(rf_fit)
}

### random forest test

rf_test <- function(fit_train, dat_test){
  
  ### Fit the classfication model with testing data
  
  ### Input: 
  ###  - the fitted classification model using training data
  ###  -  processed features from testing images 
  ### Output: training model specification
  ### save(file="fitted_rf_model.RData",rf_fit)
  library(randomForest)
  pred <- predict(fit_train, newdata=dat_test)
  return(pred)
}

rf.cv.function <- function(X.train, y.train,ntree=500,mtry=sqrt(ncol(X.train)),K=5){
  
  n <- length(y.train)
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    train.data <- X.train[s != i,]
    train.label <- y.train[s != i]
    test.data <- X.train[s == i,]
    test.label <- y.train[s == i]
    
    fit <- rf_train(train.data, train.label, ntree=ntree,mtry=mtry)
    pred <- rf_test(fit, test.data)  
    cv.error[i] <- mean(pred != test.label)  
    
  }			
  return(c(mean(cv.error),sd(cv.error)))
  
}
#rf.cv.function(sub_feature,label_train,K=5)
