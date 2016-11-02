install.packages("data.table")
library(randomForest)
library(gbm)
library(data.table)
setwd("/Users/jianitian/Desktop/project3_final")

X.test_sift=t(read.csv("sift_features.csv"))##read the sift new feature provided, rows are observations, cols are features
load("fit_196.RData")
load("fitted_gbm_model.RData")
load("training_feature.RData")

baseline_test<- function(model.gbm,model.adv,X.test_sift,X.test_modified){
  
  library(randomForest)
  library(gbm)
  
  best_iter<-gbm.perf(model.gbm,method = "OOB")
  
  gbm_pred<-predict(model.gbm, X.test_sift, n.tree=best_iter,type='response')
  gbm_pred<-as.numeric(gbm_pred > 0.5)
  ### Fit the classfication model with testing data
  
  ### Input: 
  ###  - the fitted classification model using training data
  ###  -  processed features from testing images 
  ### Output: training model specification
  ### save(file="fitted_rf_model.RData",rf_fit)
  
  adv_pred <- predict(model.adv, newdata=X.test_modified)
  return(list(baseline=gbm_pred,advanced=adv_pred))
}
pre<-baseline_test(base_train,fit_196,dat_train,sub_feature)
