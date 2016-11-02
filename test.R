#baseline_test
#Usage: baseline_test(model.gbm=base_train,model.adv=rf_fit,X.test_ori=X.test_ori,X.test_modified=feature)
library(randomForest)
library(gbm)
library(data.table)
setwd("C:/Users/ys2882/Downloads")
load("fitted_rf_model.RData")
load("fitted_gbm_model.RData")
load("training_feature.RData")
X.test_ori=t(fread("sift_features.csv"))
baseline_test<- function(model.gbm,model.adv,X.test_ori,X.test_modified){

  library(randomForest)
  library(gbm)
  
  best_iter<-gbm.perf(model.gbm,method = "OOB")
  
  gbm_pred<-predict(model.gbm, X.test_ori, n.tree=best_iter,type='response')
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

