  
  ### Input: 
  ###  - the fitted classification model using training data
  ###  -  processed features from testing images 
  ### Output: training model specification
  
library(data.table)
library(randomForest)
library(gbm)
library(data.table)
setwd("/Users/jianitian/Desktop/project3_final")
setwd(dir)
dat_test=read.csv("sift_features_test.csv")
dat_test=t(dat_train)

# load test sift feature and modified feature 
X.test_sift=t(read.csv("sift_features.csv"))##read the sift new feature provided, rows are observations, cols are features
load("feature_196.RData")

# load fitted model
load("fitted_gbm_model.RData")
load("fit_196.RData")

baseline_test<- function(model.gbm,model.adv,X.test_sift,X.test_modified){

  library(randomForest)
  library(gbm)
  # baseline_model, gbm model
  best_iter<-gbm.perf(model.gbm,method = "OOB")
  gbm_pred<-predict(model.gbm, X.test_sift, n.tree=best_iter,type='response')
  gbm_pred<-as.numeric(gbm_pred > 0.5)

  # advanced_model, random_forest
  adv_pred <- predict(model.adv, newdata=X.test_modified)
  return(list(baseline=gbm_pred,advanced=adv_pred))
}
# run the 2 models
pre<-baseline_test(base_train,fit_196,dat_test,feature_196)
