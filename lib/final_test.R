#baseline_test
library(randomForest)
library(gbm)

baseline_test<- function(model.gbm,model.adv,X.trian){
  
  best_iter<-gbm.perf(model.gbm,method = "OOB")
  
  gbm_pred<-predict(model.gbm, X.trian, n.tree=best_iter,type='response')
  gbm_pred<-as.numeric(gbm_pred > 0.5)
  
  
  
  
  ### Fit the classfication model with testing data
  
  ### Input: 
  ###  - the fitted classification model using training data
  ###  -  processed features from testing images 
  ### Output: training model specification
  ### save(file="fitted_rf_model.RData",rf_fit)
  
  adv_pred <- predict(model.adv, newdata=X.trian)
  


  
  return(list(baseline=gbm_pred,advanced=adv_pred))
}

