#baseline_test

baseline_test<- function(model.gbm, model.adv, X.trian){
  
  gbm_pred<-predict(model.gbm, X.trian)
  adv_pred<-predict(model.adv, X.train)
  
  
  return(list(baseline=gbm_pred,advanced=adv_pre))
  
}

