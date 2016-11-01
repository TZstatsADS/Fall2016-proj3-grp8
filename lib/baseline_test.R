#baseline_test

baseline_test<- function(model.gbm, X.trian){
  
  best_iter<-gbm.perf(model.gbm,method = "OOB")
  
  gbm_pred<-predict(model.gbm, X.trian, n.tree=best_iter,type='response')
  gbm_pred<-as.numeric(gbm_pred > 0.5)
  
  
  return(baseline=gbm_pred)
}

