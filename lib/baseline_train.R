##train.R

##baseline:  gbm model use SIFT feature


library(gbm)


baseline.model<-function(X.train, y.train, n.tree, shrinkage ){
  
  fit.model<-gbm.fit(x=X.train, y=y.train,
             n.trees=n.tree,
             distribution='bernoulli',
             interaction.depth=3, 
             bag.fraction = 0.5,
             shrinkage = shrinkage,
             verbose=FALSE)
  
  return(fit.model)
  
}

