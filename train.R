

######train baseline model

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


#######train advancede model

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


