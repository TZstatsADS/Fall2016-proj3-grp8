## random forest train

rf_train=function(dat_train, label_train,ntree,mtry)
{
  ### Train a Decision using processed features from training images
  
  ### Input: 
  ###  -  processed features from images 
  ###  -  class labels for training images
  ### Output: training model specification
  ### Default: minsplit=30,cp=0.005
  ### load libraries
  library(data.table)
  library(dplyr)
  library(rpart)
  library(randomForest)
  
  ### Train with decision model
  rf_fit <- randomForest(label~ .,
                    data=dat_train,
                    importance=TRUE, 
                    ntree=ntree,
                    mtry=mtry)
  return(rf_fit)
}
