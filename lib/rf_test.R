### random forest test

rf_test <- function(fit_train, dat_test){
  
  ### Fit the classfication model with testing data
  
  ### Input: 
  ###  - the fitted classification model using training data
  ###  -  processed features from testing images 
  ### Output: training model specification
  
  ### load libraries
  library(rpart)
  library(randomForest)
  pred <- predict(fit_train, newdata=dat_test)
  return(pred)
}