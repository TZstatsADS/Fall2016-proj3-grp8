########################
### Random Forest Cross Validation ###
########################

### x.train : training feaure
### y.train : training label
### k       : k-fold
### 
rf.cv.function <- function(X.train, y.train,ntree,mtry,K){
  
  n <- length(y.train)
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    train.data <- X.train[s != i,]
    train.label <- y.train[s != i]
    test.data <- X.train[s == i,]
    test.label <- y.train[s == i]
    
    fit <- rf_train(train.data, train.label, ntree=ntree,mtry=mtry)
    pred <- rf_test(fit, test.data)  
    cv.error[i] <- mean(pred != test.label)  
    
  }			
  return(c(mean(cv.error),sd(cv.error)))
  
}
