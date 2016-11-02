library(caret)
library(data.table)
library(dplyr)
library(gbm)
sift_features <- fread('/Users/sun93/Documents/ADS/pro3/Project3_poodleKFC_train/sift_features.csv',header = TRUE)
dim(sift_features)
sift_features <- as.matrix(as.data.frame(sift_features))
#########################################################
#Data cleaning
# 0 stands for chicken, 1 stands for dog
#column denotes feature, row denotes pictures
sift_train <- tbl_df(t(sift_features)) 
dim(sift_train)
str(sift_train)

label1 <- data.frame(rep(0,1000))
label2 <- data.frame(rep(1,1000))
label_train <- c(t(label1),t(label2))
# the last column is label
sift_train <- mutate(sift_train,label=label_train) 
sift_train$label <- as.factor(sift_train$label)

#########################################################
# pca
sift.pca <- prcomp(as.data.frame(sift_train[,-5001]),center = TRUE,scale. = TRUE)

index <- rep(1:5000, 1)
runPCA <- function(mat = 'Unadjusted matrix') eigen(cor(apply(mat, 2, function(i) i - mean(i))))

pca <- runPCA(as.data.frame(sift_train[,-5001]))

varExplained <- function(eigenList) {
  line_0.8 <- index[cumsum(eigenList$value) / sum(eigenList$value) >= 0.8]
  line_0.95 <- index[cumsum(eigenList$value) / sum(eigenList$value) >= 0.95]
  par(mfrow = c(1,2))
  plot(
    eigenList$value / sum(eigenList$value), pch = 19, col = 'red', bg = '#549cc4', ylim = c(0, 0.05), xlab = 'Principal Component', ylab = 'Variance Explained'
  )
  #plot the proportion variance explained by each principle component
  plot(
    cumsum(eigenList$value) / sum(eigenList$value), pch = 21,
    col = 'blue', bg = '#549cc4', ylim = c(0, 1), xlab = 'Principal Component', ylab = 'Cumulative Variance Explained'
  ) + abline(h = 0.8) + abline(h = 0.95) + abline(v = line_0.8[1]) + abline(v = line_0.95[1])
  #plot the cumulative proportion variance explained by principle components 
  print(line_0.8[1])
  print(line_0.95[1])
}

varExplained(pca)

# extract first n PCs
n = 1000
#sift.score.n <- as.matrix(sift_train[,-5001]) %*% sift.pca$rotation[,c(1:n)]

sift.score.1748 <- as.matrix(sift_train[,-5001]) %*% sift.pca$rotation[,c(1:1748)]
sift.score.1800 <- as.matrix(sift_train[,-5001]) %*% sift.pca$rotation[,c(1:1800)]
sift.score.2000 <- as.matrix(sift_train[,-5001]) %*% sift.pca$rotation[,c(1:2000)]


#########################################################
# GBM Model
GBM.cv.function2 <- function(X.train, y.train, d, K){
  
  n <- length(y.train)
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    train.data <- X.train[s != i,]
    train.label <- y.train[s != i]
    test.data <- X.train[s == i,]
    test.label <- y.train[s == i]
    
    par <- list(depth=d)
    fit_gbm <- gbm.fit(x=train.data, y=train.label,
                       n.trees=1700,
                       distribution='bernoulli',
                       interaction.depth=3, 
                       bag.fraction = 0.5,
                       shrinkage = 0.005,
                       verbose=FALSE)
    best_iter <- gbm.perf(fit_gbm, method="OOB")
    
    pred <- predict(fit_gbm,test.data,n.trees=best_iter,type='response')
    
    #cv.error[i] <- mean(pred != test.label) 
    pred <- as.numeric(pred > 0.5)
    cv.error[i] <- mean(pred != test.label) 
    print(cv.error[i])
  }			
  return(c(mean(cv.error),sd(cv.error)))
  
}

GBM.cv.function2(as.data.frame(sift_train[,-5001]),as.vector(sift_train$label),3,5)

GBM.cv.function2(sift.score.1748,as.vector(sift_train$label),3,5) #0.278500000
GBM.cv.function2(sift.score.1800,as.vector(sift_train$label),3,5) #0.274
GBM.cv.function2(sift.score.2000,as.vector(sift_train$label),3,5) #0.267

#########################

