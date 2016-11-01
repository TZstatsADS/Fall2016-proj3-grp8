##################neural network cv to choose layers ##################################
##################written by Jiani  Tian##############################################

#install.packages("rPython")
#install.packages("EBImage")
#install.packages("bioLite")
#install.packages("neuralnet")
#install.packages("nnet")
setwd("~/Desktop/ads project3")
library(neuralnet)
library(nnet)
library(rPython)
library(EBImage)
#library(bioLite)
source("http://bioconductor.org/biocLite.R") 
biocLite("EBImage") 

siftfeature.data<-read.csv("sift_features.csv")
siftfeature.data <- t(siftfeature.data)
apply(siftfeature.data,2,function(x) sum(is.na(x)))
ylable <- c(rep(0,1000),rep(1,1000))
siftfeature.data.train<-cbind(siftfeature.data,ylable)
maxs <- apply(siftfeature.data.train, 2, max)
mins <- apply(siftfeature.data.train, 2, min)
scaled <- as.data.frame(scale(siftfeature.data.train, center = mins, scale = maxs - mins))
set.seed(500)
index <- sample(1:nrow(scaled),round(0.75*nrow(scaled)))
train_ <- scaled[index,]
test_ <- scaled[-index,]


cv.function <- function(X.train, y.train, d, K){
  
  n <- length(y.train)
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    #i<-1
    train.data <- X.train[s != i,]
    train.label <- y.train[s != i]
    test.data <- X.train[s == i,]
    test.label <- y.train[s == i]
    train.total <- cbind(train.data,train.label)
    n1 <- names(train.total)
    f <- as.formula(paste("train.label ~", paste(n1[!n1 %in% "train.label"], collapse = " + ")))
    fit <- neuralnet(f,data=train.total,hidden=d,linear.output=F)
    pred <- compute(fit,test.data)
    pr.nn_ <- pred$net.result*(max(train.total$train.label)-min(train.total$train.label))+min(train.total$train.label)
    test.r <- (test.label)*(max(train.total$train.label)-min(train.total$train.label))+min(train.total$train.label)
    cv.error[i] <-  sum((test.r - pr.nn_)^2)/nrow(test.data)
  }			
  return(c(mean(cv.error),sd(cv.error)))
}

cv.function(scaled[,1:5000],scaled[,5001],0,8)

best.d <- vector()

for(i in 0:10){
  best.d[i] <- cv.function(scaled[,1:5000],scaled[,5001],i,10)
}
#########################################################################








