# ads project 3
# 
library(data.table)
library(dplyr)
library(rpart)
library(randomForest)
library(caret)
set.seed(7)
setwd("C:/Users/ys2882/Downloads/Project3_poodleKFC_train/")
dat_train=fread("sift_features.csv")
#save(dat_train,file="dat_train.RData")
#load('dat_train.RData')
label_train=c(t(data.frame(rep(0,1000))),t(data.frame(rep(1,1000))))
sub_feature=prcomp(dat_train)
sub_feature2=data.frame(t(sub_feature$x))
sub_feature2=mutate(sub_feature2,label=as.factor(label_train)) # the last column is label,

#dat_train=data.frame(t(dat_train))    # column denotes feature, row denotes pictures
#dat_train=mutate(dat_train,label=as.factor(label_train)) # the last column is label,

# source("http://bioconductor.org/biocLite.R") 
# biocLite("EBImage")

### provide test sample
# test1=dat_train %>% 
#   group_by(label)%>%
#   sample_n(.,size=1000,replace=FALSE)  # 0:chicken, 1: dog
# ### decision tree part
# tree_fit <- rpart(label~ .,
#                   method="class", data=test1,
#                   control = rpart.control(minsplit=30, cp=.005))
# printcp(tree_fit) # display the results
# plotcp(tree_fit)
# plot(tree_fit, uniform=TRUE, 
#      main="Classification Tree for Kyphosis")
# text(tree_fit, use.n=TRUE, all=TRUE, cex=.8)
# summary(tree_fit)
predictions <- predict(tree_fit, test1, type="class")
table(test1$label, predictions)

#sub_feature=dat_train %>% sample_n(size=1000,replace=F)
### Use caret to tune parameter
seed=7
control <- trainControl(method="repeatedcv", number=5, repeats=3, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(60:61))
metric <- "Accuracy"
rf_gridsearch <- caret::train(label~., data=dat_train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)




source("rf_train.R")
source("rf_test.R")
source("rf_cv.R")
rf.cv.function(dat_train,dat_train$label,ntree=150,mtry=80,K=5)
tuneRF(dat_train[,1:5000], dat_train$label, mtry=35,ntreeTry=100, 
       stepFactor=1.1, improve=0.001,trace=TRUE, plot=TRUE, doBest=FALSE)
