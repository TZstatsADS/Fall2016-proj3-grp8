# ads project 3
# 
library(data.table)
library(dplyr)
library(rpart)
library(randomForest)
library(caret)
library(EBImage)
set.seed(7)
setwd("G:/Columbia/study/3rd semester/5243/project3/Project3_poodleKFC_train/")
dat_train=fread("sift_features.csv")
img_dir="G:/Columbia/study/3rd semester/5243/project3/Project3_poodleKFC_train/images/"
img_name=list.files(path =img_dir, 
           pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

#save(dat_train,file="dat_train.RData")
#load('dat_train.RData')
label_train=c(t(data.frame(rep(1,1000))),t(data.frame(rep(0,1000))))
sub_feature=prcomp(t(dat_train))
sub_feature2=data.frame((sub_feature$x))
cutoff=min(which(var_pca>0.45))
sub_feature3=mutate(sub_feature2[,1:cutoff],label=as.factor(label_train)) # the last column is label,
randomForest(label~.,dat=sub_feature3,mtry=20,ntree=701)
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
tuneRF(sub_feature3, sub_feature3$label, mtry=70,ntreeTry=500, 
       stepFactor=2, improve=0.05,trace=TRUE, plot=TRUE, doBest=FALSE)
