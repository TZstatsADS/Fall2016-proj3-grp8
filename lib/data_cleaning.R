# ads project 3
# 
library(data.table)
library(dplyr)
library(rpart)
library(randomForest)
setwd("G:/Columbia/study/3rd semester/5243/project3/Project3_poodleKFC_train/")
dat_train=fread("sift_features.csv")
label1=data.frame(rep(0,1000))
label2=data.frame(rep(1,1000))
label_train=c(t(label1),t(label2))
dat_train=tbl_df(t(dat_train))    # column denotes feature, row denotes pictures
dat_train=mutate(dat_train,label=label_train) # the last column is label,
                                              # 0:chicken, 1: dog
### decision tree part
tree_fit <- rpart(label~ .,
             method="class", data=dat_train)
