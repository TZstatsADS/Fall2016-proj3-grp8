# ads project 3
# 
library(data.table)
library(dplyr)
setwd("G:/Columbia/study/3rd semester/5243/project3/Project3_poodleKFC_train/")
dat_train=fread("sift_features.csv")
label1=data.frame(rep("chicken",1000))
label2=data.frame(rep("dog",1000))
label_train=c(t(label1),t(label2))

