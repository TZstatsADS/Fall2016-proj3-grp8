### Input: dir of the csv file
### Output: feature 2000*196
#install.packages("randomForest")
setwd("/Users/jianitian/Desktop/final_project3_1")
setwd(dir)
dat_train=read.csv("sift_features_test.csv")
dat_train=t(dat_train)
choose_feature=function(dir){
  library(data.table)
  library(dplyr)
  library(rpart)
  library(randomForest)
  
  #setwd("/Users/jianitian/Desktop/project3_final")
  #dir="/Users/jianitian/Desktop/project3_final"
  setwd(dir)
  label_train=c(t(data.frame(rep(1,1000))),t(data.frame(rep(0,1000))))
  dat_train=fread("sift_features.csv")
  
  
  ### loading RGB feature: rgb_feature_all
  load("rgb_feature.RData")
  rgb_feature_all=tbl_df(rgb_feature_all)
  #rgb_feature_all=tbl_df(rgb_feature(dir))
  
  ### load feature from Xuechun
  load("orb_features_500.RData")
  orb_features_500=tbl_df(orb_features_500)
  
  load("pca1.RData")
  
  
  feature_ori=data.frame(dat_train,rgb_feature_all,orb_features_500)
  #sub_feature=as.matrix(feature_ori) %*% pca1_rot[,1:196]
  sub_feature=predict(pca1,newdata=feature_ori)[,1:196]
  ### PCA on feature csv
  #pca1=prcomp(feature_ori)
  #pca1_x=data.frame((pca1$x))
  #var_pca=cumsum((pca1$sdev)^2/sum((pca1$sdev)^2))
  #cutoff=min(which(var_pca>0.8))
  #sub_feature=mutate(pca1_x[,1:cutoff])
 
  
  ### combine the features into 1 final feature
  #save(file="feature_196.RData",sub_feature)
  return(sub_feature)
}
fea <- choose_feature("/Users/jianitian/Desktop/project3_final")
save(fea,file = "feature_196.RData")
