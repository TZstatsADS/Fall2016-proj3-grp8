
feature=function(dir){
  library(data.table)
  library(dplyr)
  library(rpart)
  library(randomForest)
  
#setwd("C:/Users/ys2882/Downloads")
  setwd(dir)
  dat_train=fread("sift_features.csv")
  label_train=c(t(data.frame(rep(1,1000))),t(data.frame(rep(0,1000))))
### PCA on original feature csv
  pca1=prcomp(t(dat_train))
  pca1_x=data.frame((pca1$x))
  var_pca=cumsum((pca1$sdev)^2/sum((pca1$sdev)^2))
  cutoff=min(which(var_pca>0.45))
  sub_feature=mutate(pca1_x[,1:cutoff])

### loading RGB feature: rgb_feature_all
  load("rgb_feature.RData")
  rgb_feature_all=tbl_df(rgb_feature_all)
  #rgb_feature_all=tbl_df(rgb_feature(dir))

### load feature from Xuechun
  load("orb_features_500.RData")

### combine the features into 1 final feature
  feature=data.frame(sub_feature,rgb_feature_all,orb_features_500,label_train)
  #save(file="feature.RData",feature)
  return(feature)
}