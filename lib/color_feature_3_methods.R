### Extract RGB feature
library(R.matlab)
library(dplyr)
library(randomForest)
library("e1071")
setwd("C:/Users/ys2882/Downloads")
dir_images <- "./images"
dir_names <- list.files(dir_images)
label_train=c(t(data.frame(rep(1,1000))),t(data.frame(rep(0,1000))))

########################################################################## RGB

# source("http://bioconductor.org/biocLite.R")
# biocLite()
# biocLite("EBImage")
library("EBImage")
# tuning parameters.
nR <- 10
nG <- 8
nB <- 10
rBin <- seq(0, 1, length.out=nR)
gBin <- seq(0, 1, length.out=nG)
bBin <- seq(0, 1, length.out=nB)

## create feature files
#rgb_feature <- list()
rgb_feature_all=matrix(data=NA,nrow=length(dir_names),ncol=nR*nG*nB)
for (i in (1:length(dir_names))) {
  img <- readImage(paste("./images/", dir_names[i], sep = ""))
  img <- resize(img,250,250)
  mat <- imageData(img)
  #check which RGB interval the image belongs to, the table function counts it.
  freq_rgb <- as.data.frame(table(factor(findInterval(mat[,,1], rBin), 
                                         levels=1:nR), 
                                  factor(findInterval(mat[,,2], gBin), 
                                         levels=1:nG), 
                                  factor(findInterval(mat[,,3], bBin), 
                                         levels=1:nB)))
  rgb_feature <- as.numeric(freq_rgb$Freq)/(ncol(mat)*nrow(mat))
  rgb_feature_all[i,]=rgb_feature
}
save(file="rgb_feature.RData",rgb_feature_all)
# ### PCA
# sub_feature=prcomp(rgb_feature_all)
# sub_feature2=data.frame(sub_feature$x[,1:500])
# sub_feature3=mutate(sub_feature2,label=as.factor(label_train))
# 
# dat_train=mutate(data.frame(rgb_feature_all),label=factor(label_train))
# randomForest(label~.,data=sub_feature3)
# svm1=svm(label~.,data=sub_feature3)
# pred=predict(svm1,sub_feature3[,1:500])
# table(pred,label_train)

########################################################################## HSV
# library(grDevices)
nH <- 10
nS <- 6
nV <- 6
hBin <- seq(0, 1, length.out=nH)
sBin <- seq(0, 1, length.out=nS)
vBin <- seq(0, 0.005, length.out=nV)
hsv_feature_all=matrix(data=NA,nrow=length(dir_names),ncol=nH*nS*nV)

for (i in (1:length(dir_names))) {
  img <- readImage(paste("./images/", dir_names[i], sep = ""))
  img <- resize(img,250,250)
  mat <- imageData(img)
  mat_rgb <- mat
  dim(mat_rgb) <- c(nrow(mat)*ncol(mat), 3)
  mat_hsv <- rgb2hsv(t(mat_rgb))
  freq_hsv <- as.data.frame(table(factor(findInterval(mat_hsv[1,], hBin), levels=1:nH), 
                                  factor(findInterval(mat_hsv[2,], sBin), levels=1:nS), 
                                  factor(findInterval(mat_hsv[3,], vBin), levels=1:nV)))
  hsv_feature <- as.numeric(freq_hsv$Freq)/(ncol(mat)*nrow(mat)) # normalization
  hsv_feature_all[i,]=hsv_feature
}
save(file="hsv_feature.RData",hsv_feature_all)

########################################################################## Spatial

N <- 3 # number of bins in x-axis
M <- 5 # number of bins in y-axis
p_x <- p_y <- 250
xbin <- floor(seq(0, p_x, length.out= N+1))
ybin <- seq(0, p_y, length.out=M+1)
spacial_feature_all=matrix(data=NA,nrow=length(dir_names),ncol=12000)

construct_rgb_feature <- function(X){
  freq_rgb <- as.data.frame(table(factor(findInterval(X[,,1], rBin), levels=1:nR), 
                                  factor(findInterval(X[,,2], gBin), levels=1:nG), 
                                  factor(findInterval(X[,,3], bBin), levels=1:nB)))
  rgb_feature <- as.numeric(freq_rgb$Freq)/(ncol(X)*nrow(X))
  return(rgb_feature)
}

for (i in (1:length(dir_names))) {
  print(i)
  img <- readImage(paste("./images/", dir_names[i], sep = ""))
  img_s <- resize(img, p_x, p_y)
  scf <- rep(NA, N*M*nR*nG*nB)
  for(k in 1:N){
    for(j in 1:M){
      tmp <- img_s[(xbin[k]+1):xbin[k+1], (ybin[j]+1):ybin[j+1], ]
      scf[((M*(k-1)+j-1)*nR*nG*nB+1):((M*(k-1)+j)*nR*nG*nB)] <- construct_rgb_feature(tmp) 
    }
  }
  spacial_feature_all[i,]=scf
  # name <- unlist(strsplit(dir_names[i], "[.]"))[1]
  # # create a new folder named "spatial_color_feature", which is in the same directory as images folder.
  # writeMat(paste("./spatial_color_feature/", name, ".mat", sep = ""), scfexport = scf)
}
save(file="spacial_feature.RData",spacial_feature_all)


