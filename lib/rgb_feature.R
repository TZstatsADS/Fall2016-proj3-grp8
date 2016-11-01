### ATTENTION
### We have 13 images that cannot be read in R, so delete those images before running this code.
### Their names are "Abyssinian_34", "Abyssinian_5", "Egyptian_Mau_129", "Egyptian_Mau_139",
### "Egyptian_Mau_14", "Egyptian_Mau_145", "Egyptian_Mau_156", "Egyptian_Mau_167",
### "Egyptian_Mau_177", "Egyptian_Mau_186", "Egyptian_Mau_191", "staffordshire_bull_terrier_2", 
### "staffordshire_bull_terrier_22".

library(R.matlab)
setwd("G:/Columbia/study/3rd semester/5243/project3/Project3_poodleKFC_train/")
dir_images <- "./images"
dir_names <- list.files(dir_images)

# breed_name <- rep(NA, length(dir_names))
# for(i in 1:length(dir_names)){
#   tt <- unlist(strsplit(dir_names[i], "_"))
#   tt <- tt[-length(tt)]
#   breed_name[i] = paste(tt, collapse="_", sep="")
# }
# cat_breed <- c("Abyssinian", "Bengal", "Birman", "Bombay", "British_Shorthair", "Egyptian_Mau",
#                "Maine_Coon", "Persian", "Ragdoll", "Russian_Blue", "Siamese", "Sphynx")
# 
# iscat <- breed_name %in% cat_breed
# y_cat <- as.numeric(iscat)
# 
# # I renamed the label "0" to be "-1" just for convenience when using certain algorithms like perceptron.
# for (i in (1:length(y_cat))) {
#   if (y_cat[i] == 0) {
#     y_cat[i] <- -1
#   }
# }
# # this would be train labels.
# writeMat("y.mat", y_catexport = y_cat)

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
rgb_feature_all=matrix(data=NA,nrow=length(dir_names),ncol=800)
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
  #name <- unlist(strsplit(dir_names[i], "[.]"))[1]
  # create a new folder named "color_feature", which is in the same directory as images folder.
  #writeMat(paste("./color_feature/", name, ".mat", sep = ""), rgb_featureexport = rgb_feature)
}

########################################################################## HSV
library(grDevices)
nH <- 10
nS <- 6
nV <- 6
hBin <- seq(0, 1, length.out=nH)
sBin <- seq(0, 1, length.out=nS)
vBin <- seq(0, 0.005, length.out=nV)

for (i in (1:length(dir_names))) {
  print(i)
  img <- readImage(paste("./images/", dir_names[i], sep = ""))
  mat <- imageData(img)
  mat_rgb <- mat
  dim(mat_rgb) <- c(nrow(mat)*ncol(mat), 3)
  mat_hsv <- rgb2hsv(t(mat_rgb))
  freq_hsv <- as.data.frame(table(factor(findInterval(mat_hsv[1,], hBin), levels=1:nH), 
                                  factor(findInterval(mat_hsv[2,], sBin), levels=1:nS), 
                                  factor(findInterval(mat_hsv[3,], vBin), levels=1:nV)))
  hsv_feature <- as.numeric(freq_hsv$Freq)/(ncol(mat)*nrow(mat)) # normalization
  name <- unlist(strsplit(dir_names[i], "[.]"))[1]
  # create a new folder named "HSV_color_feature", which is in the same directory as images folder.
  writeMat(paste("./HSV_color_feature/", name, ".mat", sep = ""), hsv_featureexport = hsv_feature)
}


########################################################################## Spatial

N <- 3 # number of bins in x-axis
M <- 5 # number of bins in y-axis
p_x <- p_y <- 250
xbin <- floor(seq(0, p_x, length.out= N+1))
ybin <- seq(0, p_y, length.out=M+1)


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
  name <- unlist(strsplit(dir_names[i], "[.]"))[1]
  # create a new folder named "spatial_color_feature", which is in the same directory as images folder.
  writeMat(paste("./spatial_color_feature/", name, ".mat", sep = ""), scfexport = scf)
}



