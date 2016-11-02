### Extract RGB feature
### Usage: Input: dir, the main folder, which images are stored as a subfolder
###        Output: rgb_feature
rgb_feature=function(dir){
  library(R.matlab)
  library(dplyr)
  setwd(dir)
  dir_images <- "./new_images"
  dir_names <- list.files(dir_images)
  ########################################################################## RGB
  # MAKE SURE YOUR COMPUTER RUN THE FOLLOWING 3 LINES AHEAD!!!
  # source("http://bioconductor.org/biocLite.R")
  # biocLite()
  # biocLite("EBImage")
  library("EBImage")
  nR <- 10
  nG <- 8
  nB <- 10
  rBin <- seq(0, 1, length.out=nR)
  gBin <- seq(0, 1, length.out=nG)
  bBin <- seq(0, 1, length.out=nB)
  
  ## create feature files
  #rgb_feature <- list()
  rgb_feature_all=matrix(data=NA,nrow=length(dir_names),ncol=nR*nG*nB)
  for (i in (1:length(dir_names))) 
  {
    img <- readImage(paste(dir_images,"/", dir_names[i], sep = ""))
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
  return(rgb_feature_all)
  save(file="rgb_feature",rgb_feature_all)
}
