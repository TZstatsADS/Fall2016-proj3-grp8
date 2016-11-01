library(data.table)
library(dplyr)
library(gbm)
#feature.csv derives from .py file
orb_features <- fread('/Users/sun93/Desktop/feature.csv',header = FALSE)
#dim(orb_features)
#str(orb_features)
orb_features <- as.data.frame(orb_features)
colnames(orb_features)[33] <- c("Image_index")

set.seed(333)
orb_features_cluster <- kmeans(orb_features[,c(1:32)], centers = 500)
length(orb_features_cluster$size)
length(orb_features_cluster$cluster)

index <- rep(1:length(orb_features_cluster$cluster),1)
orb_cluster_500 <- matrix(NA, nrow = 500, ncol = 2000)
View(orb_cluster_500)

#get new probability matrix 
#2000 images
for (i in 1:2000){
  if (length(index[orb_features$Image_index == i]) == 0)
    orb_cluster_500[,i] = 0
  else{
    kp_list <- index[orb_features$Image_index == i]
    #500 clusters
    for (k in 1 : 500){
      cluster_list <- index[orb_features_cluster$cluster == k]
      total_kp <- length(kp_list)
      num = 0
      for (j in 1:length(cluster_list)){
        if (cluster_list[j] >= min(kp_list) & cluster_list[j] <= max(kp_list))
        num = num + 1
      }
      orb_cluster_500[k,i] <- num / total_kp
  }
  }
  print(i)}

#test
apply(orb_cluster_500, 2, function(i) sum(i))
apply(orb_cluster_500, 2, function(i) sum(i))[1000:2000]

#########################
# add new feature
sift_features <- fread('/Users/sun93/Documents/ADS/pro3/Project3_poodleKFC_train/sift_features.csv',header = TRUE)
sift_features_add1 <- as.data.frame(sift_features)
#dim(sift_features_add1)
#dim(orb_cluster_500)
#colnames(sift_features_add1)
colnames(orb_cluster_500) <- colnames(sift_features_add1)
sift_features_add1 <- rbind(sift_features_add1,orb_cluster_500)
sift_features_add1 <- transpose(sift_features_add1)
label1 <- data.frame(rep(1,1000))
label2 <- data.frame(rep(0,1000))
label_train <- as.data.frame(c(t(label1),t(label2)))
sift_features_add1_label <- cbind(sift_features_add1,label_train)
colnames(sift_features_add1_label)[5501] <- c("label")

#add orb feature to original features
save(sift_features_add1_label, file = "/Users/sun93/Documents/ADS/pro3/codes/add_ORB_f.RData")

#########################################
#pca new feature
# pca
sift_orb.pca <- prcomp(sift_features_add1,center = TRUE,scale. = TRUE)


index <- rep(1:5500, 1)
runPCA <- function(mat = 'Unadjusted matrix') eigen(cor(apply(mat, 2, function(i) i - mean(i))))

pca.orb <- runPCA(sift_features_add1)

varExplained <- function(eigenList) {
  line_0.8 <- index[cumsum(eigenList$value) / sum(eigenList$value) >= 0.8]
  line_0.95 <- index[cumsum(eigenList$value) / sum(eigenList$value) >= 0.95]
  par(mfrow = c(1,2))
  plot(
    eigenList$value / sum(eigenList$value), pch = 19, col = 'red', bg = '#549cc4', ylim = c(0, 0.05), xlab = 'Principal Component', ylab = 'Variance Explained'
  )
  #plot the proportion variance explained by each principle component
  plot(
    cumsum(eigenList$value) / sum(eigenList$value), pch = 21,
    col = 'blue', bg = '#549cc4', ylim = c(0, 1), xlab = 'Principal Component', ylab = 'Cumulative Variance Explained'
  ) + abline(h = 0.8) + abline(h = 0.95) + abline(v = line_0.8[1]) + abline(v = line_0.95[1])
  #plot the cumulative proportion variance explained by principle components 
  print(line_0.8[1])
  print(line_0.95[1])
}

varExplained(pca.orb)

# extract first n PCs
n = 1000
#sift.score.n <- as.matrix(sift_train[,-5001]) %*% sift.pca$rotation[,c(1:n)]

#########################################
#run gbm model 
sift_orb.score.1200 <- as.matrix(sift_features_add1) %*% sift_orb.pca$rotation[,c(1:1200)]
GBM.cv.function2(sift_orb.score.1200,as.vector(sift_features_add1_label$label),3,5) #0.2865

sift_orb.score.1400 <- as.matrix(sift_features_add1) %*% sift_orb.pca$rotation[,c(1:1400)]
GBM.cv.function2(sift_orb.score.1400,as.vector(sift_features_add1_label$label),3,5) #0.2725

sift_orb.score.1500 <- as.matrix(sift_features_add1) %*% sift_orb.pca$rotation[,c(1:1500)]
GBM.cv.function2(sift_orb.score.1500,as.vector(sift_features_add1_label$label),3,5) #0.282

sift_orb.score.1700 <- as.matrix(sift_features_add1) %*% sift_orb.pca$rotation[,c(1:1700)]
GBM.cv.function2(sift_orb.score.1700,as.vector(sift_features_add1_label$label),3,5) #0.282

sift_orb.score.1800 <- as.matrix(sift_features_add1) %*% sift_orb.pca$rotation[,c(1:1800)]
GBM.cv.function2(sift_orb.score.1800,as.vector(sift_features_add1_label$label),3,5) #0.276

sift_orb.score.2000 <- as.matrix(sift_features_add1) %*% sift_orb.pca$rotation[,c(1:2000)]
GBM.cv.function2(sift_orb.score.2000,as.vector(sift_features_add1_label$label),3,5) #0.2885

#total variables
GBM.cv.function2(sift_features_add1,as.vector(sift_features_add1_label$label),3,5) #0.2655


