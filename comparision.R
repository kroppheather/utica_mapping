library(raster)
library(sf)
library(mapview)
library(caret)
library(dplyr)

##### Overview ----

# all maps:
# 0: other
# 1 = tree
# 2 = building
# 3 = pavement

###### 1950s read data----

# read in final maps
map50_256 <- raster("E:/Google Drive/research/projects/utica/model_save/1950/all_maps/utica50s_256.tif")
map50_kern <- raster("E:/Google Drive/research/projects/utica/model_save/1950/all_maps/utica50s_kernal.tif")
map50_128 <- raster("E:/Google Drive/research/projects/utica/model_save/1950/all_maps/utica50s_128.tif")

# original
r50s <- raster("e:/Google Drive/research/projects/utica/A550500171317_ref.tif")


#look at a few areas near the city center to start


Ucenter2 <- extent(-8382000,-8373500,
                   5324000,5329000)
#start with working with a small area in the center of
#utica 
u50a <- crop(r50s,Ucenter2)


###### 1950s comparison map----

treeCol1 <- rgb(0.13,0.54,0.13)
paveCol1 <- "grey30"
buildCol1 <- "tomato4"

png("E:/Google Drive/research/projects/utica/model_save/1950/all_maps/maps/comp_50.png", width=15000,height=11000)

par(mfrow=c(2,2))

plot(map50_256, axes=FALSE, legend=FALSE, box=FALSE, 
     breaks=c(-0.1,0.5,#breaks between other
              1.5, # tree
              2.5, # building
              3.5), #pavement
     maxpixels=7424*3840 ,
     col=c(NA, treeCol1,buildCol1, paveCol1))
mtext("Training n=60: 256 x 256", side=3, cex=20, line=-50)

plot(map50_kern, axes=FALSE, legend=FALSE, box=FALSE,
     breaks=c(-0.1,0.5,#breaks between other
              1.5, # tree
              2.5, # building
              3.5 ), #pavement
     col=c(NA, treeCol1,buildCol1, paveCol1),
    maxpixels=7424*3840
     )
mtext("Kernal size = 5", side=3, cex=20, line=-50)
plot(map50_128, axes=FALSE, legend=FALSE, box=FALSE, 
     breaks=c(-0.1,0.5,#breaks between other
              1.5, # tree
              2.5, # building
              3.5 ), #pavement
     col=c(NA, treeCol1,buildCol1, paveCol1),
     maxpixels=7424*3840
     )
mtext("Training n=120: 128 x 128", side=3, cex=20, line=-30)

plot(u50a, col=gray(1:100/100), axes=FALSE, legend=FALSE, box=FALSE, 
     maxpixels=7424*3840
     )
mtext("Original", side=3, cex=20, line=-30)
dev.off()


###### 1950s evaluation----
#directory for orig img
dirI <- "E:/Google Drive/research/projects/utica/model_save/1950/data_256/crop_valid"

nValid <- 20
# masks

treesMask <- list()
for(i in 1:nValid){
  treesMask[[i]] <- raster(paste0(dirI,"/tree/tree_mask_",i,".tif"))
}

buildMask <- list()
for(i in 1:nValid){
  buildMask[[i]] <- raster(paste0(dirI,"/building/building_mask_",i,".tif"))
}

paveMask <- list()
for(i in 1:nValid){
  paveMask[[i]] <- raster(paste0(dirI,"/pavement/pavement_mask_",i,".tif"))
}

img50 <- list()

for(i in 1:nValid){
  img50[[i]] <- raster(paste0(dirI,"/image/image_",i,".tif"))
}




trees_256 <- reclassify(map50_256, matrix(c(0,0,
                                          1,1,
                                          2,0,
                                          3,0), ncol=2, byrow=TRUE))

trees_128 <- reclassify(map50_128, matrix(c(0,0,
                                            1,1,
                                            2,0,
                                            3,0), ncol=2, byrow=TRUE))

trees_kern <- reclassify(map50_kern, matrix(c(0,0,
                                            1,1,
                                            2,0,
                                            3,0), ncol=2, byrow=TRUE))


build_256 <- reclassify(map50_256, matrix(c(0,0,
                                            1,0,
                                            2,1,
                                            3,0), ncol=2, byrow=TRUE))

build_128 <- reclassify(map50_128, matrix(c(0,0,
                                            1,0,
                                            2,1,
                                            3,0), ncol=2, byrow=TRUE))
build_kern <- reclassify(map50_kern, matrix(c(0,0,
                                            1,0,
                                            2,1,
                                            3,0), ncol=2, byrow=TRUE))

pave_256 <- reclassify(map50_256, matrix(c(0,0,
                                            1,0,
                                            2,0,
                                            3,1), ncol=2, byrow=TRUE))

pave_128 <- reclassify(map50_128, matrix(c(0,0,
                                           1,0,
                                           2,0,
                                           3,1), ncol=2, byrow=TRUE))

pave_kern <- reclassify(map50_kern, matrix(c(0,0,
                                           1,0,
                                           2,0,
                                           3,1), ncol=2, byrow=TRUE))


##### IOU calc kern ----
treesCrop_kern <- list()
treesResamp_kern <- list()
buildCrop_kern <- list()
buildResamp_kern <- list()
paveCrop_kern <- list()
paveResamp_kern <- list()

for(i in 1:nValid){
  treesCrop_kern[[i]] <- crop(trees_kern, treesMask[[i]])
  treesResamp_kern[[i]] <- resample(treesCrop_kern[[i]], treesMask[[i]], method="ngb")
  buildCrop_kern[[i]] <- crop(build_kern, buildMask[[i]])
  buildResamp_kern[[i]] <- resample(buildCrop_kern[[i]], buildMask[[i]], method="ngb")
  paveCrop_kern[[i]] <- crop(pave_kern, paveMask[[i]])
  paveResamp_kern[[i]] <- resample(paveCrop_kern[[i]], paveMask[[i]], method="ngb")
  
}






treeOverlap_kern <- list()
buildOverlap_kern <- list()
paveOverlap_kern <- list()

for(i in 1:nValid){
  treeOverlap_kern[[i]] <- treesResamp_kern[[i]] + treesMask[[i]]
  buildOverlap_kern[[i]] <- buildResamp_kern[[i]] + buildMask[[i]]
  paveOverlap_kern[[i]] <- paveResamp_kern[[i]] + paveMask[[i]]
 
}
#IOU
treeCalc_kern <- list()
buildCalc_kern <- list()
paveCalc_kern <- list()
for(i in 1:nValid){
  treeCalc_kern[[i]] <- freq(treeOverlap_kern[[i]])
  buildCalc_kern[[i]] <- freq(buildOverlap_kern[[i]])
  paveCalc_kern[[i]] <- freq(paveOverlap_kern[[i]])
  
}



treeSum_kern <- do.call("rbind", treeCalc_kern)
colnames(treeSum_kern) <- c("overlapID","pix")
treeSum_kern <- data.frame(treeSum_kern)

buildSum_kern <- do.call("rbind", buildCalc_kern)
colnames(buildSum_kern) <- c("overlapID","pix")
buildSum_kern <- data.frame(buildSum_kern)

paveSum_kern <- do.call("rbind", paveCalc_kern)
colnames(paveSum_kern) <- c("overlapID","pix")
paveSum_kern <- data.frame(paveSum_kern)

# some masks on edge of offset image produced that changed original extent
# need to remove NA


totalTreePix_kern <- na.omit(treeSum_kern) %>%
  group_by(overlapID) %>%
  summarize(totalPix = sum(pix))

totalBuildPix_kern <- na.omit(buildSum_kern) %>%
  group_by(overlapID) %>%
  summarize(totalPix = sum(pix))

totalPavePix_kern <- na.omit(paveSum_kern) %>%
  group_by(overlapID) %>%
  summarize(totalPix = sum(pix))



IOU_tree_kern <- totalTreePix_kern[3,2]/(totalTreePix_kern[2,2]+totalTreePix_kern[3,2])
IOU_build_kern <- totalBuildPix_kern[3,2]/(totalBuildPix_kern[2,2]+totalBuildPix_kern[3,2])
IOU_pave_kern <- totalPavePix_kern[3,2]/(totalPavePix_kern[2,2]+totalPavePix_kern[3,2])



##### IOU calc 256 ----

treesCrop_256 <- list()
treesResamp_256 <- list()
buildCrop_256 <- list()
buildResamp_256 <- list()
paveCrop_256 <- list()
paveResamp_256 <- list()

for(i in 1:nValid){
  treesCrop_256[[i]] <- crop(trees_256, treesMask[[i]])
  treesResamp_256[[i]] <- resample(treesCrop_256[[i]], treesMask[[i]], method="ngb")
  buildCrop_256[[i]] <- crop(build_256, buildMask[[i]])
  buildResamp_256[[i]] <- resample(buildCrop_256[[i]], buildMask[[i]], method="ngb")
  paveCrop_256[[i]] <- crop(pave_256, paveMask[[i]])
  paveResamp_256[[i]] <- resample(paveCrop_256[[i]], paveMask[[i]], method="ngb")
  
}






treeOverlap_256 <- list()
buildOverlap_256 <- list()
paveOverlap_256 <- list()

for(i in 1:nValid){
  treeOverlap_256[[i]] <- treesResamp_256[[i]] + treesMask[[i]]
  buildOverlap_256[[i]] <- buildResamp_256[[i]] + buildMask[[i]]
  paveOverlap_256[[i]] <- paveResamp_256[[i]] + paveMask[[i]]
  
}
#IOU
treeCalc_256 <- list()
buildCalc_256 <- list()
paveCalc_256 <- list()
for(i in 1:nValid){
  treeCalc_256[[i]] <- freq(treeOverlap_256[[i]])
  buildCalc_256[[i]] <- freq(buildOverlap_256[[i]])
  paveCalc_256[[i]] <- freq(paveOverlap_256[[i]])
  
}



treeSum_256 <- do.call("rbind", treeCalc_256)
colnames(treeSum_256) <- c("overlapID","pix")
treeSum_256 <- data.frame(treeSum_256)

buildSum_256 <- do.call("rbind", buildCalc_256)
colnames(buildSum_256) <- c("overlapID","pix")
buildSum_256 <- data.frame(buildSum_256)

paveSum_256 <- do.call("rbind", paveCalc_256)
colnames(paveSum_256) <- c("overlapID","pix")
paveSum_256 <- data.frame(paveSum_256)

# some masks on edge of offset image produced that changed original extent
# need to remove NA


totalTreePix_256 <- na.omit(treeSum_256) %>%
  group_by(overlapID) %>%
  summarize(totalPix = sum(pix))

totalBuildPix_256 <- na.omit(buildSum_256) %>%
  group_by(overlapID) %>%
  summarize(totalPix = sum(pix))

totalPavePix_256 <- na.omit(paveSum_256) %>%
  group_by(overlapID) %>%
  summarize(totalPix = sum(pix))



IOU_tree_256 <- totalTreePix_256[3,2]/(totalTreePix_256[2,2]+totalTreePix_256[3,2])
IOU_build_256 <- totalBuildPix_256[3,2]/(totalBuildPix_256[2,2]+totalBuildPix_256[3,2])
IOU_pave_256 <- totalPavePix_256[3,2]/(totalPavePix_256[2,2]+totalPavePix_256[3,2])



##### IOU calc 128 ----

treesCrop_128 <- list()
treesResamp_128 <- list()
buildCrop_128 <- list()
buildResamp_128 <- list()
paveCrop_128 <- list()
paveResamp_128 <- list()

for(i in 1:nValid){
  treesCrop_128[[i]] <- crop(trees_128, treesMask[[i]])
  treesResamp_128[[i]] <- resample(treesCrop_128[[i]], treesMask[[i]], method="ngb")
  buildCrop_128[[i]] <- crop(build_128, buildMask[[i]])
  buildResamp_128[[i]] <- resample(buildCrop_128[[i]], buildMask[[i]], method="ngb")
  paveCrop_128[[i]] <- crop(pave_128, paveMask[[i]])
  paveResamp_128[[i]] <- resample(paveCrop_128[[i]], paveMask[[i]], method="ngb")
  
}






treeOverlap_128 <- list()
buildOverlap_128 <- list()
paveOverlap_128 <- list()

for(i in 1:nValid){
  treeOverlap_128[[i]] <- treesResamp_128[[i]] + treesMask[[i]]
  buildOverlap_128[[i]] <- buildResamp_128[[i]] + buildMask[[i]]
  paveOverlap_128[[i]] <- paveResamp_128[[i]] + paveMask[[i]]
  
}
#IOU
treeCalc_128 <- list()
buildCalc_128 <- list()
paveCalc_128 <- list()
for(i in 1:nValid){
  treeCalc_128[[i]] <- freq(treeOverlap_128[[i]])
  buildCalc_128[[i]] <- freq(buildOverlap_128[[i]])
  paveCalc_128[[i]] <- freq(paveOverlap_128[[i]])
  
}



treeSum_128 <- do.call("rbind", treeCalc_128)
colnames(treeSum_128) <- c("overlapID","pix")
treeSum_128 <- data.frame(treeSum_128)

buildSum_128 <- do.call("rbind", buildCalc_128)
colnames(buildSum_128) <- c("overlapID","pix")
buildSum_128 <- data.frame(buildSum_128)

paveSum_128 <- do.call("rbind", paveCalc_128)
colnames(paveSum_128) <- c("overlapID","pix")
paveSum_128 <- data.frame(paveSum_128)

# some masks on edge of offset image produced that changed original extent
# need to remove NA


totalTreePix_128 <- na.omit(treeSum_128) %>%
  group_by(overlapID) %>%
  summarize(totalPix = sum(pix))

totalBuildPix_128 <- na.omit(buildSum_128) %>%
  group_by(overlapID) %>%
  summarize(totalPix = sum(pix))

totalPavePix_128 <- na.omit(paveSum_128) %>%
  group_by(overlapID) %>%
  summarize(totalPix = sum(pix))



IOU_tree_128 <- totalTreePix_128[3,2]/(totalTreePix_128[2,2]+totalTreePix_128[3,2])
IOU_build_128 <- totalBuildPix_128[3,2]/(totalBuildPix_128[2,2]+totalBuildPix_128[3,2])
IOU_pave_128 <- totalPavePix_128[3,2]/(totalPavePix_128[2,2]+totalPavePix_128[3,2])

##### All IOU----
IOUtable <- data.frame(method = rep(c("256", "kern", "128"), times=3),
                       class = rep(c("tree","build","pave"), each=3),
                       IOU = c(IOU_tree_256,IOU_tree_kern, IOU_tree_128,
                               IOU_build_256,IOU_build_kern, IOU_build_128,
                               IOU_pave_256,IOU_pave_kern, IOU_pave_128))


# confusion matrix
validClass <- list()
predictCrop_256 <- list()
predictClass_256 <- list()
conMat_256 <- list()
confDF_256 <- list()
for(i in 1:20){

validClass[[i]] <-  (treesMask[[i]]*1) + (buildMask[[i]]*2) + (paveMask[[i]]*3)
predictCrop_256[[i]] <- crop(map50_256, treesMask[[i]])
predictClass_256[[i]] <- resample(predictCrop_256[[i]], validClass[[i]], method="ngb")
conMat_256[[i]] <- confusionMatrix(as.factor(getValues(validClass[[i]])),as.factor(getValues(predictClass_256[[i]])))
confDF_256[[i]] <- data.frame(pix = as.vector(conMat[[i]]$table),
                         pred.class = rep(row.names(conMat[[i]]$table), times=4),
                         ref.clas = rep(colnames(conMat[[i]]$table), each=4))
}

conMat_256[[1]]$table
conFcalcDF_256 <- do.call("rbind",confDF_256)
