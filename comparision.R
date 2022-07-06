library(raster)
library(sf)
library(mapview)
library(caret)

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
treeCalc <- freq(treeOverlap)

treeCalc[3,2]/(treeCalc[2,2]+treeCalc[3,2])

# confusion matrix

validClass <-  (treesMask[[1]]*1) + (buildMask[[1]]*2) + (paveMask[[1]]*3)
plot(validClass)

predictCrop <- crop(map50_256, treesMask[[1]])
predictClass <- resample(predictCrop, validClass, method="ngb")

testC <- confusionMatrix(as.factor(getValues(validClass)),as.factor(getValues(predictClass)))

testC$table + testC$table
