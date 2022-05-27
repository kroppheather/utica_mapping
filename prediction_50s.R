library(raster)
library(sf)
library(mapview)


# Image merge ---------

dirP <- "E:/Google Drive/research/projects/utica/model_save/1950/prediction_256/image"
dirI <- "E:/Google Drive/research/projects/utica/model_save/1950/img_tile256/predict50"


treeImg <- list()

Nimg <- 435

for(i in 1:Nimg){
  treeImg[[i]] <- raster(paste0(dirP,"/tree/tree_",i,".tif"))
  
  
}

treeAll <- do.call(merge, treeImg)



paveImg <- list()

for(i in 1:Nimg){
  paveImg[[i]] <- raster(paste0(dirP,"/pavement/pavement_",i,".tif"))
  
  
}

paveAll <- do.call(merge, paveImg)


buildImg <- list()
for(i in 1:Nimg){
  buildImg[[i]] <- raster(paste0(dirP,"/building/building_",i,".tif"))
  
  
}

buildAll <- do.call(merge, buildImg)

origImg <- list()
for(i in 1:Nimg){
  origImg[[i]] <- raster(paste0(dirI,"/predict_",i,".tif"))
  
  
}

origAll <- do.call(merge, origImg)



# Offset 2 merge ------------------


dirP2 <- "E:/Google Drive/research/projects/utica/model_save/1950/prediction_256/image_2"
dirI2 <- "E:/Google Drive/research/projects/utica/model_save/1950/img_tile256/predict50_2"


treeImg2 <- list()

Nimg2 <- 435

for(i in 1:Nimg2){
  treeImg2[[i]] <- raster(paste0(dirP2,"/tree/tree_",i,".tif"))
  
  
}

treeAll2 <- do.call(merge, treeImg2)



paveImg2 <- list()

for(i in 1:Nimg2){
  paveImg2[[i]] <- raster(paste0(dirP2,"/pavement/pavement_",i,".tif"))
  
  
}

paveAll2 <- do.call(merge, paveImg2)


buildImg2 <- list()
for(i in 1:Nimg){
  buildImg2[[i]] <- raster(paste0(dirP2,"/building/building_",i,".tif"))
  
  
}

buildAll2 <- do.call(merge, buildImg2)

origImg2 <- list()
for(i in 1:Nimg2){
  origImg2[[i]] <- raster(paste0(dirI2,"/predict_",i,".tif"))
  
  
}

origAll2 <- do.call(merge, origImg2)




# Offset 3 merge -----------------


dirP3 <- "E:/Google Drive/research/projects/utica/model_save/1950/prediction_256/image_3"
dirI3 <- "E:/Google Drive/research/projects/utica/model_save/1950/img_tile256/predict50_3"


treeImg3 <- list()

Nimg3 <- 392

for(i in 1:Nimg3){
  treeImg3[[i]] <- raster(paste0(dirP3,"/tree/tree_",i,".tif"))
  
  
}

treeAll3 <- do.call(merge, treeImg3)



paveImg3 <- list()

for(i in 1:Nimg3){
  paveImg3[[i]] <- raster(paste0(dirP3,"/pavement/pavement_",i,".tif"))
  
  
}

paveAll3 <- do.call(merge, paveImg3)


buildImg3 <- list()
for(i in 1:Nimg){
  buildImg3[[i]] <- raster(paste0(dirP3,"/building/building_",i,".tif"))
  
  
}

buildAll3 <- do.call(merge, buildImg3)

origImg3 <- list()
for(i in 1:Nimg3){
  origImg3[[i]] <- raster(paste0(dirI3,"/predict_",i,".tif"))
  
  
}

origAll3 <- do.call(merge, origImg3)



# Combine overlays -------------

#match offsets to original

treeAll2rs <- resample(treeAll2, treeAll)
treeAll3rs <- resample(treeAll3, treeAll)


treeCombine <- stack(treeAll, treeAll2rs, treeAll3rs)


treeLayer <- calc(treeCombine, function(x){max(x, na.rm=TRUE)})



# check out weird line artifacts:

plot(treeImg[[300]])
plot(treeImg2[[300]], add=TRUE)
plot(treeImg2[[301]], add=TRUE)

plot(treeImg[[301]], add=TRUE)
plot(treeImg[[299]], add=TRUE)
plot(treeImg2[[299]], add=TRUE)

plot(origImg[[300]], col=grey(1:100/100))
plot(origImg2[[300]], col=grey(1:100/100),add=TRUE)


plot(origImg[[301]], col=grey(1:100/100),add=TRUE)
plot(origImg[[299]], col=grey(1:100/100),add=TRUE)

treeMap <- calc(treeAll,function(x){ifelse(x <= 0.3, 0, 1)})
buildMap <- calc(buildAll,function(x){ifelse(x <= 0.15, 0, 1)})
paveMap <- calc(paveAll,function(x){ifelse(x <= 0.15, 0, 1)})



plot(treeMap)
plot(origAll, col=gray(1:100/100))
plot(buildMap)
plot(paveMap)

treeCol <- rgb(0.13,0.54,0.13,0.5)
paveCol <- rgb(0.5,0.5,0.5,0.5)
buildCol <- rgb(0.53,0.17,0.09,0.5)

par(mfrow=c(1,2))
par(mai=c(0,0,0,0))
plot(treeMap, col=c(NA,treeCol), breaks=c(0,0.5,1.5),
     legend=FALSE, axes=FALSE, box=FALSE)
plot(paveMap, col=c(NA,paveCol), breaks=c(0,0.5,1.5),
     legend=FALSE, add=TRUE, axes=FALSE, box=FALSE)
plot(buildMap, col=c(NA,buildCol), breaks=c(0,0.5,1.5),
     legend=FALSE, add=TRUE, axes=FALSE, box=FALSE)
par(mai=c(0,0,0,0))
plot(origAll, col=gray(1:100/100),
     legend=FALSE, axes=FALSE, box=FALSE)


mapview(treeMap)
mapview(buildMap)

help("plot")

cropE <- extent(-75.24,-75.23,43.105,43.107)

treeC <- crop(treeMap, cropE)  
buildC <- crop(buildMap, cropE)  
paveC <- crop(paveMap, cropE)  
origC <- crop(origAll, cropE) 

par(mfrow=c(1,2))
par(mai=c(0,0,0,0))

plot(treeC, col=c(NA,treeCol), breaks=c(0,0.5,1.5),
     legend=FALSE, axes=FALSE, box=FALSE)
plot(paveC, col=c(NA,paveCol), breaks=c(0,0.5,1.5),
     legend=FALSE, add=TRUE, axes=FALSE, box=FALSE)
plot(buildC, col=c(NA,buildCol), breaks=c(0,0.5,1.5),
     legend=FALSE, add=TRUE, axes=FALSE, box=FALSE)
par(mai=c(0,0,0,0))
plot(origC, col=gray(1:100/100),
     legend=FALSE, axes=FALSE, box=FALSE)


cropE <- extent(-75.24,-75.23,43.103,43.109)

treeC <- crop(treeMap, cropE)  
buildC <- crop(buildMap, cropE)  
paveC <- crop(paveMap, cropE)  
origC <- crop(origAll, cropE) 

par(mfrow=c(1,2))
par(mai=c(0,0,0,0))

plot(treeC, col=c(NA,treeCol), breaks=c(0,0.5,1.5),
     legend=FALSE, axes=FALSE, box=FALSE)
plot(paveC, col=c(NA,paveCol), breaks=c(0,0.5,1.5),
     legend=FALSE, add=TRUE, axes=FALSE, box=FALSE)
plot(buildC, col=c(NA,buildCol), breaks=c(0,0.5,1.5),
     legend=FALSE, add=TRUE, axes=FALSE, box=FALSE)
par(mai=c(0,0,0,0))
plot(origC, col=gray(1:100/100),
     legend=FALSE, axes=FALSE, box=FALSE)



cropE <- extent(-75.26,-75.25,43.080,43.085)

treeC <- crop(treeMap, cropE)  
buildC <- crop(buildMap, cropE)  
paveC <- crop(paveMap, cropE)  
origC <- crop(origAll, cropE) 

par(mfrow=c(1,2))
par(mai=c(0,0,0,0))

plot(treeC, col=c(NA,treeCol), breaks=c(0,0.5,1.5),
     legend=FALSE, axes=FALSE, box=FALSE)
plot(paveC, col=c(NA,paveCol), breaks=c(0,0.5,1.5),
     legend=FALSE, add=TRUE, axes=FALSE, box=FALSE)
plot(buildC, col=c(NA,buildCol), breaks=c(0,0.5,1.5),
     legend=FALSE, add=TRUE, axes=FALSE, box=FALSE)
par(mai=c(0,0,0,0))
plot(origC, col=gray(1:100/100),
     legend=FALSE, axes=FALSE, box=FALSE)


cropE <- extent(-75.28,-75.27,43.082,43.087)

treeC <- crop(treeMap, cropE)  
buildC <- crop(buildMap, cropE)  
paveC <- crop(paveMap, cropE)  
origC <- crop(origAll, cropE) 

par(mfrow=c(1,2))
par(mai=c(0,0,0,0))

plot(treeC, col=c(NA,treeCol), breaks=c(0,0.5,1.5),
     legend=FALSE, axes=FALSE, box=FALSE)
plot(paveC, col=c(NA,paveCol), breaks=c(0,0.5,1.5),
     legend=FALSE, add=TRUE, axes=FALSE, box=FALSE)
plot(buildC, col=c(NA,buildCol), breaks=c(0,0.5,1.5),
     legend=FALSE, add=TRUE, axes=FALSE, box=FALSE)
par(mai=c(0,0,0,0))
plot(origC, col=gray(1:100/100),
     legend=FALSE, axes=FALSE, box=FALSE)


cropE <- extent(-75.29,-75.27,43.095,43.105)

treeC <- crop(treeMap, cropE)  
buildC <- crop(buildMap, cropE)  
paveC <- crop(paveMap, cropE)  
origC <- crop(origAll, cropE) 

par(mfrow=c(1,2))
par(mai=c(0,0,0,0))

plot(treeC, col=c(NA,treeCol), breaks=c(0,0.5,1.5),
     legend=FALSE, axes=FALSE, box=FALSE)
plot(paveC, col=c(NA,paveCol), breaks=c(0,0.5,1.5),
     legend=FALSE, add=TRUE, axes=FALSE, box=FALSE)
plot(buildC, col=c(NA,buildCol), breaks=c(0,0.5,1.5),
     legend=FALSE, add=TRUE, axes=FALSE, box=FALSE)
par(mai=c(0,0,0,0))
plot(origC, col=gray(1:100/100),
     legend=FALSE, axes=FALSE, box=FALSE)


plot(merge(treeImg[[35]],treeImg[[36]],treeImg[[37]],treeImg[[38]]))
