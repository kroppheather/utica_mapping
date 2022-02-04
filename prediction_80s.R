library(raster)
library(sf)
library(mapview)

dirP <- "/Volumes/GoogleDrive/My Drive/research/projects/utica/80s_predict_out"
dirI <- "/Volumes/GoogleDrive/My Drive/research/projects/utica/predict80"


treeImg <- list()

Nimg <- 627

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



treeMap <- calc(treeAll,function(x){ifelse(x <= 0.3, 0, 1)})
buildMap <- calc(buildAll,function(x){ifelse(x <= 0.1, 0, 1)})
paveMap <- calc(paveAll,function(x){ifelse(x <= 0.2, 0, 1)})



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
     legend=FALSE,  axes=FALSE, box=FALSE)
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
