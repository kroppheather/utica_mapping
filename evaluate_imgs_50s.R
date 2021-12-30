library(sf)
library(raster)
library(mapview)



# directory 
dirP <- "/Volumes/GoogleDrive/My Drive/research/projects/utica/50s_valid_out"

# number of validation images
nValid <- 20

# predictions
treesPredict <- list()
for(i in 1:nValid){
  treesPredict[[i]] <- raster(paste0(dirP,"/trees/tree_predict_",i,".tif"))
}

buildPredict <- list()
for(i in 1:nValid){
  buildPredict[[i]] <- raster(paste0(dirP,"/building/building_predict_",i,".tif"))
}

pavePredict <- list()
for(i in 1:nValid){
  pavePredict[[i]] <- raster(paste0(dirP,"/pavement/pavement_predict_",i,".tif"))
}

# masks

treesMask <- list()
for(i in 1:nValid){
treesMask[[i]] <- raster(paste0(dirP,"/orig/tree/tree_mask_",i,".tif"))
}

buildMask <- list()
for(i in 1:nValid){
  buildMask[[i]] <- raster(paste0(dirP,"/orig/building/building_mask_",i,".tif"))
}

paveMask <- list()
for(i in 1:nValid){
  paveMask[[i]] <- raster(paste0(dirP,"/orig/pavement/pavement_mask_",i,".tif"))
}

img50 <- list()

for(i in 1:nValid){
  img50[[i]] <- raster(paste0(dirP,"/orig/image/image_",i,".tif"))
}


sampStack <- list()

for(i in 1:nValid){
  sampStack[[i]] <- stack(treesPredict[[i]],
                          buildPredict[[i]],
                          pavePredict[[i]],
                          treesMask[[i]],
                          buildMask[[i]],
                          paveMask[[i]],
                          img50[[i]]
                          )
}




# check influence of threshold decision with IOU and accuracy

# make class under incremental thresholds
buildThresh <- list()

for(i in 1:nValid){
  buildThresh[[i]] <- stack(calc(buildPredict[[i]],function(x){ifelse(x <= 0.1, 0, 1)}),
                      calc(buildPredict[[i]],function(x){ifelse(x <= 0.2, 0, 1)}),
                      calc(buildPredict[[i]],function(x){ifelse(x <= 0.3, 0, 1)}),
                      calc(buildPredict[[i]],function(x){ifelse(x <= 0.4, 0, 1)}),
                      calc(buildPredict[[i]],function(x){ifelse(x <= 0.5, 0, 1)}),
                      calc(buildPredict[[i]],function(x){ifelse(x <= 0.6, 0, 1)}),
                      calc(buildPredict[[i]],function(x){ifelse(x <= 0.7, 0, 1)}),
                      calc(buildPredict[[i]],function(x){ifelse(x <= 0.8, 0, 1)}),
                      calc(buildPredict[[i]],function(x){ifelse(x <= 0.9, 0, 1)}))
  
}

#calculate IOU and accuracy for each
plot(buildThresh[[1]][[1]])

buildTot <- list()

for(i in 1:nValid){

    buildTot[[i]] <- buildThresh[[i]]+buildMask[[i]]
}

buildAssess <- list()
for(i in 1:nValid){
  buildAssess[[i]] <- freq(buildTot[[i]])
}  

#calculate IOU using pixels as area (1 pixel)
# 1+ 2 are union and 2 are intersection
buildAssess[[1]][[1]][3,2]/(buildAssess[[1]][[1]][2,2]+buildAssess[[1]][[1]][3,2])
threshSeq <- seq(0.1,0.9,by=0.1)
buildIOU <- list()
buildDF<- data.frame()
for(i in 1:nValid){
  if(nrow(buildAssess[[i]][[1]]) == 3){
    buildDF <- data.frame(thresh=threshSeq[1],
                        IOU = buildAssess[[i]][[1]][3,2]/(buildAssess[[i]][[1]][2,2]+buildAssess[[i]][[1]][3,2]))
  }else{ buildDF <- data.frame(thresh=threshSeq[1],
                              IOU = 0)}
  for(j in 2:9){
    if(nrow(buildAssess[[i]][[j]]) == 3){  
      buildDF <- rbind(buildDF,data.frame(thresh=threshSeq[j],
                        IOU = buildAssess[[i]][[j]][3,2]/(buildAssess[[i]][[j]][2,2]+buildAssess[[i]][[j]][3,2])))
    }else{ buildDF <- rbind(buildDF,data.frame(thresh=threshSeq[j],
                                 IOU = 0))}
  }
  buildIOU[[i]] <- buildDF
}


plot(buildThresh[[1]])


plot(sampStack[[1]], col=gray(1:100/100))
plot(sampStack[[2]], col=gray(1:100/100))
plot(sampStack[[3]], col=gray(1:100/100))
plot(sampStack[[4]], col=gray(1:100/100))
plot(sampStack[[5]], col=gray(1:100/100))
plot(sampStack[[6]], col=gray(1:100/100))
plot(sampStack[[7]], col=gray(1:100/100))
plot(sampStack[[8]], col=gray(1:100/100))
plot(sampStack[[9]], col=gray(1:100/100))
