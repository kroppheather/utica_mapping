library(sf)
library(raster)
library(mapview)
library(ggplot2)
library(dplyr)


# directory 
dirP <- "E:/Google Drive/research/projects/utica/model_save/2017/prediction_strat/valid"
#directory for orig img
dirI <- "E:/Google Drive/research/projects/utica/model_save/2017/data/valid_out"

# number of validation images
nValid <- 20

# predictions
treesPredict <- list()
for(i in 1:nValid){
  treesPredict[[i]] <- raster(paste0(dirP,"/tree/tree_predict_",i,".tif"))
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
treesMask[[i]] <- raster(paste0(dirI,"/trees/tree_mask_",i,".tif"))
}

buildMask <- list()
for(i in 1:nValid){
  buildMask[[i]] <- raster(paste0(dirI,"/building/build_mask_",i,".tif"))
}

paveMask <- list()
for(i in 1:nValid){
  paveMask[[i]] <- raster(paste0(dirI,"/pavement/pave_mask_",i,".tif"))
}

img50 <- list()

for(i in 1:nValid){
  img50[[i]] <- stack(paste0("E:/Google Drive/research/projects/utica/model_save/2017/data/valid/images/valid_",i,".tif"))
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
treesThresh <- list()
paveThresh <- list()


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

  treesThresh[[i]] <- stack(calc(treesPredict[[i]],function(x){ifelse(x <= 0.1, 0, 1)}),
                            calc(treesPredict[[i]],function(x){ifelse(x <= 0.2, 0, 1)}),
                            calc(treesPredict[[i]],function(x){ifelse(x <= 0.3, 0, 1)}),
                            calc(treesPredict[[i]],function(x){ifelse(x <= 0.4, 0, 1)}),
                            calc(treesPredict[[i]],function(x){ifelse(x <= 0.5, 0, 1)}),
                            calc(treesPredict[[i]],function(x){ifelse(x <= 0.6, 0, 1)}),
                            calc(treesPredict[[i]],function(x){ifelse(x <= 0.7, 0, 1)}),
                            calc(treesPredict[[i]],function(x){ifelse(x <= 0.8, 0, 1)}),
                            calc(treesPredict[[i]],function(x){ifelse(x <= 0.9, 0, 1)}))

  paveThresh[[i]] <- stack(calc(pavePredict[[i]],function(x){ifelse(x <= 0.1, 0, 1)}),
                           calc(pavePredict[[i]],function(x){ifelse(x <= 0.2, 0, 1)}),
                           calc(pavePredict[[i]],function(x){ifelse(x <= 0.3, 0, 1)}),
                           calc(pavePredict[[i]],function(x){ifelse(x <= 0.4, 0, 1)}),
                           calc(pavePredict[[i]],function(x){ifelse(x <= 0.5, 0, 1)}),
                           calc(pavePredict[[i]],function(x){ifelse(x <= 0.6, 0, 1)}),
                           calc(pavePredict[[i]],function(x){ifelse(x <= 0.7, 0, 1)}),
                           calc(pavePredict[[i]],function(x){ifelse(x <= 0.8, 0, 1)}),
                           calc(pavePredict[[i]],function(x){ifelse(x <= 0.9, 0, 1)}))    
}

#calculate IOU and accuracy for each
plot(buildThresh[[1]][[1]])

buildTot <- list()
treesTot <- list()
paveTot <- list()

for(i in 1:nValid){

    buildTot[[i]] <- buildThresh[[i]]+buildMask[[i]]
    treesTot[[i]] <- treesThresh[[i]]+treesMask[[i]]
    paveTot[[i]] <- paveThresh[[i]]+paveMask[[i]]
}

buildAssess <- list()
treesAssess <- list()
paveAssess <- list()
for(i in 1:nValid){
  buildAssess[[i]] <- freq(buildTot[[i]])
  treesAssess[[i]] <- freq(treesTot[[i]])
  paveAssess[[i]] <- freq(paveTot[[i]])
}  

#calculate IOU using pixels as area (1 pixel)
# 1+ 2 are union and 2 are intersection

threshSeq <- seq(0.1,0.9,by=0.1)


buildIOU <- list()
buildDF<- data.frame()

treesIOU <- list()
treesDF <- list()

paveIOU <- list()
treesDF <- list()

for(i in 1:nValid){
  if(nrow(buildAssess[[i]][[1]]) == 3){
    buildDF <- data.frame(thresh=threshSeq[1],
                        IOU = buildAssess[[i]][[1]][3,2]/(buildAssess[[i]][[1]][2,2]+buildAssess[[i]][[1]][3,2]),
                          imgN = i)
  }else{ buildDF <- data.frame(thresh=threshSeq[1],
                              IOU = 0,
                              imgN = i)}
  for(j in 2:9){
    if(nrow(buildAssess[[i]][[j]]) == 3){  
      buildDF <- rbind(buildDF,data.frame(thresh=threshSeq[j],
                        IOU = buildAssess[[i]][[j]][3,2]/(buildAssess[[i]][[j]][2,2]+buildAssess[[i]][[j]][3,2]),
                        imgN=i))
    }else{ buildDF <- rbind(buildDF,data.frame(thresh=threshSeq[j],
                                 IOU = 0,
                                 imgN=i))}
  }
  buildIOU[[i]] <- buildDF
}



for(i in 1:nValid){
  if(nrow(treesAssess[[i]][[1]]) == 3){
    treesDF <- data.frame(thresh=threshSeq[1],
                          IOU = treesAssess[[i]][[1]][3,2]/(treesAssess[[i]][[1]][2,2]+treesAssess[[i]][[1]][3,2]),
                          imgN = i)
  }else{ treesDF <- data.frame(thresh=threshSeq[1],
                               IOU = 0,
                               imgN = i)}
  for(j in 2:9){
    if(nrow(treesAssess[[i]][[j]]) == 3){  
      treesDF <- rbind(treesDF,data.frame(thresh=threshSeq[j],
                                          IOU = treesAssess[[i]][[j]][3,2]/(treesAssess[[i]][[j]][2,2]+treesAssess[[i]][[j]][3,2]),
                                          imgN=i))
    }else{ treesDF <- rbind(treesDF,data.frame(thresh=threshSeq[j],
                                               IOU = 0,
                                               imgN=i))}
  }
  treesIOU[[i]] <- treesDF
}


for(i in 1:nValid){
  if(nrow(paveAssess[[i]][[1]]) == 3){
    paveDF <- data.frame(thresh=threshSeq[1],
                          IOU = paveAssess[[i]][[1]][3,2]/(paveAssess[[i]][[1]][2,2]+paveAssess[[i]][[1]][3,2]),
                          imgN = i)
  }else{ paveDF <- data.frame(thresh=threshSeq[1],
                               IOU = 0,
                               imgN = i)}
  for(j in 2:9){
    if(nrow(paveAssess[[i]][[j]]) == 3){  
      paveDF <- rbind(paveDF,data.frame(thresh=threshSeq[j],
                                          IOU = paveAssess[[i]][[j]][3,2]/(paveAssess[[i]][[j]][2,2]+paveAssess[[i]][[j]][3,2]),
                                          imgN=i))
    }else{ paveDF <- rbind(treesDF,data.frame(thresh=threshSeq[j],
                                               IOU = 0,
                                               imgN=i))}
  }
  paveIOU[[i]] <- paveDF
}

IOUbuild <- do.call("rbind",buildIOU)
IOUtrees <- do.call("rbind", treesIOU)
IOUpave <- do.call("rbind", paveIOU)

#calculate IOU across all images
buildTotDF <- list()
tempDF <- data.frame()
for(i in 1:nValid){
  tempDF <- data.frame(buildAssess[[i]][[1]])
  tempDF$thresh <- rep(0.1,nrow(tempDF))
  for(j in 2:9){
    tempDF <- rbind(tempDF,data.frame(buildAssess[[i]][[j]],
                                      thresh=rep(j/10,nrow(data.frame(buildAssess[[i]][[j]])))))
  }  
  buildTotDF[[i]] <- tempDF
}  


treesTotDF <- list()
tempDFt <- data.frame()
for(i in 1:nValid){
  tempDFt <- data.frame(treesAssess[[i]][[1]])
  tempDFt$thresh <- rep(0.1,nrow(tempDFt))
  for(j in 2:9){
    tempDFt <- rbind(tempDFt,data.frame(treesAssess[[i]][[j]],
                                      thresh=rep(j/10,nrow(data.frame(treesAssess[[i]][[j]])))))
  }  
  treesTotDF[[i]] <- tempDFt
}  

paveTotDF <- list()
tempDFp <- data.frame()
for(i in 1:nValid){
  tempDFp <- data.frame(paveAssess[[i]][[1]])
  tempDFp$thresh <- rep(0.1,nrow(tempDFp))
  for(j in 2:9){
    tempDFp <- rbind(tempDFp,data.frame(paveAssess[[i]][[j]],
                                        thresh=rep(j/10,nrow(data.frame(paveAssess[[i]][[j]])))))
  }  
  paveTotDF[[i]] <- tempDFp
}  


buildSumDF <- do.call("rbind",buildTotDF)
treesSumDF <- do.call("rbind",treesTotDF)
paveSumDF <- do.call("rbind",paveTotDF)


IOUcalc <- function(x,y){
  sum(x[y == 2])/ (sum(x[y == 2])+ sum(x[y == 1]))
}


allIOUbuild <- buildSumDF %>%
  group_by(thresh) %>%
  summarize(IOU=IOUcalc(count,value))

allIOUtrees <- treesSumDF %>%
  group_by(thresh) %>%
  summarize(IOU=IOUcalc(count,value))


allIOUpave <- paveSumDF %>%
  group_by(thresh) %>%
  summarize(IOU=IOUcalc(count,value))

allIOUpave$type <- rep("pave", nrow(allIOUpave))
allIOUtrees$type <- rep("trees", nrow(allIOUtrees))
allIOUbuild$type <- rep("build", nrow(allIOUbuild))

IOU50 <- rbind(allIOUpave,allIOUtrees,allIOUbuild)

#plot IOU over the different thresholds

ggplot(data=IOUbuild, aes(x=thresh,y=IOU,color=imgN))+
  geom_point()+
  geom_path()


ggplot(data=IOUpave, aes(x=thresh,y=IOU,color=imgN))+
  geom_point()+
  geom_path()

ggplot(data=IOUtrees, aes(x=thresh,y=IOU,color=imgN))+
  geom_point()+
  geom_path()

ggplot(data=IOU50, aes(x=thresh,y=IOU,color=type))+
  geom_point()+
  geom_path()
#calculate threshold with hightest IOU
