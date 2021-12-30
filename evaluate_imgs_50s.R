library(sf)
library(raster)
library(mapview)



# directory 
dirP <- "/Volumes/GoogleDrive/My Drive/research/projects/utica/50s_valid_out"

# predictions
treesPredict <- list()
for(i in 1:20){
  treesPredict[[i]] <- raster(paste0(dirP,"/trees/tree_predict_",i,".tif"))
}

buildPredict <- list()
for(i in 1:20){
  buildPredict[[i]] <- raster(paste0(dirP,"/building/building_predict_",i,".tif"))
}

pavePredict <- list()
for(i in 1:20){
  pavePredict[[i]] <- raster(paste0(dirP,"/pavement/pavement_predict_",i,".tif"))
}

# masks

treesMask <- list()
for(i in 1:20){
treesMask[[i]] <- raster(paste0(dirP,"/orig/tree/tree_mask_",i,".tif"))
}

buildMask <- list()
for(i in 1:20){
  buildMask[[i]] <- raster(paste0(dirP,"/orig/building/building_mask_",i,".tif"))
}

paveMask <- list()
for(i in 1:20){
  paveMask[[i]] <- raster(paste0(dirP,"/orig/pavement/pavement_mask_",i,".tif"))
}

img50 <- list()

for(i in 1:20){
  img50[[i]] <- raster(paste0(dirP,"/orig/image/image_",i,".tif"))
}


sampStack <- list()

for(i in 1:20){
  sampStack[[i]] <- stack(treesPredict[[i]],
                          buildPredict[[i]],
                          pavePredict[[i]],
                          treesMask[[i]],
                          buildMask[[i]],
                          paveMask[[i]],
                          img50[[i]]
                          )
}

plot(treesMask[[1]])
plot(buildMask[[1]])
plot(treesPredict[[1]])
plot(pavePredict[[1]])
plot(buildPredict[[1]])


plot(treesMask[[2]])
plot(buildMask[[2]])
plot(treesPredict[[2]])
plot(pavePredict[[2]])
plot(buildPredict[[2]])


plot(sampStack[[1]], col=gray(1:100/100))
plot(sampStack[[2]], col=gray(1:100/100))

plot(sampStack[[3]], col=gray(1:100/100))
plot(sampStack[[4]], col=gray(1:100/100))
plot(sampStack[[5]], col=gray(1:100/100))
plot(sampStack[[6]], col=gray(1:100/100))
plot(sampStack[[7]], col=gray(1:100/100))
plot(sampStack[[8]], col=gray(1:100/100))
plot(sampStack[[9]], col=gray(1:100/100))

ThresM <- function(x,tr){
  ifelse(x <= tr, 0, 1)
}

probThresh <- stack(calc(buildPredict[[2]],function(x){ifelse(x <= 0.1, 0, 1)}),
                calc(buildPredict[[2]],function(x){ifelse(x <= 0.2, 0, 1)}),
                calc(buildPredict[[2]],function(x){ifelse(x <= 0.3, 0, 1)}),
                calc(buildPredict[[2]],function(x){ifelse(x <= 0.4, 0, 1)}),
                calc(buildPredict[[2]],function(x){ifelse(x <= 0.5, 0, 1)}),
                calc(buildPredict[[2]],function(x){ifelse(x <= 0.6, 0, 1)}),
                calc(buildPredict[[2]],function(x){ifelse(x <= 0.7, 0, 1)}),
                calc(buildPredict[[2]],function(x){ifelse(x <= 0.8, 0, 1)}),
                calc(buildPredict[[2]],function(x){ifelse(x <= 0.9, 0, 1)}),
                buildMask[[2]])
plot(probThresh)

probThresh <- stack(calc(buildPredict[[3]],function(x){ifelse(x <= 0.1, 0, 1)}),
                    calc(buildPredict[[3]],function(x){ifelse(x <= 0.2, 0, 1)}),
                    calc(buildPredict[[3]],function(x){ifelse(x <= 0.3, 0, 1)}),
                    calc(buildPredict[[3]],function(x){ifelse(x <= 0.4, 0, 1)}),
                    calc(buildPredict[[3]],function(x){ifelse(x <= 0.5, 0, 1)}),
                    calc(buildPredict[[3]],function(x){ifelse(x <= 0.6, 0, 1)}),
                    calc(buildPredict[[3]],function(x){ifelse(x <= 0.7, 0, 1)}),
                    calc(buildPredict[[3]],function(x){ifelse(x <= 0.8, 0, 1)}),
                    calc(buildPredict[[3]],function(x){ifelse(x <= 0.9, 0, 1)}),
                    buildMask[[3]])
plot(probThresh)


probThresh <- stack(calc(buildPredict[[9]],function(x){ifelse(x <= 0.1, 0, 1)}),
                    calc(buildPredict[[9]],function(x){ifelse(x <= 0.2, 0, 1)}),
                    calc(buildPredict[[9]],function(x){ifelse(x <= 0.3, 0, 1)}),
                    calc(buildPredict[[9]],function(x){ifelse(x <= 0.4, 0, 1)}),
                    calc(buildPredict[[9]],function(x){ifelse(x <= 0.5, 0, 1)}),
                    calc(buildPredict[[9]],function(x){ifelse(x <= 0.6, 0, 1)}),
                    calc(buildPredict[[9]],function(x){ifelse(x <= 0.7, 0, 1)}),
                    calc(buildPredict[[9]],function(x){ifelse(x <= 0.8, 0, 1)}),
                    calc(buildPredict[[9]],function(x){ifelse(x <= 0.9, 0, 1)}),
                    buildMask[[9]])
plot(probThresh)
