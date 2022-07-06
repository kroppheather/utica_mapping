library(raster)
library(sf)
library(mapview)


# Image merge ---------

dirP <- "E:/Google Drive/research/projects/utica/model_save/1950/prediction_128/out/image"
dirI <- "E:/Google Drive/research/projects/utica/model_save/1950/prediction_128/image"


treeImg <- list()

Nimg <- 1740

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


dirP2 <- "E:/Google Drive/research/projects/utica/model_save/1950/prediction_128/out/image_2"
dirI2 <- "E:/Google Drive/research/projects/utica/model_save/1950/prediction_128/image_2"


treeImg2 <- list()

Nimg2 <- 1740

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


dirP3 <- "E:/Google Drive/research/projects/utica/model_save/1950/prediction_128/out/image_3"
dirI3 <- "E:/Google Drive/research/projects/utica/model_save/1950/prediction_128/image_3"


treeImg3 <- list()

Nimg3 <- 1653

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
for(i in 1:Nimg3){
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

## Trees  



#resample to original
treeAll2rs <- resample(treeAll2, treeAll)
treeAll3rs <- resample(treeAll3, treeAll)


treeCombine <- stack(treeAll, treeAll2rs, treeAll3rs)

# get maximum prob
treeLayer <- calc(treeCombine, function(x){max(x, na.rm=TRUE)})
plot(treeLayer)


## Buildings

#resample to original
buildAll2rs <- resample(buildAll2, buildAll)
buildAll3rs <- resample(buildAll3, buildAll)

buildCombine <- stack(buildAll,buildAll2rs,buildAll3rs)
buildLayer <- calc(buildCombine, function(x){max(x, na.rm=TRUE)})
plot(buildLayer)


## Pavement
paveAll2rs <- resample(paveAll2, paveAll)
paveAll3rs <- resample(paveAll3, paveAll)

paveCombine <- stack(paveAll, paveAll2rs, paveAll3rs)
paveLayer <- calc(paveCombine, function(x){max(x, na.rm=TRUE)})
plot(paveLayer)


# Make final map cover -------------

# remove noise below set threshold

treeMap <- calc(treeLayer,function(x){ifelse(x <= 0.15, 0, x)})
buildMap <- calc(buildLayer,function(x){ifelse(x <= 0.15, 0, x)})
paveMap <- calc(paveLayer,function(x){ifelse(x <= 0.15, 0, x)})


# binary map of above

treeMapB <- calc(treeLayer,function(x){ifelse(x <= 0.15, 0, 1)})
buildMapB <- calc(buildLayer,function(x){ifelse(x <= 0.15, 0, 1)})
paveMapB <- calc(paveLayer,function(x){ifelse(x <= 0.15, 0, 1)})

binaryStack <- stack(treeMapB,buildMapB,paveMapB)


# need to filter so only one class for each pixel
# take the highest probability

coverStack <- stack(treeMap, buildMap, paveMap)

which.max2 <- function(x){
  max_idx <- which.max(x)   # Get the max
  ifelse(length(max_idx)== 0,return(NA),return(max_idx))
}

classR <- calc(coverStack, which.max2)

#now need to make a rule for determining if the class has a high enough threshold
# need to muliply by binary so turns to zero if too low

treeCalc <- calc(classR, function(x){ifelse(x ==1,1,0)})
treeClass <- treeMapB*treeCalc


buildCalc <- calc(classR, function(x){ifelse(x ==2,1,0)})
buildClass <- buildMapB*buildCalc


paveCalc <- calc(classR, function(x){ifelse(x ==3,1,0)})
paveClass <- paveMapB*paveCalc

plot(treeClass)
plot(paveClass)
plot(buildClass)


buildClass2 <- buildClass*2
paveClass2 <- paveClass*3

# other will be zero, trees =1, buildings =2, pavement =3
uticaClass <- treeClass+buildClass2+paveClass2

plot(uticaClass)

uticaRes <- resample(uticaClass, origAll, method="ngb")


plot(origAll, col=grey(1:100/100))

treeCol1 <- rgb(0.13,0.54,0.13,0.5)
paveCol1 <- rgb(0.96,0.49,0,0.5)
buildCol1 <- rgb(0.53,0.17,0.09,0.5)

treeCol1 <- rgb(0.13,0.54,0.13)
paveCol1 <- rgb(0.96,0.49,0)
buildCol1 <- rgb(0.53,0.17,0.09)

png("E:/Google Drive/research/projects/utica/model_save/1950/prediction_128/maps/utica_classification.png", width=7424,height=3840)

plot(origAll, col=grey(1:100/100), axes=FALSE, legend=FALSE, box=FALSE, maxpixels= 28508160)

plot(uticaRes, breaks=c(-0.1,0.5,#breaks between other
                          1.5, # tree
                          2.5, # building
                          3.5 ), #pavement
     col=c(NA, treeCol1,buildCol1, paveCol1),add=TRUE, legend=FALSE, box=FALSE, maxpixels=28508160)


dev.off()

writeRaster(uticaRes, "E:/Google Drive/research/projects/utica/model_save/1950/all_maps/utica50s_128.tif", format="GTiff" )
