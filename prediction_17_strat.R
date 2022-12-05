library(raster)
library(sf)
library(mapview)

dirP <- "E:/Google Drive/research/projects/utica/model_save/2017/prediction_strat/image"




treeImg <- list()

Nimg <- 7930

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

writeRaster(treeAll, "E:/Google Drive/research/projects/utica/model_save/2017/save_strat/tree_1.tif",
            format="GTiff")
writeRaster(paveAll, "E:/Google Drive/research/projects/utica/model_save/2017/save_strat/pave_1.tif",
            format="GTiff")
writeRaster(buildAll, "E:/Google Drive/research/projects/utica/model_save/2017/save_strat/build_1.tif",
            format="GTiff")


paveAll2 <- raster("E:/Google Drive/research/projects/utica/model_save/2017/save_strat/pave_2.tif")

treeAll2 <- raster("E:/Google Drive/research/projects/utica/model_save/2017/save_strat/tree_2.tif")
buildAll2 <- raster("E:/Google Drive/research/projects/utica/model_save/2017/save_strat/build_2.tif")

plot(treeAll)
plot(buildAll)
plot(buildAll2)
plot(treeAll2)
plot(paveAll2)
plot(paveAll)
# origImg <- list()
# for(i in 1:Nimg){
#  origImg[[i]] <- raster(paste0(dirI,"/predict_",i,".tif"))
  
  
#}

# origAll <- do.call(merge, origImg)



# Offset 2 merge ------------------


#dirP2 <- "E:/Google Drive/research/projects/utica/model_save/2017/prediction_strat/image_2"


#treeImg2 <- list()

#Nimg2 <- 7930

#for(i in 1:Nimg2){
#  treeImg2[[i]] <- raster(paste0(dirP2,"/tree/tree_",i,".tif"))
  
  
#}

#treeAll2 <- do.call(merge, treeImg2)



#paveImg2 <- list()

#for(i in 1:Nimg2){
#  paveImg2[[i]] <- raster(paste0(dirP2,"/pavement/pavement_",i,".tif"))
  
  
#}

#paveAll2 <- do.call(merge, paveImg2)


#buildImg2 <- list()
#for(i in 1:Nimg){
#  buildImg2[[i]] <- raster(paste0(dirP2,"/building/building_",i,".tif"))
  
  
#}

#buildAll2 <- do.call(merge, buildImg2)




# Offset 3 merge -----------------


#dirP3 <- "E:/Google Drive/research/projects/utica/model_save/2017/prediction_strat/image_3"


#treeImg3 <- list()

#Nimg3 <- 7740

#for(i in 1:Nimg3){
#  treeImg3[[i]] <- raster(paste0(dirP3,"/tree/tree_",i,".tif"))
  
  
#}

#treeAll3 <- do.call(merge, treeImg3)



#paveImg3 <- list()

##for(i in 1:Nimg3){
#  paveImg3[[i]] <- raster(paste0(dirP3,"/pavement/pavement_",i,".tif"))
  
  
#}

#paveAll3 <- do.call(merge, paveImg3)


#buildImg3 <- list()
#for(i in 1:Nimg3){
#  buildImg3[[i]] <- raster(paste0(dirP3,"/building/building_",i,".tif"))
  
  
#}

#buildAll3 <- do.call(merge, buildImg3)






# Combine overlays -------------

#match offsets to original

## Trees  



#resample to original
treeAll2rs <- resample(treeAll2, treeAll)
#treeAll3rs <- resample(treeAll3, treeAll)


treeCombine <- stack(treeAll, treeAll2rs) #, treeAll3rs)

# get maximum prob
treeLayer <- calc(treeCombine, function(x){max(x, na.rm=TRUE)})
plot(treeLayer)


## Buildings

#resample to original
buildAll2rs <- resample(buildAll2, buildAll)
#buildAll3rs <- resample(buildAll3, buildAll)

buildCombine <- stack(buildAll,buildAll2rs) #,buildAll3rs)
buildLayer <- calc(buildCombine, function(x){max(x, na.rm=TRUE)})
plot(buildLayer)


## Pavement
paveAll2rs <- resample(paveAll2, paveAll)
#paveAll3rs <- resample(paveAll3, paveAll)

paveCombine <- stack(paveAll, paveAll2rs)#, paveAll3rs)
paveLayer <- calc(paveCombine, function(x){max(x, na.rm=TRUE)})
plot(paveLayer)


# Make final map cover -------------

# remove noise below set threshold

treeMap <- calc(treeLayer,function(x){ifelse(x <= 0.2, 0, x)})
buildMap <- calc(buildLayer,function(x){ifelse(x <= 0.15, 0, x)})
paveMap <- calc(paveLayer,function(x){ifelse(x <= 0.3, 0, x)})


# binary map of above

treeMapB <- calc(treeLayer,function(x){ifelse(x <= 0.2, 0, 1)})
buildMapB <- calc(buildLayer,function(x){ifelse(x <= 0.15, 0, 1)})
paveMapB <- calc(paveLayer,function(x){ifelse(x <= 0.3, 0, 1)})

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



treeCol1 <- rgb(0.13,0.54,0.13,0.5)
paveCol1 <- rgb(0.96,0.49,0,0.5)
buildCol1 <- rgb(0.53,0.17,0.09,0.5)

treeCol1 <- rgb(0.13,0.54,0.13)
paveCol1 <- rgb(0.96,0.49,0)
buildCol1 <- rgb(0.53,0.17,0.09)

#save raster

writeRaster(uticaClass, "E:/Google Drive/research/projects/utica/model_save/2017/all_maps/utica17_strat_fix.tif", format="GTiff" )

writeRaster(uticaClass, "c:/Users/hkropp/Documents/utica17_strat_fix.tif", format="GTiff" )

