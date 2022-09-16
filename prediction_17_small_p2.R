library(raster)
library(sf)
library(mapview)


#start 12:17 8/26

origAll <- raster("E:/Google Drive/research/projects/utica/utica17/utica2017_crop.tif")


treeAll <- raster( "E:/Google Drive/research/projects/utica/model_save/2017/save_128/tree_1.tif")

paveAll <- raster( "E:/Google Drive/research/projects/utica/model_save/2017/save_128/pave_1.tif")




buildAll <- raster( "E:/Google Drive/research/projects/utica/model_save/2017/save_128/build_1.tif")


# Offset 2 merge ------------------


treeAll2 <- raster("E:/Google Drive/research/projects/utica/model_save/2017/save_128/tree_2.tif")

paveAll2 <- raster("E:/Google Drive/research/projects/utica/model_save/2017/save_128/pave_2.tif")


buildAll2 <- raster("E:/Google Drive/research/projects/utica/model_save/2017/save_128/build_2.tif")


# Offset 3 merge -----------------


treeAll3 <- raster( "E:/Google Drive/research/projects/utica/model_save/2017/save_128/tree_3.tif")




paveAll3 <- raster( "E:/Google Drive/research/projects/utica/model_save/2017/save_128/pave_3.tif")




buildAll3 <- raster("E:/Google Drive/research/projects/utica/model_save/2017/save_128/build_3.tif")



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

treeMap <- calc(treeLayer,function(x){ifelse(x <= 0.45, 0, x)})
buildMap <- calc(buildLayer,function(x){ifelse(x <= 0.65, 0, x)})
paveMap <- calc(paveLayer,function(x){ifelse(x <= 0.2, 0, x)})


# binary map of above

treeMapB <- calc(treeLayer,function(x){ifelse(x <= 0.45, 0, 1)})
buildMapB <- calc(buildLayer,function(x){ifelse(x <= 0.65, 0, 1)})
paveMapB <- calc(paveLayer,function(x){ifelse(x <= 0.2, 0, 1)})

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


plotRGB(origAll3)

treeCol1 <- rgb(0.13,0.54,0.13,0.5)
paveCol1 <- rgb(0.96,0.49,0,0.5)
buildCol1 <- rgb(0.53,0.17,0.09,0.5)

treeCol1 <- rgb(0.13,0.54,0.13)
paveCol1 <- rgb(0.96,0.49,0)
buildCol1 <- rgb(0.53,0.17,0.09)

#save raster

writeRaster(uticaClass, "E:/Google Drive/research/projects/utica/model_save/2017/all_maps/utica17_128.tif", format="GTiff" )

writeRaster(uticaClass, "c:/Users/hkropp/Documents/utica17_128.tif", format="GTiff" )

png("E:/Google Drive/research/projects/utica/model_save/2017/maps_128/utica_classification.png", width=origAll@ncols,height=origAll@nrows)

plotRGB(origAll, axes=FALSE, legend=FALSE, box=FALSE, maxpixels=origAll@ncols*origAll@nrows)

plot(uticaClass, breaks=c(-0.1,0.5,#breaks between other
                        1.5, # tree
                        2.5, # building
                        3.5 ), #pavement
     col=c(NA, treeCol1,buildCol1, paveCol1),add=TRUE, legend=FALSE, box=FALSE, maxpixels=uticaClass@nrows*uticaClass@ncols)


dev.off()

plotRGB(origAll, axes=FALSE, legend=FALSE, box=FALSE)
