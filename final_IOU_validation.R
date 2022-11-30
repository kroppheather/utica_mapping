library(raster)
library(sf)
library(mapedit)
library(mapview)
library(caret)
library(dplyr)


# reclassify all final maps so that they overlap.(land class overlay plus original image)


# 1957

  
image57 <- raster("E:/Google Drive/research/projects/utica/utica50/utica50_3.tif")
image57


# 2017

image17 <- stack("E:/Google Drive/research/projects/utica/utica17/u2017_crop.tif")
image17


### valid images for 1957 ###

nSamp <- 20
set.seed(1032)
samplesx <- sample(1:(image57@ncols-257), nSamp)
set.seed(120)
samplesy <- sample(1:(image57@nrows-257), nSamp)


 for(i in 1:20){
   
   
   writeRaster(crop(image57, extent(image57, samplesy[i], 
                                samplesy[i] +255, 
                                 samplesx[i], 
                               samplesx[i]+255)), 
              paste0( "E:/Google Drive/research/projects/utica/model_save/1950/IOU_valid/images/valid_",i,".tif"),
             format="GTiff" ,overwrite=TRUE)
  
 
 }





#### Step 1: read in image   ##
dirN <- 1
dirO <- "E:/Google Drive/research/projects/utica/model_save/1950/IOU_valid/images"
dirM <- "E:/Google Drive/research/projects/utica/model_save/1950/IOU_valid/masks"


#give training image number
trainNum <- 20

imgN <- raster(paste0(dirO[dirN], "/valid_",trainNum,".tif"))
plot(imgN, col=gray(1:100/100))

imgN@ncols
imgN@nrows


# use zoom 18-21
# avoid features that are not clearly identifiable
# do not label shadows as any feature
# only count immediate coverage of the surface. For example
# a tree canopy clearly extending over the street gets
# counted as a tree not street since it is the object that is
# directly observed.
#### Step 2 make trees mask   ##

trees <- drawFeatures(mapview(imgN, col=grey(1:100/100)))

treeMask <- rasterize(trees,imgN, field=1, background=0)
# treeMask <- treeMask -1
plot(treeMask)

treeMask@ncols
treeMask@nrows

writeRaster(treeMask, paste0(dirM[dirN],"/trees/tree_mask_",trainNum,".tif"),
            format="GTiff")


#### Step 3 make buildings mask   ##

buildings <- drawFeatures(mapview(imgN, col=grey(1:100/100))+
                            mapview(trees, col.regions="seagreen"))

buildingMask <- rasterize(buildings,imgN, field=1, background=0)
# for images with no features trace whole image then run this code
# buildingMask <- buildingMask -1
plot(buildingMask)


writeRaster(buildingMask, paste0(dirM[dirN],"/buildings/building_mask_",trainNum,".tif"),
            format="GTiff")

buildingMask@ncols
buildingMask@nrows

range(getValues(buildingMask))
#### Step 4 make buildings mask   ##

pave <- drawFeatures(mapview(imgN, col=grey(1:100/100))+
                       mapview(trees, col.regions="seagreen")+
                       mapview(buildings, col.regions="tomato"))

paveMask <- rasterize(pave,imgN, field=1, background=0)
# paveMask <- paveMask -1
plot(paveMask)



writeRaster(paveMask, paste0(dirM[dirN],"/pavement/pavement_mask_",trainNum,".tif"),
            format="GTiff")


### valid images for 1957 ###

nSamp <- 20
set.seed(594)
samplesx <- sample(1:(image17@ncols-257), nSamp)
set.seed(284)
samplesy <- sample(1:(image17@nrows-257), nSamp)


for(i in 1:20){
  
  
  writeRaster(crop(image17, extent(image17, samplesy[i], 
                                   samplesy[i] +255, 
                                   samplesx[i], 
                                   samplesx[i]+255)), 
              paste0( "E:/Google Drive/research/projects/utica/model_save/2017/IOU_valid/images/valid_",i,".tif"),
              format="GTiff" ,overwrite=TRUE)
  
  
}





#### Step 1: read in image   ##

dirN <- 1
dirO <- "E:/Google Drive/research/projects/utica/model_save/2017/IOU_valid/images"
dirM <- "E:/Google Drive/research/projects/utica/model_save/2017/IOU_valid/masks"



#give training image number
trainNum <- 15

imgN <- stack(paste0(dirO[dirN], "/valid_",trainNum,".tif"))
plotRGB(imgN)

imgN@ncols
imgN@nrows


# use zoom 18-21
# avoid features that are not clearly identifiable
# do not label shadows as any feature
# only count immediate coverage of the surface. For example
# a tree canopy clearly extending over the street gets
# counted as a tree not street since it is the object that is
# directly observed.
#### Step 2 make trees mask   ##

trees <- drawFeatures(viewRGB(imgN, quantiles=c(0.01,0.99)))

treeMask <- rasterize(trees,imgN, field=1, background=0)

plot(treeMask)

treeMask@ncols
treeMask@nrows

writeRaster(treeMask, paste0(dirM[dirN],"/trees/tree_mask_",trainNum,".tif"),
            format="GTiff")


#### Step 3 make buildings mask   ##

buildings <- drawFeatures(viewRGB(imgN, quantiles=c(0,1))+
                            mapview(trees, col.regions="seagreen"))

buildingMask <- rasterize(buildings,imgN, field=1, background=0)
# buildingMask <- buildingMask -1
plot(buildingMask)


writeRaster(buildingMask, paste0(dirM[dirN],"/building/building_mask_",trainNum,".tif"),
            format="GTiff")

buildingMask@ncols
buildingMask@nrows

range(getValues(buildingMask))
#### Step 4 make buildings mask   ##

pave <- drawFeatures(viewRGB(imgN, quantiles=c(0,1))+
                       mapview(trees, col.regions="seagreen")+
                       mapview(buildings, col.regions="tomato"))

paveMask <- rasterize(pave,imgN, field=1, background=0)
# paveMask <- paveMask -1
plot(paveMask)


writeRaster(paveMask, paste0(dirM[dirN],"/pavement/pavement_mask_",trainNum,".tif"),
            format="GTiff")


