library(sf)
library(raster)
library(mapview)
library(mapedit)

#directory of training images
dirO <- c("/Users/hkropp/Google Drive/research/projects/utica/u_train")
#directory for masks
dirM <- c("/Users/hkropp/Google Drive/research/projects/utica/mask")


#### read in data and visualize ----
#read in data from 1950s
r50s <- raster("E:/Google Drive/GIS/utica/MyProject4/A550500171317_ref.tif")
r50s@crs
plot(r50s, col=gray(1:100/100))

#look at a few areas near the city center to start
Ucenter <- extent(-8377200,-8373500,
                  5326000,5328800)

Ucenter2 <- extent(-8382000,-8373500,
                  5324000,5329000)
#start with working with a small area in the center of
#utica 
u50a <- crop(r50s,Ucenter2)
plot(u50a, col=gray(1:100/100))
#visualize the map
plot(u50s, col=gray(1:100/100))
mapview(u50a, col=gray(1:100/100))


#### subset raster for training ----
u50a

# subset size
# 128 x 128 pixels
#for training masks
#convert to matrix

test <- imgV[1:128,1:128]

nSamp <- 20
set.seed(42)
samplesx <- sample(1:(u50a@ncols-257), nSamp)
set.seed(12)
samplesy <- sample(1:(u50a@nrows-257), nSamp)


#save data, commented out since does not need to run every time



#for(i in 1:nSamp){
  
  
#  writeRaster(crop(u50a, extent(u50a, samplesy[i], 
#                                samplesy[i] +256, 
#                                samplesx[i], 
#                                samplesx[i]+256)), 
#              paste0(dirO, "\\train_",i,".tif"),
#             format="GTiff" )
  

#}

#### make masks for training ----


#### Step 1: read in image   ##

#give training image number
trainNum <- 1

imgN <- raster(paste0(dirO, "/train_",trainNum,".tif"))
plot(test)
#reproject to WGS 84 for mapedit
trainDgc <- projectRaster(trainD, crs="+init=epsg:4326")



#### Step 2 make trees mask   ##

trees <- drawFeatures(mapview(trainDgc, col=grey(1:100/100)))

treeMask <- rasterize(trees,test, field=1, background=0)

plot(treeMask)


writeRaster(treeMask, paste0(dirM,"/trees/tree_mask_",trainNum,".tif"),
            format="GTiff")


#### Step 3 make buildings mask   ##

buildings <- drawFeatures(mapview(trainDgc, col=grey(1:100/100)))

buildingMask <- rasterize(buildings,test, field=1, background=0)

plot(buildingMask)


writeRaster(buildingMask, paste0(dirM,"/building/building_mask_",trainNum,".tif"),
            format="GTiff")

#### Step 4 make buildings mask   ##

pave <- drawFeatures(mapview(trainDgc, col=grey(1:100/100)))

paveMask <- rasterize(pave,test, field=1, background=0)

plot(paveMask)


writeRaster(paveMask, paste0(dirM,"/pavement/pavement_mask_",trainNum,".tif"),
            format="GTiff")


