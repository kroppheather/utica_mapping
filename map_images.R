library(sf)
library(raster)
library(mapview)
library(mapedit)

#directory of training images
dirO <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica")
#directory for masks
dirM <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica/mask")


#### read in data and visualize ----
#read in data from 1950s
r50s <- raster("/Volumes/GoogleDrive/My Drive/research/projects/utica/A550500171317_ref.tif")
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


#additional samples: 20-40
nSamp2 <- 40
set.seed(42)
samplesx2 <- sample(1:(u50a@ncols-257), nSamp2)[21:40]
set.seed(12)
samplesy2 <- sample(1:(u50a@nrows-257), nSamp2)[21:40]
#save data, commented out since does not need to run every time



for(i in 1:20){
  
  
  writeRaster(crop(u50a, extent(u50a, samplesy2[i], 
                               samplesy2[i] +256, 
                                samplesx2[i], 
                              samplesx2[i]+256)), 
            paste0(dirO, "/50s_train/train_",i+20,".tif"),
            format="GTiff" ,overwrite=TRUE)
  

}

#### make masks for training ----


#### Step 1: read in image   ##

#give training image number
trainNum <- 34

imgN <- raster(paste0(dirO, "/50s_train/train_",trainNum,".tif"))
plot(imgN, col=grey(1:100/100))

imgN@ncols
imgN@nrows
#reproject to WGS 84 for mapedit
trainDgc <- projectRaster(imgN, crs="+init=epsg:4326")

writeRaster(trainDgc, paste0(dirM,"/u_train_reproject/wgs_train_",trainNum,".tif"),
            format="GTiff")
trainDgc@nrows
trainDgc@ncols
# use zoom 18-21
# avoid features that are not clearly identifiable
# do not label shadows as any feature
# only count immediate coverage of the surface. For example
# a tree canopy clearly extending over the street gets
# counted as a tree not street since it is the object that is
# directly observed.
#### Step 2 make trees mask   ##

trees <- drawFeatures(mapview(trainDgc, col=grey(1:100/100)))

treeMask <- rasterize(trees,trainDgc, field=1, background=0)

plot(treeMask)


writeRaster(treeMask, paste0(dirM,"/trees/tree_mask_",trainNum,".tif"),
            format="GTiff")


#### Step 3 make buildings mask   ##

buildings <- drawFeatures(mapview(trainDgc, col=grey(1:100/100))+
                            mapview(trees, col.regions="seagreen"))

buildingMask <- rasterize(buildings,trainDgc, field=1, background=0)

plot(buildingMask)


writeRaster(buildingMask, paste0(dirM,"/building/building_mask_",trainNum,".tif"),
            format="GTiff")

buildingMask@ncols
buildingMask@nrows

range(getValues(buildingMask))
#### Step 4 make buildings mask   ##

pave <- drawFeatures(mapview(trainDgc, col=grey(1:100/100))+
                       mapview(trees, col.regions="seagreen")+
                       mapview(buildings, col.regions="tomato"))

paveMask <- rasterize(pave,trainDgc, field=1, background=0)

plot(paveMask)


writeRaster(paveMask, paste0(dirM,"/pavement/pavement_mask_",trainNum,".tif"),
            format="GTiff")


