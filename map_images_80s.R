library(sf)
library(raster)
library(mapview)
library(mapedit)


#directory of training images
dirO <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica")
#directory for masks
dirM <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica/80s_mask")

dirMV <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica/mask_80s_valid")

Ucenter2 <- extent(-8382000,-8373500,
                   5324000,5329000)
# 1980s is a different scope and edge of film is in this extent
# adjust
Ucenter3 <- extent(-8379200,-8371200,
                   5324000,5329000)

#### read in data and visualize ----
#read in data from 1950s
#georeferenced in ArcPro to the ESRI world imagery basemap
# crs is in web mercator since that is automatic base map CRS.
r80s <- raster("/Volumes/GoogleDrive/My Drive/research/projects/utica/utica80/utica80_10086.tif")
r80s@crs
plot(r80s, col=gray(1:100/100))

#start with working with a small area in the center of
#utica 
u80a <- crop(r80s,Ucenter3)
plot(u80a, col=gray(1:100/100))


u80rp <- projectRaster(u80a,  crs="+init=epsg:4326")
plot(u80rp, col=gray(1:100/100))
mapview(u80rp)
u80rp@crs


# random samples

#additional samples: 1-60
nSamp1 <- 60
set.seed(342)
samplesx <- sample(1:(u80a@ncols-257), nSamp1)[1:60]
set.seed(142)
samplesy <- sample(1:(u80a@nrows-257), nSamp1)[1:60]

#validation samples: 60-80
nSamp2 <- 80
set.seed(342)
samplesx2 <- sample(1:(u80a@ncols-257), nSamp2)[61:80]
set.seed(142)
samplesy2 <- sample(1:(u80a@nrows-257), nSamp2)[61:80]



# save data, commented out since does not need to run every time
# 
#  for(i in 1:60){
#    
#    
#    writeRaster(crop(u80a, extent(u80a, samplesy[i], 
#                                  samplesy[i] +255, 
#                                  samplesx[i], 
#                                  samplesx[i]+255)), 
#                paste0(dirO, "/80s_train/train_",i,".tif"),
#                format="GTiff" ,overwrite=TRUE)
#    
#    
#  }
#   for(i in 1:20){
#     
#     
#     writeRaster(crop(u80a, extent(u80a, samplesy2[i], 
#                                  samplesy2[i] +255, 
#                                   samplesx2[i], 
#                                 samplesx2[i]+255)), 
#               paste0(dirO, "/80s_valid/valid_",i,".tif"),
#               format="GTiff" ,overwrite=TRUE)
#    
#   
#   }

#### Step 1: read in image   ##

#give training image number
trainNum <- 1

imgN <- raster(paste0(dirO, "/80s_train/train_",trainNum,".tif"))
plot(imgN, col=grey(1:100/100))

#reproject to WGS 84 for mapedit
trainDgc <- projectRaster(imgN, crs="+init=epsg:4326")

writeRaster(trainDgc, paste0(dirM,"/reproj_test/wgs_train_",trainNum,".tif"),
            format="GTiff")

writeRaster(trainDgc, paste0(dirM,"/u_train_reproject/wgs_train_",trainNum,".tif"),
            format="GTiff", overwrite=TRUE)
trainDgc@nrows
trainDgc@ncols
plot(trainDgc)

#check
trainNum <- 1
imCheck <- raster(paste0(dirM,"/u_train_reproject/wgs_train_",trainNum,".tif"))
plot(imCheck)
range(imCheck)
imgN@ncols
imgN@nrows
imgN@crs

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


###### Validation ----

#### Step 1: read in image   ##

#give valid image number
validNum <- 1

imgN <- raster(paste0(dirO, "/80s_valid/valid_",validNum,".tif"))
plot(imgN, col=grey(1:100/100))

imgN@ncols
imgN@nrows
#reproject to WGS 84 for mapedit
validDgc <- projectRaster(imgN, crs="+init=epsg:4326")

writeRaster(validDgc, paste0(dirMV,"/u_valid_reproject/wgs_valid_",validNum,".tif"),
            format="GTiff")
validDgc@nrows
validDgc@ncols
# use zoom 18-21
# avoid features that are not clearly identifiable
# do not label shadows as any feature
# only count immediate coverage of the surface. For example
# a tree canopy clearly extending over the street gets
# counted as a tree not street since it is the object that is
# directly observed.
#### Step 2 make trees mask   ##

trees <- drawFeatures(mapview(validDgc, col=grey(1:100/100)))

treeMask <- rasterize(trees,validDgc, field=1, background=0)

plot(treeMask)



writeRaster(treeMask, paste0(dirMV,"/trees/tree_mask_",validNum,".tif"),
            format="GTiff")


#### Step 3 make buildings mask   ##

buildings <- drawFeatures(mapview(validDgc, col=grey(1:100/100))+
                            mapview(trees, col.regions="seagreen"))

buildingMask <- rasterize(buildings,validDgc, field=1, background=0)

plot(buildingMask)


writeRaster(buildingMask, paste0(dirMV,"/building/building_mask_",validNum,".tif"),
            format="GTiff")

buildingMask@ncols
buildingMask@nrows

range(getValues(buildingMask))
#### Step 4 make buildings mask   ##

pave <- drawFeatures(mapview(validDgc, col=grey(1:100/100))+
                       mapview(trees, col.regions="seagreen")+
                       mapview(buildings, col.regions="tomato"))

paveMask <- rasterize(pave,validDgc, field=1, background=0)

plot(paveMask)




###### Prep for prediction ----

#reproject to be in WGS 1984 so matches all mask images



cols80 <- floor(u80rp@ncols/256) 
rows80 <- floor(u80rp@nrows/256) 

colsSeq <- seq(1,cols80*256, by=256)
rowsSeq <- seq(1,rows80*256, by=256)
subDF <- data.frame(cols=rep(colsSeq,times=length(rowsSeq)),
                    rows=rep(rowsSeq,each=length(colsSeq)))
#subdivide raster into 256 x 256
sub80s <- list()
rowcount <- numeric()
colcount <- numeric()
#this will shave off extra off south and west 
for(i in 1:nrow(subDF)){
  sub80s[[i]] <- crop(u80rp, extent(u80rp,  subDF$rows[i], 
                                    subDF$rows[i]+255,
                                    subDF$cols[i], 
                                    subDF$cols[i]+255))
  rowcount[i] <- sub80s[[i]]@nrows
  colcount[i] <- sub80s[[i]]@ncols
}
sub80s[[1]]@ncols

m <- do.call(merge, sub80s)
plot(m, col=gray(1:100/100))
#save

for(i in 1:nrow(subDF)){
  writeRaster(sub80s[[i]],
              paste0(dirO,"/predict80/predict_",i,".tif"),
              format="GTiff")
}
