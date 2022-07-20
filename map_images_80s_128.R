library(sf)
library(raster)
library(mapview)
library(mapedit)


#directory of training images
dirO <- c(#"/Volumes/GoogleDrive/My Drive/research/projects/utica/model_save/1980/img_tile_128"
  "e:/Google Drive/research/projects/utica/model_save/1980/img_tile_128")
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
r80s <- raster( # "/Volumes/GoogleDrive/My Drive/research/projects/utica/utica80/utica80_10086.tif")
              "e:/Google Drive/research/projects/utica/utica80/utica80_10086.tif")
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


nSamp5 <- 120
set.seed(342)
samplesx5 <- sample(1:(u80a@ncols-257), nSamp5)[81:120]
set.seed(142)
samplesy5 <- sample(1:(u80a@nrows-257), nSamp5)[81:120]

# save data, commented out since does not need to run every time
# 
#  for(i in 1:40){
#    
#    
#   writeRaster(crop(u80a, extent(u80a, samplesy5[i], 
#                                  samplesy5[i] +127, 
#                                  samplesx5[i], 
#                                  samplesx5[i]+127)), 
#                paste0( "/Volumes/GoogleDrive/My Drive/research/projects/utica/model_save/1980/data_128/train/img/train_",i,".tif"),
#                format="GTiff" ,overwrite=TRUE)
    
    
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



cols80 <- floor(u80rp@ncols/128) 
rows80 <- floor(u80rp@nrows/128) 

colsSeq <- seq(1,cols80*128, by=128)
rowsSeq <- seq(1,rows80*128, by=128)
subDF <- data.frame(cols=rep(colsSeq,times=length(rowsSeq)),
                    rows=rep(rowsSeq,each=length(colsSeq)))
#subdivide raster 
sub80s <- list()
rowcount <- numeric()
colcount <- numeric()
#this will shave off extra off south and west 
for(i in 1:nrow(subDF)){
  sub80s[[i]] <- crop(u80rp, extent(u80rp,  subDF$rows[i], 
                                    subDF$rows[i]+127,
                                    subDF$cols[i], 
                                    subDF$cols[i]+127))
  rowcount[i] <- sub80s[[i]]@nrows
  colcount[i] <- sub80s[[i]]@ncols
}
sub80s[[1]]@ncols

m <- do.call(merge, sub80s)
plot(m, col=gray(1:100/100))
#save

for(i in 1:nrow(subDF)){
  writeRaster(sub80s[[i]],
              paste0(dirO,"/image/predict_",i,".tif"),
              format="GTiff")
}


# break up into tiles to do offset:

colsSeq2 <- seq(25,((cols80-1)*128)+25, by=128)
rowsSeq2 <- seq(25,(rows80*128-1)-25, by=128)
subDF2 <- data.frame(cols=rep(colsSeq2,times=length(rowsSeq2)),
                     rows=rep(rowsSeq2,each=length(colsSeq2)))

#subdivide raster into 256 x 256
sub80s2 <- list()
rowcount2 <- numeric()
colcount2 <- numeric()
#this will shave off extra off south and west 
for(i in 1:nrow(subDF2)){
  sub80s2[[i]] <- crop(u80rp, extent(u80rp,  subDF2$rows[i], 
                                     subDF2$rows[i]+127,
                                     subDF2$cols[i], 
                                     subDF2$cols[i]+127))
  rowcount2[i] <- sub80s2[[i]]@nrows
  colcount2[i] <- sub80s2[[i]]@ncols
}

sub80s2[[1]]@ncols

m2 <- do.call(merge, sub80s2)
plot(m2, col=gray(1:100/100))
#save

for(i in 1:nrow(subDF2)){
  writeRaster(sub80s2[[i]],
              paste0(dirO,"/image_2/predict_",i,".tif"),
              format="GTiff")
}


colsSeq3 <- seq(100,((cols80-2)*128)+100, by=128)
rowsSeq3 <- seq(100,((rows80-2)*128)+100, by=128)
subDF3 <- data.frame(cols=rep(colsSeq3,times=length(rowsSeq3)),
                     rows=rep(rowsSeq3,each=length(colsSeq3)))

#subdivide raster into 256 x 256
sub80s3 <- list()
rowcount3 <- numeric()
colcount3 <- numeric()
#this will shave off extra off south and west 
for(i in 1:nrow(subDF3)){
  sub80s3[[i]] <- crop(u80rp, extent(u80rp,  subDF3$rows[i], 
                                     subDF3$rows[i]+127,
                                     subDF3$cols[i], 
                                     subDF3$cols[i]+127))
  rowcount3[i] <- sub80s3[[i]]@nrows
  colcount3[i] <- sub80s3[[i]]@ncols
}

plot(sub80s[[1]])
plot(sub80s2[[1]])
plot(sub80s3[[1]])

m3 <- do.call(merge, sub80s3)
plot(m3, col=gray(1:100/100))
#save

for(i in 1:nrow(subDF3)){
  writeRaster(sub80s3[[i]],
              paste0(dirO,"/image_3/predict_",i,".tif"),
              format="GTiff")
}