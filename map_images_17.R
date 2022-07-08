library(sf)
library(raster)
library(mapview)
library(mapedit)

dirN <- 1
#directory of training images
dirO <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica/model_save/2017/data/train/images",
          "e:/Google Drive/research/projects/utica/model_save/2017/data/train/images")


#directory for masks
dirM <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica/model_save/2017/data/train/mask",
          "e:/Google Drive/research/projects/utica/model_save/2017/data/train/mask")

dirMV <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica/model_save/2017/data/valid",
           "e:/Google Drive/research/projects/utica/model_save/2017/data/valid" )

#### read in data and visualize ----
#read in data from 2017
# crs is in state plane from State website

# r17 <- stack("e:/Google Drive/research/projects/utica/utica17/u2017.tif")

# r17@crs
# plotRGB(r17)

# r17rp <- projectRaster(r17,crs="+init=epsg:4326")

# save reprojected because too long to reload:
# writeRaster(r17rp,"e:/Google Drive/research/projects/utica/utica17/u2017_rp.tif", format="GTiff")


# r17rp <- stack("e:/Google Drive/research/projects/utica/utica17/u2017_rp.tif")




# Ucenter <- extent(-75.3386, -75.21397, 43.07212, 43.115 )

#start with working with a small area in the center of
#utica 
# u17a <- crop(r17rp,Ucenter)
# plot(u17a[[1]], col=gray(1:100/100))

# writeRaster(u17a,"e:/Google Drive/research/projects/utica/utica17/u2017_crop.tif", format="GTiff")

u17a <- stack("e:/Google Drive/research/projects/utica/utica17/u2017_crop.tif")
u17a <- stack("/Volumes/GoogleDrive/My Drive/research/projects/utica/utica17/u2017_crop.tif")
plotRGB(u17a)



u17a@nrows*u17a@ncols/(256*256)

#### subset raster for training ----


# subset size
# 256 x 256 pixels
#for training masks
#convert to matrix


nSamp <- 60
set.seed(452)
samplesx <- sample(1:(u17a@ncols-257), nSamp)
set.seed(102)
samplesy <- sample(1:(u17a@nrows-257), nSamp)


#validation samples: 60-80
nSamp4 <- 80
set.seed(452)
samplesx4 <- sample(1:(u17a@ncols-257), nSamp4)[61:80]
set.seed(102)
samplesy4 <- sample(1:(u17a@nrows-257), nSamp4)[61:80]

nSamp2 <- 81
set.seed(452)
samplesx2 <- sample(1:(u17a@ncols-257), nSamp2)[81]
set.seed(102)
samplesy2 <- sample(1:(u17a@nrows-257), nSamp2)[81]

# save data, commented out since does not need to run every time
 
#  
#    for(i in 1:60){
#      
#      
#      writeRaster(crop(u17a, extent(u17a, samplesy[i], 
#                                   samplesy[i] +255, 
#                                    samplesx[i], 
#                                  samplesx[i]+255)), 
#                paste0(dirO[dirN], "/train_",i,".tif"),
#                format="GTiff" ,overwrite=TRUE)
#     
#    
#    }
# 
# 
 # for(i in 1:20){
 #   
 #   
 #   writeRaster(crop(u17a, extent(u17a, samplesy4[i], 
 #                                samplesy4[i] +255, 
 #                                 samplesx4[i], 
 #                                 samplesx4[i]+255)), 
 #               paste0(dirMV[dirN], "/images/valid_",i,".tif"),
 #               format="GTiff" ,overwrite=TRUE)
 #   
 #  
 # }

# image 2 is in a weird location on edge of map but within extent. 
# too many NA

   # writeRaster(crop(u17a, extent(u17a, samplesy2, 
   #                               samplesy2 +255, 
   #                               samplesx2, 
   #                              samplesx2+255)), 
   #             paste0(dirO[dirN], "/train_",2,".tif"),
   #             format="GTiff" )

#### make masks for training & validation ----


#### Step 1: read in image   ##

#give training image number
trainNum <- 21

imgN <- stack(paste0(dirO[dirN], "/train_",trainNum,".tif"))
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

trees <- drawFeatures(viewRGB(imgN, quantiles=c(0,1)))

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

plot(paveMask)


writeRaster(paveMask, paste0(dirM[dirN],"/pavement/pavement_mask_",trainNum,".tif"),
            format="GTiff")

###### Validation ----

#### Step 1: read in image   ##

#give valid image number
validNum <- 20

imgN <- raster(paste0(dirO, "/50s_valid/valid_",validNum,".tif"))
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


writeRaster(paveMask, paste0(dirMV,"/pavement/pavement_mask_",validNum,".tif"),
            format="GTiff")


###### Prep for prediction ----

#reproject to be in WGS 1984 so matches all mask images

u50rp <- projectRaster(u50a,  crs="+init=epsg:4326")
plot(u50rp, col=gray(1:100/100))

cols50 <- floor(u50rp@ncols/256) 
rows50 <- floor(u50rp@nrows/256) 


# break up into tiles
colsSeq <- seq(1,cols50*256, by=256)
rowsSeq <- seq(1,rows50*256, by=256)
subDF <- data.frame(cols=rep(colsSeq,times=length(rowsSeq)),
                    rows=rep(rowsSeq,each=length(colsSeq)))
#subdivide raster into 256 x 256
sub50s <- list()
rowcount <- numeric()
colcount <- numeric()
#this will shave off extra off south and west 
for(i in 1:nrow(subDF)){
  sub50s[[i]] <- crop(u50rp, extent(u50rp,  subDF$rows[i], 
                                   subDF$rows[i]+255,
                                   subDF$cols[i], 
                                   subDF$cols[i]+255))
  rowcount[i] <- sub50s[[i]]@nrows
  colcount[i] <- sub50s[[i]]@ncols
}
sub50s[[1]]@ncols

m <- do.call(merge, sub50s)
plot(m, col=gray(1:100/100))
#save

for(i in 1:nrow(subDF)){
  writeRaster(sub50s[[i]],
             paste0(dirO,"/predict50/predict_",i,".tif"),
             format="GTiff")
}

plot(sub50s2[[1]])

# break up into tiles to do offset:

colsSeq2 <- seq(25,((cols50-1)*256)+25, by=256)
rowsSeq2 <- seq(25,(rows50*256-1)-25, by=256)
subDF2 <- data.frame(cols=rep(colsSeq2,times=length(rowsSeq2)),
                    rows=rep(rowsSeq2,each=length(colsSeq2)))

#subdivide raster into 256 x 256
sub50s2 <- list()
rowcount2 <- numeric()
colcount2 <- numeric()
#this will shave off extra off south and west 
for(i in 1:nrow(subDF2)){
  sub50s2[[i]] <- crop(u50rp, extent(u50rp,  subDF2$rows[i], 
                                    subDF2$rows[i]+255,
                                    subDF2$cols[i], 
                                    subDF2$cols[i]+255))
  rowcount2[i] <- sub50s2[[i]]@nrows
  colcount2[i] <- sub50s2[[i]]@ncols
}

sub50s2[[1]]@ncols

m2 <- do.call(merge, sub50s2)
plot(m2, col=gray(1:100/100))
#save

for(i in 1:nrow(subDF2)){
  writeRaster(sub50s2[[i]],
              paste0("/Volumes/GoogleDrive/My Drive/research/projects/utica/model_save/1950/img_tile256/predict50_2/predict_",i,".tif"),
              format="GTiff")
}

# break up into tiles to do second offset:

colsSeq3 <- seq(100,((cols50-2)*256)+100, by=256)
rowsSeq3 <- seq(100,((rows50-2)*256)+100, by=256)
subDF3 <- data.frame(cols=rep(colsSeq3,times=length(rowsSeq3)),
                     rows=rep(rowsSeq3,each=length(colsSeq3)))

#subdivide raster into 256 x 256
sub50s3 <- list()
rowcount3 <- numeric()
colcount3 <- numeric()
#this will shave off extra off south and west 
for(i in 1:nrow(subDF3)){
  sub50s3[[i]] <- crop(u50rp, extent(u50rp,  subDF3$rows[i], 
                                     subDF3$rows[i]+255,
                                     subDF3$cols[i], 
                                     subDF3$cols[i]+255))
  rowcount3[i] <- sub50s3[[i]]@nrows
  colcount3[i] <- sub50s3[[i]]@ncols
}

plot(sub50s[[1]])
plot(sub50s2[[1]])
plot(sub50s3[[1]])

m3 <- do.call(merge, sub50s3)
plot(m3, col=gray(1:100/100))
#save

for(i in 1:nrow(subDF3)){
  writeRaster(sub50s3[[i]],
              paste0("/Volumes/GoogleDrive/My Drive/research/projects/utica/model_save/1950/img_tile256/predict50_3/predict_",i,".tif"),
              format="GTiff")
}

testp <- list()
rowcountp <- numeric()
colcountp <- numeric()
for(i in 1:nrow(subDF)){
testp[[i]] <- raster(paste0(dirO,"/predict50/predict_",i,".tif"))
rowcountp[i] <- testp[[i]]@nrows
colcountp[i] <- testp[[i]]@ncols
}



#read in test
testOut <- raster("/Volumes/GoogleDrive/My Drive/research/projects/utica/50s_valid_out/valid_out.tif")

plot(testOut)


treesPredict <- list()
for(i in 1:20){
  treesPredict[[i]] <- raster(paste0("/Volumes/GoogleDrive/My Drive/research/projects/utica/50s_valid_out/tree/tree_predict_",i,".tif"))
  
}

plot(u50rp, col=gray(1:100/100))
plot(treesPredict[[1]], add=TRUE)
plot(treesPredict[[2]], add=TRUE)
plot(treesPredict[[3]], add=TRUE)
plot(treesPredict[[4]], add=TRUE)
plot(treesPredict[[5]], add=TRUE)

mapview(u50rp, col=gray(1:100/100))+
  mapview(treesPredict[[1]])
