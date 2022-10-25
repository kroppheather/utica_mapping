library(sf)
library(raster)
library(mapview)
library(mapedit)

dirN <- 2
#directory of training images
dirO <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica/model_save/2017/data_strat/images",
          "e:/Google Drive/research/projects/utica/model_save/2017/data_strat/images")


#directory for masks
dirM <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica/model_save/2017/data_strat/mask",
          "e:/Google Drive/research/projects/utica/model_save/2017/data_strat/mask")

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

bound_poly <- st_read("e:/Google Drive/research/projects/utica/strat/bound_strat_17.shp")
bound_poly$Loc <- as.factor(bound_poly$type)
bound_poly$LocID <- as.numeric(bound_poly$Loc)


u17a@nrows*u17a@ncols/(256*256)


# rasterize poly
polyRast <- rasterize(st_zm(bound_poly), u17a[[1]], field="LocID")
plot(polyRast)

matPoly <- getValues(polyRast)
polyRast@nrows
polyRast@ncols

polyDF <- na.omit(data.frame(LocID = matPoly,
                             rowID = rep(seq(1, polyRast@nrows), each=polyRast@ncols),
                             colID = rep(seq(1, polyRast@ncols), times=polyRast@nrows)))

rowIDS <- list()
randRows <- list()
seedi <- c(43,23,5,2,6,34)
lengthi <- c(7,7,7,7,6,6)
dfSub <- list()
for(i in 1:length(unique(polyDF$LocID))){
  rowIDS[[i]] <- polyDF[polyDF$LocID == i,]
  set.seed(seedi[i])
  randRows[[i]] <- sample(1:nrow(rowIDS[[i]]), lengthi[i])
  dfSub[[i]] <- rowIDS[[i]][randRows[[i]],]
}

dfSubA <- do.call( "rbind", dfSub)

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
# cropT <- list()
# 
# for(i in 1:40){
#   
#   cropT[[i]] <-  crop(u17a, extent(u17a, dfSubA$rowID[i], 
#                   dfSubA$rowID[i] +255, 
#                   dfSubA$colID[i], 
#                   dfSubA$colID[i]+255))
# 
# }
#  for(i in 1:40){
#   
#    
#    writeRaster(cropT[[i]], 
#              paste0("e:/Google Drive/research/projects/utica/model_save/2017/data_strat/images/train_",i,".tif"),
#              format="GTiff" ,overwrite=TRUE)
#   
#  
#  }


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
trainNum <- 3

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

validNum <- 20

imgN <- stack(paste0(dirMV[dirN], "/images/valid_",validNum,".tif"))
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

writeRaster(treeMask, paste0(dirMV[dirN],"/mask/trees/tree_mask_",validNum,".tif"),
            format="GTiff")


#### Step 3 make buildings mask   ##

buildings <- drawFeatures(viewRGB(imgN, quantiles=c(0,1))+
                            mapview(trees, col.regions="seagreen"))

buildingMask <- rasterize(buildings,imgN, field=1, background=0)

plot(buildingMask)


writeRaster(buildingMask, paste0(dirMV[dirN],"/mask/building/building_mask_",validNum,".tif"),
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


writeRaster(paveMask, paste0(dirMV[dirN],"/mask/pavement/pavement_mask_",validNum,".tif"),
            format="GTiff")

###### Prep for prediction ----



cols17 <- floor(u17a@ncols/256) 
rows17 <- floor(u17a@nrows/256) 


# break up into tiles
colsSeq <- seq(1,cols17*256, by=256)
rowsSeq <- seq(1,rows17*256, by=256)
subDF <- data.frame(cols=rep(colsSeq,times=length(rowsSeq)),
                    rows=rep(rowsSeq,each=length(colsSeq)))
#subdivide raster into 256 x 256
sub17s <- list()
rowcount <- numeric()
colcount <- numeric()
#this will shave off extra off south and west 
for(i in 1:nrow(subDF)){
  writeRaster(crop(u17a, extent(u17a,  subDF$rows[i], 
                                   subDF$rows[i]+255,
                                   subDF$cols[i], 
                                   subDF$cols[i]+255)),  
              paste0(dirI[dirN],"/image/predict_",i,".tif"),
              format="GTiff")

}

# break up into tiles to do offset:

colsSeq2 <- seq(25,((cols17-1)*256)+25, by=256)
rowsSeq2 <- seq(25,(rows17*256-1)-25, by=256)
subDF2 <- data.frame(cols=rep(colsSeq2,times=length(rowsSeq2)),
                    rows=rep(rowsSeq2,each=length(colsSeq2)))

#subdivide raster into 256 x 256

#this will shave off extra off south and west 
for(i in 1:nrow(subDF2)){
  writeRaster(crop(u17a, extent(u17a,  subDF2$rows[i], 
                                subDF2$rows[i]+255,
                                subDF2$cols[i], 
                                subDF2$cols[i]+255)),  
              paste0(dirI[dirN],"/image_2/predict_",i,".tif"),
              format="GTiff")
  
}

#save


# break up into tiles to do second offset:

colsSeq3 <- seq(100,((cols17-2)*256)+100, by=256)
rowsSeq3 <- seq(100,((rows17-2)*256)+100, by=256)
subDF3 <- data.frame(cols=rep(colsSeq3,times=length(rowsSeq3)),
                     rows=rep(rowsSeq3,each=length(colsSeq3)))

#subdivide raster into 256 x 256
for(i in 1:nrow(subDF3)){
  writeRaster(crop(u17a, extent(u17a,  subDF3$rows[i], 
                                subDF3$rows[i]+255,
                                subDF3$cols[i], 
                                subDF3$cols[i]+255)),  
              paste0(dirI[dirN],"/image_3/predict_",i,".tif"),
              format="GTiff")
  
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
