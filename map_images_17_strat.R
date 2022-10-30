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
trainNum <- 35

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

