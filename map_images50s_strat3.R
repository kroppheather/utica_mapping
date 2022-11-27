library(sf)
library(raster)
library(mapview)
library(mapedit)
library(dplyr)

#directory of training images
dirO <- c("e:/Google Drive/research/projects/utica/model_save/1950/data_strat3/image")
#directory for masks
dirM <- c("e:/Google Drive/research/projects/utica/model_save/1950/data_strat3/mask")

#### read in data and visualize ----
#read in data from 1950s
#georeferenced in ArcPro to the ESRI world imagery basemap
# crs is in web mercator since that is automatic base map CRS.
r50s <- raster("e:/Google Drive/research/projects/utica/A550500171317_ref.tif")
r50s@crs
plot(r50s, col=gray(1:100/100))

# polygons to stratify training data
bound_poly <- st_read("e:/Google Drive/research/projects/utica/strat/bound_strat2_50.shp")
bound_polyPr <- st_transform(bound_poly, st_crs(r50s))
#create ID field
bound_polyPr$Loc <- as.factor(c("field","commercial"))
bound_polyPr$LocID <- as.numeric(bound_polyPr$Loc)

#look at a few areas near the city center to start
Ucenter <- extent(-8377200,-8373500,
                  5326000,5328800)

Ucenter2 <- extent(-8382000,-8373500,
                  5324000,5329000)
#start with working with a small area in the center of
#utica 
u50a <- crop(r50s,Ucenter2)
plot(u50a, col=gray(1:100/100))


u50a@nrows*u50a@ncols/(256*256)

# rasterize poly
polyRast <- rasterize(st_zm(bound_polyPr), u50a, field="LocID")
plot(polyRast)

matPoly <- getValues(polyRast)
polyRast@nrows
polyRast@ncols

polyDF <- na.omit(data.frame(LocID = matPoly,
                     rowID = rep(seq(1, polyRast@nrows), each=polyRast@ncols),
                     colID = rep(seq(1, polyRast@ncols), times=polyRast@nrows)))

rowIDS <- list()
randRows <- list()
seedi <- c(40,1000)
dfSub <- list()
for(i in 1:length(unique(polyDF$LocID))){
   rowIDS[[i]] <- polyDF[polyDF$LocID == i,]
   set.seed(seedi[i])
   randRows[[i]] <- sample(1:nrow(rowIDS[[i]]), 10)
   dfSub[[i]] <- rowIDS[[i]][randRows[[i]],]
}

dfSubA <- do.call( "rbind", dfSub)
635248  735671  476598 1216856  956559  607138 1562125 2006408 1719638 1137437
#### subset raster for training ----
u50a

# subset size
# 128 x 128 pixels
#for training masks
#convert to matrix

test <- imgV[1:128,1:128]







# save data, commented out since does not need to run every time
 
      
      # for(i in 1:20){
      #  
      #   
      #   writeRaster(crop(u50a, extent(u50a, dfSubA$rowID[i], 
      #                                dfSubA$rowID[i] +127, 
      #                                 dfSubA$colID[i], 
      #                                 dfSubA$colID[i]+127)), 
      #             paste0("e:/Google Drive/research/projects/utica/model_save/1950/data_strat3/image/train_",i,".tif"),
      #             format="GTiff" ,overwrite=TRUE)
      #  
      # 
      # }

#### make masks for training & validation ----


#### Step 1: read in image   ##

#give training image number
trainNum <- 11

imgN <- raster(paste0("e:/Google Drive/research/projects/utica/model_save/1950/data_strat3/image/train_",trainNum,".tif"))
plot(imgN, col=grey(1:100/100))

imgN@ncols
imgN@nrows
#reproject to WGS 84 for mapedit
trainDgc <- projectRaster(imgN, crs="+init=epsg:4326")
plot(trainDgc, col=grey(1:100/100))
writeRaster(trainDgc, paste0(dirM,"/u_train_reproject/wgs_train_",trainNum,".tif"),
            format="GTiff")

trainDgc@nrows
trainDgc@ncols
#check images that were made:
# 
# treeM <- raster(paste0("/Volumes/GoogleDrive/My Drive/research/projects/utica/50s_small_mask/trees/tree_mask_",trainNum,".tif"))
# buildM <- raster(paste0("/Volumes/GoogleDrive/My Drive/research/projects/utica/50s_small_mask/building/building_mask_",trainNum,".tif"))
# paveM <- raster(paste0("/Volumes/GoogleDrive/My Drive/research/projects/utica/50s_small_mask/pavement/pavement_mask_",trainNum,".tif"))
# 
# plot(trainDgc, col=grey(1:100/100))

# plot(treeM)
# plot(buildM)
# plot(paveM)

# use zoom 18-21
# avoid features that are not clearly identifiable
# do not label shadows as any feature
# only count immediate coverage of the surface. For example
# a tree canopy clearly extending over the street gets
# counted as a tree not street since it is the object that is
# directly observed.
#### Step 2 make trees mask   ##

# draw tree polygon
trees <- drawFeatures(mapview(trainDgc, col=grey(1:100/100)))
# convert to raster
treeMask <- rasterize(trees,trainDgc, field=1, background=0)
# check that it looks good
plot(treeMask)
# save mask to file
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


