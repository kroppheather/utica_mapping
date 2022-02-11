# packages
library(sf)
library(raster)
library(mapview)
library(mapedit)

####################
## User input:    ##
## set file path  ##
####################

# training images to read in (separate folder)
dirImg <- "/Volumes/GoogleDrive/My Drive/research/projects/utica/50s_train_small"
# place to save mask images:
# this space should have 4 folders:
# 1. u_train_reproject, 2. tree, 

dirMask <- "/Volumes/GoogleDrive/My Drive/research/projects/utica/50s_mask_small"


################################
#### Step 1: read in image   ##
################################

####################
## User input:    ##
## change # for   ##
## each image     ##
####################

#give training image number 81-120
trainNum <- 81


#read in image 
imgN <- raster(paste0(dirImg,"/train_",trainNum,".tif"))

# check that image looks right
plot(imgN, col=grey(1:100/100))
#reproject to WGS 84 for mapedit
trainDgc <- projectRaster(imgN, crs="+init=epsg:4326")

writeRaster(trainDgc, paste0(dirMask,"/u_train_reproject/wgs_train_",trainNum,".tif"),
            format="GTiff")

#### Instructions for making masks   ##
# use zoom 18-21
# avoid features that are not clearly identifiable
# do not label shadows as any feature
# only count immediate coverage of the surface. For example
# a tree canopy clearly extending over the street gets
# counted as a tree not street since it is the object that is
# directly observed.

################################
#### Step 2 make trees mask   ##
################################

# draw tree polygon
trees <- drawFeatures(mapview(trainDgc, col=grey(1:100/100)))
# convert to raster
treeMask <- rasterize(trees,trainDgc, field=1, background=0)
# check that it looks good
plot(treeMask)
# save mask to file
writeRaster(treeMask, paste0(dirMask,"/trees/tree_mask_",trainNum,".tif"),
            format="GTiff")

####################################
#### Step 3 make buildings mask  ##
###################################

# make buildings mask and show tree outlines
buildings <- drawFeatures(mapview(trainDgc, col=grey(1:100/100))+
                            mapview(trees, col.regions="seagreen"))
# convert to raster
buildingMask <- rasterize(buildings,trainDgc, field=1, background=0)
# check the mask
plot(buildingMask)
# save to file
writeRaster(buildingMask, paste0(dirMask,"/building/building_mask_",trainNum,".tif"),
            format="GTiff")


####################################
#### Step 4 make pavement mask   ##
###################################
# draw pavement and show trees and buildings
pave <- drawFeatures(mapview(trainDgc, col=grey(1:100/100))+
                       mapview(trees, col.regions="seagreen")+
                       mapview(buildings, col.regions="tomato"))
# convert to raster
paveMask <- rasterize(pave,trainDgc, field=1, background=0)
# show in plot
plot(paveMask)
# save to file
writeRaster(paveMask, paste0(dirM,"/pavement/pavement_mask_",trainNum,".tif"),
            format="GTiff")

#########################################
#### Repeat, changing the img number  ##
########################################
