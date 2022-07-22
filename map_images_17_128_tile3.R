library(sf)
library(raster)
library(mapview)
library(mapedit)

dirN <- 2
#directory of training images
dirO <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica/model_save/2017/data_128/train/images",
          "e:/Google Drive/research/projects/utica/model_save/2017/data_128/train/images")


#directory for masks
dirM <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica/model_save/2017/data_128/train/mask",
          "e:/Google Drive/research/projects/utica/model_save/2017/data/train_128/mask")

# dirMV <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica/model_save/2017/data/valid",
#           "e:/Google Drive/research/projects/utica/model_save/2017/data/valid" )

dirI <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica/model_save/2017/img_tile_128",
          "e:/Google Drive/research/projects/utica/model_save/2017/img_tile_128")

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
#u17a <- stack("/Volumes/GoogleDrive/My Drive/research/projects/utica/utica17/u2017_crop.tif")
plotRGB(u17a)



u17a@nrows*u17a@ncols/(256*256)

#### subset raster for training ----


###### Prep for prediction ----




dir256 <- "E:/Google Drive/research/projects/utica/model_save/2017/img_tile_256/image_3"


Nimg <- 7740
imgT <- stack(paste0(dir256, "/predict_1.tif"))
imgT1 <- crop(imgT, extent(imgT, 1,
                                128,
                                1,
                                128))

imgT2 <- crop(imgT, extent(imgT, 129,
                           256,
                           1,
                           128))

imgT3 <- crop(imgT, extent(imgT, 1,
                           128,
                           129,
                           256))

imgT4 <- crop(imgT, extent(imgT, 129,
                           256,
                           129,
                           256))


writeRaster(imgT1, paste0(dirI[dirN], "/image_3/predict_", ((1*4)-3), ".tif"), 
            format="GTiff")
writeRaster(imgT2, paste0(dirI[dirN], "/image_3/predict_", ((1*4)-2), ".tif"), 
            format="GTiff")
writeRaster(imgT3, paste0(dirI[dirN], "/image_3/predict_", ((1*4)-1), ".tif"), 
            format="GTiff")
writeRaster(imgT4, paste0(dirI[dirN], "/image_3/predict_", ((1*4)), ".tif"), 
            format="GTiff")



for(i in 2:Nimg){

  imgT <- stack(paste0(dir256, "/predict_", i,".tif"))

  imgT1 <- crop(imgT, extent(imgT, 1,
                            128,
                            1,
                            128))

  imgT2 <- crop(imgT, extent(imgT, 129,
                           256,
                           1,
                           128))

  imgT3 <- crop(imgT, extent(imgT, 1,
                          128,
                           129,
                           256))

  imgT4 <- crop(imgT, extent(imgT, 129,
                           256,
                           129,
                           256))

  writeRaster(imgT1, paste0(dirI[dirN], "/image_3/predict_", ((i*4)-3), ".tif"), 
              format="GTiff")
  writeRaster(imgT2, paste0(dirI[dirN], "/image_3/predict_", ((i*4)-2), ".tif"), 
              format="GTiff")
  writeRaster(imgT3, paste0(dirI[dirN], "/image_3/predict_", ((i*4)-1), ".tif"), 
              format="GTiff")
  writeRaster(imgT4, paste0(dirI[dirN], "/image_3/predict_", ((i*4)), ".tif"), 
              format="GTiff")

}

