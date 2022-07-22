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




dir256 <- "E:/Google Drive/research/projects/utica/model_save/2017/img_tile_256/image"


Nimg <- 7930


for(i in 1:Nimg){

imgT <- stack(paste0(dir256, "/predict_5.tif"))
plotRGB(imgT)
imgT1 <- crop(imgT, extent(imgT, 1,
                            128,
                            1,
                            128))
imgT1
plotRGB(imgT1)
imgT2 <- crop(imgT, extent(imgT, 129,
                           256,
                           1,
                           128))
imgT2
plotRGB(imgT2)

imgT3 <- crop(imgT, extent(imgT, 1,
                          128,
                           129,
                           256))
imgT3
plotRGB(imgT3)

imgT4 <- crop(imgT, extent(imgT, 129,
                           256,
                           129,
                           256))
imgT4
plotRGB(imgT4)

}
tilesNum 



cols17 <- floor(u17a@ncols/128) 
rows17 <- floor(u17a@nrows/128) 


# break up into tiles
colsSeq <- seq(1,cols17*128, by=128)
rowsSeq <- seq(1,rows17*128, by=128)
subDF <- data.frame(cols=rep(colsSeq,times=length(rowsSeq)),
                    rows=rep(rowsSeq,each=length(colsSeq)))
#subdivide raster into 
sub17s <- list()
rowcount <- numeric()
colcount <- numeric()
#this will shave off extra off south and west 
for(i in 1:nrow(subDF)){
  writeRaster(crop(u17a, extent(u17a,  subDF$rows[i], 
                                   subDF$rows[i]+127,
                                   subDF$cols[i], 
                                   subDF$cols[i]+127)),  
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
