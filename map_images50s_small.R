library(sf)
library(raster)
library(mapview)
library(mapedit)


#directory of training images
dirO <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica")
#directory for masks
dirM <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica/50s_small_mask")

#### read in data and visualize ----
#read in data from 1950s
#georeferenced in ArcPro to the ESRI world imagery basemap
# crs is in web mercator since that is automatic base map CRS.
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


u50a@nrows*u50a@ncols/(256*256)

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

#additional samples: 40-60
nSamp3 <- 60
set.seed(42)
samplesx3 <- sample(1:(u50a@ncols-257), nSamp3)[41:60]
set.seed(12)
samplesy3 <- sample(1:(u50a@nrows-257), nSamp3)[41:60]

#validation samples: 60-80
nSamp4 <- 80
set.seed(42)
samplesx4 <- sample(1:(u50a@ncols-257), nSamp4)[61:80]
set.seed(12)
samplesy4 <- sample(1:(u50a@nrows-257), nSamp4)[61:80]

nSamp5 <- 120
set.seed(42)
samplesx5 <- sample(1:(u50a@ncols-257), nSamp5)[81:120]
set.seed(12)
samplesy5 <- sample(1:(u50a@nrows-257), nSamp5)[81:120]


# save data, commented out since does not need to run every time
 
   # 
   # for(i in 1:40){
   #  
   #   
   #   writeRaster(crop(u50a, extent(u50a, samplesy5[i], 
   #                               samplesy5[i] +127, 
   #                                 samplesx5[i], 
   #                               samplesx5[i]+127)), 
   #             paste0("/Volumes/GoogleDrive/My Drive/research/projects/utica/50s_train_small/train_",i+80,".tif"),
   #             format="GTiff" ,overwrite=TRUE)
   #  
   # 
   # }

#### make masks for training & validation ----


#### Step 1: read in image   ##

#give training image number
trainNum <- 97

imgN <- raster(paste0("/Volumes/GoogleDrive/My Drive/research/projects/utica/50s_train_small/train_",trainNum,".tif"))
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

buildings <- drawFeatures(mapview(trainDgc, col=grey(1:100/100)))

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


###### Prep for prediction ----

#reproject to be in WGS 1984 so matches all mask images

u50rp <- projectRaster(u50a,  crs="+init=epsg:4326")
plot(u50rp, col=gray(1:100/100))

cols50 <- floor(u50rp@ncols/256) 
rows50 <- floor(u50rp@nrows/256) 

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
