library(sf)
library(raster)
library(mapview)

dirO <- "E:\\Google Drive\\GIS\\utica\\u_train"

#### read in data and visualize ----
trees <- read.csv("E:/Google Drive/GIS/utica/trees/trees_utica.csv")
treeS <- st_as_sf(trees, coords=c("Longitude","Latitude"),
                  crs=4326)


r50s <- raster("E:/Google Drive/GIS/utica/MyProject4/A550500171317_ref.tif")
r50s@crs
plot(r50s, col=gray(1:100/100))
Ucenter <- extent(-8377200,-8373500,
                  5326000,5328800)

Ucenter2 <- extent(-8382000,-8373500,
                  5324000,5329000)

u50a <- crop(r50s,Ucenter2)
plot(u50a, col=gray(1:100/100))

plot(u50s, col=gray(1:100/100))
plot(treeP$geometry, col="forestgreen", add=TRUE)

mapview(u50a, col=gray(1:100/100))







#### subset raster for training ----
u50a


# subset size
# 128 x 128 pixels
#convert to matrix



test <- imgV[1:128,1:128]

nSamp <- 20
set.seed(42)
samplesx <- sample(1:(u50a@ncols-257), nSamp)
set.seed(12)
samplesy <- sample(1:(u50a@nrows-257), nSamp)

uSubs <- crop(u50a, extent(u50a, samplesy[1], 
                           samplesy[1] +255, 
                           samplesx[1], 
                           samplesx[1]+255))

uSubs
plot(uSubs)
writeRaster(uSubs, paste0(dirO,"test.tif"),
            format="GTiff")

which(is.na(getValues(uSubs)))

for(i in 1:nSamp){
  
  
  writeRaster(crop(u50a, extent(u50a, samplesy[i], 
                                samplesy[i] +256, 
                                samplesx[i], 
                                samplesx[i]+256)), 
              paste0(dirO, "\\train_",i,".tif"),
             format="GTiff" )
  

}


plot(uSubs[[10]], col=gray(1:100/100))

uSubs <- crop(u50a, extent(u50a, samplesy[1], 
                                samplesy[1] +256, 
                                samplesx[1], 
                                samplesx[1]+256))


test <- raster(paste0(dirO, "/train_",3,".tif"))
plot(test)
