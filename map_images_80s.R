library(sf)
library(raster)
library(mapview)
library(mapedit)


#directory of training images
dirO <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica")
#directory for masks
dirM <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica/80s_mask")

dirMV <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica/mask_50s_valid")

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
r80s <- raster("/Volumes/GoogleDrive/My Drive/research/projects/utica/utica80/utica80_10086.tif")
r80s@crs
plot(r80s, col=gray(1:100/100))

#start with working with a small area in the center of
#utica 
u80a <- crop(r80s,Ucenter3)
plot(u80a, col=gray(1:100/100))


u80rp <- projectRaster(u80a,  crs="+init=epsg:4326")
plot(u80rp, col=gray(1:100/100))
mapview(u80rp)
