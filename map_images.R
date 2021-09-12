library(sf)
library(raster)
library(mapview)

trees <- read.csv("E:/Google Drive/GIS/utica/trees/trees_utica.csv")
treeS <- st_as_sf(trees, coords=c("Longitude","Latitude"),
                  crs=4326)



r50s <- raster("E:/Google Drive/GIS/utica/MyProject4/A550500171317_ref.tif")
r50s@crs
treeP <- st_transform(treeS, 3857)

plot(r50s, col=gray(1:100/100))
click()

Ucenter <- extent(-8376822,-8374033,
                  5326000, 5328949)
u50s <- crop(r50s,Ucenter)

treeCenter <- st_crop(treeP, xmin=-8376822,
                      ymin=5326000,
                     xmax= -8374033,
                     ymax= 5328949)


plot(u50s, col=gray(1:100/100))
plot(treeP$geometry, col="forestgreen", add=TRUE)

mapview(u50s, col=gray(1:100/100))+
  mapview(treeCenter, color="forestgreen")


