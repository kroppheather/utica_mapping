library(sf)
library(raster)
library(mapview)
library(caret)
library(nnet)
library(keras)

trees <- read.csv("E:/Google Drive/GIS/utica/trees/trees_utica.csv")
treeS <- st_as_sf(trees, coords=c("Longitude","Latitude"),
                  crs=4326)



r50s <- raster("E:/Google Drive/GIS/utica/MyProject4/A550500171317_ref.tif")
r50s@crs
treeP <- st_transform(treeS, 3857)

plot(r50s, col=gray(1:100/100))
click()

Ucenter <- extent(-8377200,-8373500,
                  5326000,5328800)
u50s <- crop(r50s,Ucenter)


plot(u50s, col=gray(1:100/100))
plot(treeP$geometry, col="forestgreen", add=TRUE)

mapview(u50s, col=gray(1:100/100))#+
  #mapview(treeCenter, color="forestgreen")



treeP <- drawFeatures()+mapview(u50s, col=gray(1:100/100))

TreeT <- st_read("E:/Google Drive/GIS/utica/train/tree.shp")


StreetT <- st_read("E:/Google Drive/GIS/utica/train/streets.shp")

BuildingT <- st_read("E:/Google Drive/GIS/utica/train/building.shp")

otherT <- st_read("E:/Google Drive/GIS/utica/train/other_fixed.shp")

plot(u50s, col=gray(1:100/100))
plot(TreeT$geometry, col="red", add=TRUE)
plot(StreetT$geometry, col="red", add=TRUE)
plot(BuildingT$geometry, col="red", add=TRUE)
plot(otherT$geometry, col="red", add=TRUE)

#set up data frame that extracts all
#1 = tree
#2= street
#3 = building
#4 = other

#sampleID, 1 is train, 2 is valid

TreeT$classID <- rep(1, nrow(TreeT))
TreeT$sampleID <- rep(1, nrow(TreeT))
#sample half for valid
set.seed(12)
TreeT$sampleID[sort(sample(seq(1,nrow(TreeT)),60))] <- 2

StreetT$classID <- rep(2, nrow(StreetT))
StreetT$sampleID <- rep(1, nrow(StreetT))
#sample half for valid
set.seed(132)
StreetT$sampleID[sort(sample(seq(1,nrow(StreetT)),60))] <- 2

BuildingT$classID <- rep(3, nrow(BuildingT))
BuildingT$sampleID <- rep(1, nrow(BuildingT))
#sample half for valid
set.seed(132)
BuildingT$sampleID[sort(sample(seq(1,nrow(BuildingT)),60))] <- 2

otherT$classID <- rep(4, nrow(otherT))
otherT$sampleID <- rep(1, nrow(otherT))
#sample half for valid
set.seed(33)
otherT$sampleID[sort(sample(seq(1,nrow(otherT)),60))] <- 2

allPTS <- rbind(TreeT[,2:4],StreetT[,2:4],BuildingT[,2:4],otherT[,2:4])

trainPTs <- allPTS[allPTS$sampleID == 1,]
vaildPTs <- allPTS[allPTS$sampleID == 2,]


#extract training

#extract data
