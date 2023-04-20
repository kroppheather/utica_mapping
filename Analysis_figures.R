########################################################
########################################################
##### Analysis and figures for utica mapping       #####
########################################################
########################################################

##### libraries -----
library(terra)
library(sf)
library(dplyr)
library(caret)

##### read in data -----
#in final maps folder:
# other will be zero, trees =1, buildings =2, pavement =3
# 1957: utica stratified sampling (fixed from overlay error in prediction file), 128 x128
# 1987: utica stratified sampling 128 x 128
# 1957: utica stratified sampling 256 x 256

dirI <- "E:/Google Drive/research/projects/utica/maps_final"
# read in land cover predictions
lc1957 <- rast(paste0(dirI,"/lc_1957.tif"))
lc1987 <- rast(paste0(dirI,"/lc_1987.tif"))
lc2017 <- rast(paste0(dirI,"/lc_2017.tif"))

# census shape files from the ACS 2020
# average summer land surface temperature from landsat collection 2 level 2

# read in validation data points

valid57build <- vect("E:/Google Drive/research/projects/utica/model_save/1950/valid_pt_2000/valid_50_build.shp")
valid57pave <- vect("E:/Google Drive/research/projects/utica/model_save/1950/valid_pt_2000/valid_50_pave.shp")
valid57other <- vect("E:/Google Drive/research/projects/utica/model_save/1950/valid_pt_2000/valid_50_other.shp")
valid57tree <- vect("E:/Google Drive/research/projects/utica/model_save/1950/valid_pt_2000/valid_50_tree.shp")

valid87build <- vect("E:/Google Drive/research/projects/utica/model_save/1980/valid_pt_2000/valid_80_building.shp")
valid87pave <- vect("E:/Google Drive/research/projects/utica/model_save/1980/valid_pt_2000/valid_80_pave.shp")
valid87other <- vect("E:/Google Drive/research/projects/utica/model_save/1980/valid_pt_2000/valid_80_other.shp")
valid87tree <- vect("E:/Google Drive/research/projects/utica/model_save/1980/valid_pt_2000/valid_80_tree.shp")




valid17build <- vect("E:/Google Drive/research/projects/utica/model_save/2017/valid_pt_2000/valid_17_build.shp")
valid17pave <- vect("E:/Google Drive/research/projects/utica/model_save/2017/valid_pt_2000/valid_17_pave.shp")
valid17other <- vect("E:/Google Drive/research/projects/utica/model_save/2017/valid_pt_2000/valid_17_other.shp")
valid17tree <- vect("E:/Google Drive/research/projects/utica/model_save/2017/valid_pt_2000/valid_17_tree.shp")


#### extract validation data and set up accuracy measures ----

other17extract <- extract(lc2017,project(valid17other,lc2017))
other17extract$actual <- rep(0,nrow(other17extract)) 

tree17extract <- extract(lc2017,project(valid17tree,lc2017))
tree17extract$actual <- rep(1,nrow(tree17extract)) 

build17extract <- extract(lc2017,project(valid17build,lc2017))
build17extract$actual <- rep(2,nrow(build17extract)) 

pave17extract <- extract(lc2017,project(valid17pave,lc2017))
pave17extract$actual <- rep(3,nrow(pave17extract)) 

valid17m <- na.omit(rbind(other17extract,tree17extract,build17extract,pave17extract))

conf_17 <- confusionMatrix(as.factor(valid17m$lc_2017),as.factor(valid17m$actual))
conf_17$table
conf_17$overall

other_UA_17 <- conf_17$table[1,1]/sum(conf_17$table[,1])
other_PA_17 <- conf_17$table[1,1]/sum(conf_17$table[1,])

tree_UA_17 <-  conf_17$table[2,2]/sum(conf_17$table[,2])
tree_PA_17 <-  conf_17$table[2,2]/sum(conf_17$table[2,])
