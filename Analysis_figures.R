########################################################
########################################################
##### Analysis and figures for utica mapping       #####
########################################################
########################################################

##### libraries -----
library(terra)
library(sf)
library(dplyr)

##### read in data -----
#in final maps folder:
# 1957: utica stratified sampling (fixed from overlay error in prediction file), 128 x128
# 1987: utica stratified sampling 128 x 128
# 1957: utica stratified sampling 256 x 256
# census shape files from the ACS 2020
# average summer land surface temperature from landsat collection 2 level 2
dirI <- "E:/Google Drive/research/projects/utica/maps_final"

lc1957 <- rast(paste0(dirI,"/lc_1957.tif"))
lc1987 <- rast(paste0(dirI,"/lc_1987.tif"))
lc2017 <- rast(paste0(dirI,"/lc_2017.tif"))