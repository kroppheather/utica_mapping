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

lc1957 <- 