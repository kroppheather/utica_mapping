library(raster)
library(sf)
library(dplyr)


#######################################
##### Read in data -----


dirI <- "E:/Google Drive/research/projects/utica/maps_save"

# read in land cover
lc57 <- raster(paste0(dirI,"/lc_1957.tif"))
lc17 <- raster(paste0(dirI,"/lc_2017.tif"))

# read in temperature
tempC <- stack(paste0(dirI, "/dailyTemp.tif"))

# read in census

renter <- st_read(paste0(dirI, "/census/Oneida_rental_2020.shp"))
race <-  st_read(paste0(dirI, "/census/Oneida_race_2020.shp"))
income <- st_read(paste0(dirI, "/census/Oneida_income_2020.shp"))


#######################################
##### Merge data layers -----

# land surface temperature
# focus on 2017 extent
tempRP <- projectRaster()
tempRs <- resample(tempC, lc17)



#######################################
##### L -----

# land cover viz





# land cover change



# patterns with census data

# land cover change by tract

# 2017 land cover by tract

# temperature vs 2017 tree cover

# 2017 tree cover vs income

# 2017 tree cover vs renting

# 2017 tree cover vs race

# land cover change patterns

