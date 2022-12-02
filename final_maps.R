# try updating newer analysis to terra
library(sf)
library(raster)
library(tidycensus)
library(dplyr)


source("c:/Users/hkropp/Documents/census_key.r")
census_api_key(key)

#total population, 2 white alone, 3, black alone , 4= native american alone, 5 = asian alone, 8 = two or more races
race <- get_acs("tract", state="36", county="065",
                variables = c("B02001_001","B02001_002", "B02001_003", "B02001_004", "B02001_005", "B02001_008"),
                year=2020, geometry=TRUE, output="wide")


names(race)

colnames(race) <- c("GEOID", "NAME", "total", "totalME",
                    "white", "whiteME","black", "blackME",
                    "NativeA", "NativeME","Asian","AsianME",
                    "biracial","biraciaME", "geometry")
#
# median household income in dollars
income_house <- get_acs("tract", state="36", county="065",
                        variables = c("B19013_001"),year=2020, geometry=TRUE,output="wide")

names(income_house)
colnames(income_house) <- c("GEOID", "NAME", "med_income","med_incMOE","geometry")

dp_vars <- load_variables(2020, "acs5/profile", cache = TRUE) 

rental_info <- get_acs("tract", state="36", county="065",
                       variables = c("DP04_0002", # total occupied units
                                     "DP04_0002P", # total occupied units as percent of tota
                                     "DP04_0046", # owner occupied
                                     "DP04_0046P", # owner occupied
                                     "DP04_0047", # renter occupied
                                     "DP04_0047P" # renter occupied
                                     ),year=2020, geometry=TRUE, output="wide")

names(rental_info)

colnames(rental_info) <- c("GEOID", "NAME","totalOcc",
                           "totalMOE","totalOccP","totalOccPM",
                           "totalOwn", "totalOwnM", "OwnP","OwnPM",
                           "totalRent","totalRentM","RentP","RentPM","geometry")

plot(income_house["estimate"])

income_houseSP <- st_transform(income_house,32116 )


rental_infoSP <- st_transform(income_house,32116 )


raceSP <- st_transform(race,32116 )


plot(income_houseSP["estimate"])

st_write(income_houseSP, 
         "E:/Google Drive/research/projects/utica/maps_save/census/Oneida_income_2020.shp")

st_write(rental_infoSP, 
         "E:/Google Drive/research/projects/utica/maps_save/census/Oneida_rental_2020.shp")

st_write(raceSP, 
         "E:/Google Drive/research/projects/utica/maps_save/census/Oneida_race_2020.shp")



lc1957s <- raster("E:/Google Drive/research/projects/utica/model_save/1950/all_maps/utica50s_strat3.tif")


lc50sp <- projectRaster(lc1957s, crs="+init=epsg:32116", method="ngb")

writeRaster(lc50sp, "E:/Google Drive/research/projects/utica/maps_save/lc_1957.tif",
            format="GTiff")

plot(lc50sp)
lc50sp
lc50p

lc2017s <-  raster("E:/Google Drive/research/projects/utica/model_save/2017/all_maps/utica17_strat.tif")

lc2017sp <- projectRaster(lc2017s, crs="+init=epsg:32116", method="ngb")
writeRaster(lc17sp, "E:/Google Drive/research/projects/utica/maps_save/lc_2017.tif",
            format="GTiff")


# pull out just trees
trees57 <- reclassify(lc1957, rcl=matrix(c(0,NA,
                              1,1,
                              2,NA,
                              3,NA), ncol=2, byrow=TRUE))
# pull out just trees
trees57s <- reclassify(lc1957s, rcl=matrix(c(0,NA,
                                           1,1,
                                           2,NA,
                                           3,NA), ncol=2, byrow=TRUE))


plot(trees57s)


trees17 <- reclassify(lc2017, rcl=matrix(c(0,NA,
                                         1,1,
                                         2,NA,
                                         3,NA), ncol=2, byrow=TRUE))

trees17s <- reclassify(lc2017s, rcl=matrix(c(0,NA,
                                           1,1,
                                           2,NA,
                                           3,NA), ncol=2, byrow=TRUE))

plot(trees17)


# writeRaster(trees57s, "E:/Google Drive/research/projects/utica/model_save/1950/all_maps/Utrees57_strat2.tif",
            format="GTiff")

writeRaster(trees17, "E:/Google Drive/research/projects/utica/model_save/2017/all_maps/Utrees17_256.tif",
            format="GTiff")
writeRaster(trees17s, "E:/Google Drive/research/projects/utica/model_save/2017/all_maps/Utrees17_strat.tif",
            format="GTiff")
