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


rental_infoSP <- st_transform(rental_info,32116 )


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
plot(lc2017sp)
lc2017sp
writeRaster(lc2017sp, "E:/Google Drive/research/projects/utica/maps_save/lc_2017.tif",
            format="GTiff")

# read


# pull out just trees

trees57s <- reclassify(lc50sp, rcl=matrix(c(0,NA,
                                           1,1,
                                           2,NA,
                                           3,NA), ncol=2, byrow=TRUE))


build57s <- reclassify(lc50sp, rcl=matrix(c(0,NA,
                                              1,NA,
                                              2,1,
                                              3,NA), ncol=2, byrow=TRUE))

pave57s <- reclassify(lc50sp, rcl=matrix(c(0,NA,
                                              1,NA,
                                              2,NA,
                                              3,1), ncol=2, byrow=TRUE))
plot(trees57s)
plot(build57s)
plot(pave57s)


trees17s <- reclassify(lc2017sp, rcl=matrix(c(0,NA,
                                         1,1,
                                         2,NA,
                                         3,NA), ncol=2, byrow=TRUE))
build17s <- reclassify(lc2017sp, rcl=matrix(c(0,NA,
                                              1,NA,
                                              2,1,
                                              3,NA), ncol=2, byrow=TRUE))
pave17s <- reclassify(lc2017sp, rcl=matrix(c(0,NA,
                                              1,NA,
                                              2,NA,
                                              3,1), ncol=2, byrow=TRUE))
plot(trees17s)
plot(build17s)
plot(pave17s)


writeRaster(trees17s,"E:/Google Drive/research/projects/utica/maps_save/trees17.tif",
            format="GTiff")
writeRaster(build17s,"E:/Google Drive/research/projects/utica/maps_save/build17.tif",
            format="GTiff")
writeRaster(pave17s,"E:/Google Drive/research/projects/utica/maps_save/pave17.tif",
            format="GTiff")




writeRaster(trees57s,"E:/Google Drive/research/projects/utica/maps_save/trees57.tif",
            format="GTiff")
writeRaster(build57s,"E:/Google Drive/research/projects/utica/maps_save/build57.tif",
            format="GTiff")
writeRaster(pave57s,"E:/Google Drive/research/projects/utica/maps_save/pave57.tif",
            format="GTiff")


### Land surface temperature


# directory
dirI <- "E:/Google Drive/GIS/landsat/utica"

dirs <- list.dirs(dirI, recursive=FALSE)
# get files in each directory
# can't stack since mix of L09 and L08
# landsat between 2015 -2022 mid May - Sept 
# with low cloud cover in the utica area
Lfiles <- list()
STraster <- list()
STQAraster <- list()
QAraster <- list()

for(i in 1:length(dirs)){
  Lfiles[[i]] <- list.files(dirs[i])
  STraster[[i]] <- raster(paste0(dirs[i], "/",Lfiles[[i]][grep("ST_B10.TIF", Lfiles[[i]])]))
  STQAraster[[i]] <- raster(paste0(dirs[i], "/",Lfiles[[i]][grep("ST_QA.TIF", Lfiles[[i]])]))
  QAraster[[i]] <- raster(paste0(dirs[i], "/",Lfiles[[i]][grep("QA_PIXEL.TIF", Lfiles[[i]])]))  
}




# take the clear value from the QA pixel to remove clouds

cloudf <- function(x){
  ifelse(x == 21824, 1,0)
}

QAf <- list()
STM <- list()
STQM <- list()
for(i in 1:length(dirs)){
  QAf[[i]] <- calc(QAraster[[i]],cloudf)
  STM[[i]] <- mask(STraster[[i]], QAf[[i]], maskvalue=0)
  STQM[[i]] <- mask(STQAraster[[i]], QAf[[i]], maskvalue=0)
}



# mask rasters by oneida county
STK <- list()
STQA <- list()
for(i in 1:length(dirs)){
  STK[[i]] <- crop(STM[[i]],extent(472500, 484000, 4768000, 4775000 ))
  STQA[[i]] <- crop(STQM[[i]],extent(472500, 484000, 4768000, 4775000 ))
}



# DN conversion
ST_C <- list()
ST_QA <- list()
ST_calc <- list()

for(i in 1:length(dirs)){
  ST_calc[[i]] <- (0.00341802*STK[[i]]) + 149
  ST_QA[[i]] <- (0.01*STQA[[i]]) 
  ST_C[[i]] <-  ST_calc[[i]]-273.15
}



#average all cells
plot(ST_calc[[2]])

# resample since L08 and L09 are present
ST_resamp <- list(ST_C[[1]])

for(i in 2:length(dirs)){
  ST_resamp[[i]] <- resample(ST_C[[i]], ST_C[[1]])
}



# resample since L08 and L09 are present
STQA_resamp <- list(ST_QA[[1]])

for(i in 2:length(dirs)){
  STQA_resamp[[i]] <- resample(ST_QA[[i]], ST_QA[[1]])
}

ST_st <- stack(ST_resamp)

STQA_st <- stack(STQA_resamp)
# IF the uncertainty is greater than or equal to 3 degrees remove
QAfunction <- function(x){
  ifelse(x >=3, 0, 1)
}

STQA_calc <- calc(STQA_st, QAfunction)

ST_qc <- mask(ST_st, STQA_calc , maskvalue=0)
plot(ST_qc)


writeRaster(ST_qc,"E:/Google Drive/research/projects/utica/maps_save/dailyTemp.tif",
            format="GTiff")
