library(raster)
library(sf)
library(dplyr)
library(tmap)

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
##### Land surface anom calc -----

# land surface temperature
# focus on 2017 extent
tempRP <- projectRaster(tempC, crs="+init=epsg:32116")
tempRs <- crop(tempRP, lc17)



dayMean <- cellStats(tempRs, stat='mean', na.rm=TRUE)


ST_diff <- list()
for(i in 1:10){
  ST_diff[[i]] <- tempRs[[i]] - dayMean[i]
}

ST_diffs <- stack(ST_diff)
meanNA <- function(x){mean(x, na.rm=TRUE)}

ST_anom <- calc(ST_diffs, meanNA)
plot(ST_anom)

ST_quant <- quantile(getValues(ST_anom), prob=seq(0,1,by=0.01), na.rm=TRUE)

quant_filter <- function(x){
  ifelse(x <= ST_quant[2] | x>= ST_quant[99], NA, x)
}

Temp_Anom <- calc(ST_anom, quant_filter)



#######################################
##### Resampling    -----
lc17Crop <- crop(lc17, lc57)

lc57RS <- resample(lc57,lc17Crop, method="ngb" )
plot(lc57RS)
plot(lc17Crop)

#######################################
##### L -----

# land cover viz

help(overlay)

trees57R <- reclassify(lc57RS, rcl=matrix(c(0,0,
                                              1,1,
                                              2,0,
                                              3,0), ncol=2, byrow=TRUE))

trees17R <- reclassify(lc17Crop, rcl=matrix(c(0,0,
                                              1,1,
                                              2,0,
                                              3,0), ncol=2, byrow=TRUE))

treeComp <- function(x,y){
  ifelse(x == 1 & y == 1,1, # always tree cover
         ifelse(x == 1 & y == 0, 2, # loss tree cover
                ifelse(x == 0 & y == 1, 3, # gain
                 ifelse(x == 0 & y == 0,4,0)))) # always other

}

is.na(test[1])

test <- getValues(trees57R)

test2 <- getValues(trees17R)

test3 <- treeComp(test,test2)

dfCheck <- data.frame(test,test2,test3)

dfCheck[1:15,]

plot(trees57R)
plot(trees17R)

treeChange <- overlay(trees57R, trees17R, fun=treeComp)
plot(treeChange)

tm_shape(treeChange)+
  tm_raster(breaks=c(0,1.5,2.5,3.5))
# land cover change



# patterns with census data

# land cover change by tract

# 2017 land cover by tract

# temperature vs 2017 tree cover

# 2017 tree cover vs income

# 2017 tree cover vs renting

# 2017 tree cover vs race

# land cover change patterns

