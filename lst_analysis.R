library(raster)
library(sf)
library(dplyr)
library(tidycensus)

# directory
dirI <- "E:/Google Drive/GIS/landsat/utica/LC08_L2SP_015030_20160904_20200906_02_T1"

# ST in K
ST09_04_16 <- raster(paste0(dirI,"/LC08_L2SP_015030_20160904_20200906_02_T1_ST_B10.tif"))
ST09_04_16
plot(ST09_04_16)
# DN conversion
ST1 <- 0.00341802*ST09_04_16 + 149
plot(ST1)
STC <-ST1 -273.15
plot(STC)

# QA in K
ST09_04_16Q <- raster(paste0(dirI,"/LC08_L2SP_015030_20160904_20200906_02_T1_ST_QA.tif"))
plot(ST09_04_16Q)
STQK <- (0.01*ST09_04_16Q) 
plot(STQK)


QA <- stack(paste0(dirI,"/LC08_L2SP_015030_20160904_20200906_02_T1_QA_PIXEL.tif"))
plot(QA)
unique(getValues(QA))

freq(QA)

cloudf <- function(x){
  ifelse(x == 21824, 1,0)
}

QAf <- calc(QA,cloudf)
plot(QAf)

STM <- mask(STC, QAf, maskvalue=0)
plot(STM)

ctm <- st_read("E:/Google Drive/research/projects/utica/census/tl_2020_36_tract/tl_2020_36_tract.shp")
crmp <- st_transform(ctm, 32618)

onc <- crmp[crmp$COUNTYFP == "065",]

plot(onc$geometry)
onc$Tract <- as.numeric(onc$TRACTCE)

ottr <- rasterize(onc,STC, field="Tract")
plot(ottr)

stc <- zonal(STC,ottr)
stc

source("c:/Users/hkropp/Documents/census_key.r")
census_api_key(key)

#total population, 2 white alone, 3, black alone 
race <- get_acs("tract", state="36", county="065",
                variables = c("B02001_001","B02001_002", "B02001_003"),year=2020, geometry=TRUE)
#
# median household income in dollars
income_house <- get_acs("tract", state="36", county="065",
                variables = c("B19013_001"),year=2020, geometry=TRUE)
plot(income_house)

# total population B01003 

# demographic and housing  DP05

pop <- get_acs("tract", state="36", county="065",
                        table = c("DP05"),year=2020, geometry=TRUE)
