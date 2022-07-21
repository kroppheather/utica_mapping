library(raster)
library(sf)
library(dplyr)
library(tidycensus)

# census boundary
ctm <- st_read("E:/Google Drive/research/projects/utica/census/tl_2020_36_tract/tl_2020_36_tract.shp")
crmp <- st_transform(ctm, 32618)

onc <- crmp[crmp$COUNTYFP == "065",]

#land cover data

lc50 <- raster("E:/Google Drive/research/projects/utica/model_save/1950/all_maps/utica50s_128.tif")
lc50p <- projectRaster(lc50, crs="+init=epsg:32618")

# directory
dirI <- "E:/Google Drive/GIS/landsat/utica"

dirs <- list.dirs(dirI, recursive=FALSE)

# get dates
dirsDates <- character()

for(i in 1:length(dirs)){
  dirsDates[i] <- strsplit(dirs[i], "_" )[[1]][4]
}

# get files in each directory
# can't stack since mix of L09 and L08
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
for(i in 1:length(dirs)){
  QAf[[i]] <- calc(QAraster[[i]],cloudf)
  STM[[i]] <- mask(STraster[[i]], QAf[[i]], maskvalue=0)
}

# mask rasters by oneida county
STK <- list()

for(i in 1:length(dirs)){
  STK[[i]] <- crop(STM[[i]],extent(475000, 483000, 4768000, 4775000 ))
}
  
  

plot(STK[[9]], col=grey(1:100/100))

dirs[4]
dirs[9]

treeCol1 <- rgb(0.13,0.54,0.13)
paveCol1 <- rgb(0.96,0.49,0)
buildCol1 <- rgb(0.53,0.17,0.09)
plot(lc50p, breaks=c(-0.1,0.5,#breaks between other
                        1.5, # tree
                        2.5, # building
                        3.5 ), #pavement
     col=c(NA, treeCol1,buildCol1, paveCol1),add=TRUE, legend=FALSE, box=FALSE)


STM <- mask(STC, QAf, maskvalue=0)
plot(STM)

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
