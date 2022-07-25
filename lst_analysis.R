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
plot(lc50)
lc50p <- projectRaster(lc50, crs="+init=epsg:32618", method="ngb")

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
# landsat between 2015 -2022 mid May - Sept 
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
  STK[[i]] <- crop(STM[[i]],extent(475000, 483000, 4768000, 4775000 ))
  STQA[[i]] <- crop(STQM[[i]],extent(475000, 483000, 4768000, 4775000 ))
}

plot(STK[[1]])


  
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

ST_st <- stack(ST_resamp)

plot(ST_st)


dayMean <- cellStats(ST_st, stat='mean', na.rm=TRUE)


ST_diff <- list()
for(i in 1:length(dirs)){
  ST_diff[[i]] <- ST_st[[i]] - dayMean[i]
}

ST_diffs <- stack(ST_diff)
meanNA <- function(x){mean(x, na.rm=TRUE)}

ST_anom <- calc(ST_diffs, meanNA)

plot(ST_anom)


treeCol1 <- rgb(0.13,0.54,0.13)
paveCol1 <- rgb(0.96,0.49,0)
buildCol1 <- rgb(0.53,0.17,0.09)
plot(lc50p, breaks=c(-0.1,0.5,#breaks between other
                        1.5, # tree
                        2.5, # building
                        3.5 ), #pavement
     col=c(NA, treeCol1,buildCol1, paveCol1),add=TRUE, legend=FALSE, box=FALSE)





plot(onc$geometry)
onc$Tract <- as.numeric(onc$TRACTCE)

ottr <- rasterize(onc,ST_anom, field="Tract")
plot(ottr)

stc <- zonal(ST_anom,ottr)
stc

stcA <- zonal(ST_st,ottr)
stcA

source("c:/Users/hkropp/Documents/census_key.r")
census_api_key(key)

#total population, 2 white alone, 3, black alone 
race <- get_acs("tract", state="36", county="065",
                variables = c("B02001_001","B02001_002", "B02001_003"),year=2020, geometry=TRUE)
#
# median household income in dollars
income_house <- get_acs("tract", state="36", county="065",
                variables = c("B19013_001"),year=2020, geometry=TRUE)
plot(income_house["estimate"])
income_housep <- st_transform(income_house,32618 )

inc_crop <- st_crop(income_housep, lc50p)
plot(inc_crop["estimate"])

temp_lc50 <- crop(ST_st, lc50p) 

plot(temp_lc50)

dayMeanLC <- cellStats(temp_lc50, stat='mean', na.rm=TRUE)


ST_diffLC <- list()
for(i in 1:length(dirs)){
  ST_diffLC[[i]] <- temp_lc50[[i]] - dayMeanLC[i]
}

ST_diffsLC <- stack(ST_diffLC)
meanNA <- function(x){mean(x, na.rm=TRUE)}

ST_anomLC <- calc(ST_diffsLC, meanNA)

inc_crop$Tract <- as.numeric(inc_crop$GEOID)

incomeRast <- rasterize(inc_crop, ST_anomLC, field="Tract")

incomeZonal <- zonal(ST_anomLC, incomeRast)

tempZone <- data.frame(Tract = incomeZonal[,1], 
                       Anom.C = incomeZonal[,2])

income_join <- left_join(inc_crop, tempZone, by="Tract")

plot(income_join["Anom.C"])
plot(income_join["estimate"])

plot(income_join$estimate, income_join$Anom.C, 
     ylab="Temperature Anomaly (C)",
     xlab="Median household income ($)", pch=19)
plot(lc50p)

tree50 <- reclassify(lc50p, matrix(c(0,0,
                                     1,1,
                                     2,0,
                                     3,0), byrow=TRUE, ncol=2))
plot(tree50)  

incomeTree <- rasterize(inc_crop, tree50, field="Tract")


#line up 
  
tree50_zone <- raster::zonal(tree50, incomeTree, fun="sum"  )

tree50DF <- data.frame(Tract = tree50_zone[,1],
                       area_tree_m2 = tree50_zone[,2]*res(incomeTree)[1]*res(incomeTree)[2])

income_join2 <- left_join(inc_crop, tree50DF, by="Tract")
income_join2$area <- st_area(income_join2)

income_join2$percTree50 <- (income_join2$area_tree_m2/income_join2$area)*100

plot(income_join2["percTree50"])

plot(income_join2$estimate, income_join2$percTree50, 
     ylab="Percentage tree in 1950 (%)",
     xlab="Median household income ($)", pch=19)

# total population B01003 

# demographic and housing  DP05

pop <- get_acs("tract", state="36", county="065",
                        table = c("DP05"),year=2020, geometry=TRUE)
