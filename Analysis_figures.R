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
library(lmtest)
library(BAMMtools)

##### saving directory ----
dirSave <- "E:/Google Drive/research/projects/utica/manuscript/figures"

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


# read in original images

img57 <- rast("E:/Google Drive/research/projects/utica/utica50/A550500171317_ref.tif")
img87 <- rast("E:/Google Drive/research/projects/utica/model_save/1980/all_maps/utica80s_crop_orig.tif")
img17 <- rast("E:/Google Drive/research/projects/utica/utica17/u2017_crop.tif")

# lc1987 is still in WGS84 project to state plane

lc1987 <- project(lc1987,"+init=epsg:32116", method="near")

img57 <-  project(img57,"+init=epsg:32116", method="near")
img87 <-  project(img87,"+init=epsg:32116", method="near")
img17 <-  project(img17,"+init=epsg:32116", method="near")
img57@ptr$res
img87@ptr$res
img17@ptr$res

# census shape files from the ACS 2020

income <-vect("E:/Google Drive/research/projects/utica/maps_final/census/Oneida_income_2020.shp")
rental <- vect("E:/Google Drive/research/projects/utica/maps_final/census/Oneida_rental_2020.shp")
income.sf <-st_read("E:/Google Drive/research/projects/utica/maps_final/census/Oneida_income_2020.shp")
rental.sf <- st_read("E:/Google Drive/research/projects/utica/maps_final/census/Oneida_rental_2020.shp")

# average summer land surface temperature from landsat collection 2 level 2
tempC <- rast("E:/Google Drive/research/projects/utica/maps_final/dailyTemp.tif")
tempCp <-  project(tempC,"+init=epsg:32116")

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


### 2017
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

## 1987

other87extract <- extract(lc1987,project(valid87other,lc1987))
other87extract$actual <- rep(0,nrow(other87extract)) 

tree87extract <- extract(lc1987,project(valid87tree,lc1987))
tree87extract$actual <- rep(1,nrow(tree87extract)) 

build87extract <- extract(lc1987,project(valid87build,lc1987))
build87extract$actual <- rep(2,nrow(build87extract)) 

pave87extract <- extract(lc1987,project(valid87pave,lc1987))
pave87extract$actual <- rep(3,nrow(pave87extract)) 

valid87m <- na.omit(rbind(other87extract,tree87extract,build87extract,pave87extract))

conf_87 <- confusionMatrix(as.factor(valid87m$lc_1987),as.factor(valid87m$actual))


conf_87$table
conf_87$overall

other_UA_87 <- conf_87$table[1,1]/sum(conf_87$table[,1])
other_PA_87 <- conf_87$table[1,1]/sum(conf_87$table[1,])

tree_UA_87 <-  conf_87$table[2,2]/sum(conf_87$table[,2])
tree_PA_87 <-  conf_87$table[2,2]/sum(conf_87$table[2,])


## 1957

other57extract <- extract(lc1957,project(valid57other,lc1957))
other57extract$actual <- rep(0,nrow(other57extract)) 

tree57extract <- extract(lc1957,project(valid57tree,lc1957))
tree57extract$actual <- rep(1,nrow(tree57extract)) 

build57extract <- extract(lc1957,project(valid57build,lc1957))
build57extract$actual <- rep(2,nrow(build57extract)) 

pave57extract <- extract(lc1957,project(valid57pave,lc1957))
pave57extract$actual <- rep(3,nrow(pave57extract)) 

valid57m <- na.omit(rbind(other57extract,tree57extract,build57extract,pave57extract))

conf_57 <- confusionMatrix(as.factor(valid57m$lc_1957),as.factor(valid57m$actual))


##### Organize land cover data for maps ----
plot(lc2017, xlim=c(351000,362000))
plot(lc1957,add=TRUE)
plot(lc1987, add=TRUE)

ext(lc1957)
ext(lc1987)
ext(lc2017)

#compare area that overlaps in all images (accounts for angle of some images with missing data)
overlapExt <- ext(356825,360870, 342870,346415)
lc2017_crop <- crop(lc2017,overlapExt, snap="near")
lc1987_crop <- crop(lc1987,overlapExt, snap="near")
lc1957_crop <- crop(lc1957,overlapExt, snap="near")


img17_crop <- crop(img17,overlapExt, snap="near")
img57_crop <- crop(img57,overlapExt, snap="near")
img87_crop <- crop(img87,overlapExt, snap="near")
plot(img57_crop, col=grey(1:100/100))
# count areas


l57count <- freq(lc1957_crop)

area57DF <- data.frame(l57count)

area57DF$area.m2 <- area57DF$count * res(lc1957_crop)[1]*res(lc1957_crop)[2]
area57DF$area.km2 <- area57DF$area.m2*1e-6


l87count <- freq(lc1987_crop)

area87DF <- data.frame(l87count)

area87DF$area.m2 <- area87DF$count * res(lc1987_crop)[1]*res(lc1987_crop)[2]
area87DF$area.km2 <- area87DF$area.m2*1e-6


l17count <- freq(lc2017_crop)

area17DF <- data.frame(l17count)

area17DF$area.m2 <- area17DF$count * res(lc2017_crop)[1]*res(lc2017_crop)[2]
area17DF$area.km2 <- area17DF$area.m2*1e-6



#### Tree change calculation ----

lc1957rs <- resample(lc1957_crop, lc2017_crop, method="near")

trees57R <- classify(lc1957rs, rcl=matrix(c(0,0,
                                            1,1,
                                            2,0,
                                            3,0), ncol=2, byrow=TRUE))

trees17R <- classify(lc2017_crop, rcl=matrix(c(0,0,
                                               1,1,
                                               2,0,
                                               3,0), ncol=2, byrow=TRUE))



treeComp <- function(x,y){
  ifelse(x == 1 & y == 1,1, # always tree cover
         ifelse(x == 1 & y == 0, 2, # loss tree cover
                ifelse(x == 0 & y == 1, 3, # gain
                       ifelse(x == 0 & y == 0,4,0)))) # always other
  
}


# need to find terra replacement for overlay
treeChange <- lapp(c(x=trees57R, y=trees17R),treeComp)  



Tchange <- freq(treeChange)

TchangeDF <- data.frame(Tchange)

TchangeDF$area.m2 <- TchangeDF$count * res(treeChange)[1]*res(treeChange)[2]
TchangeDF$area.km2 <- TchangeDF$area.m2*1e-6


#### Census tract tree change ----
temp_crop <- crop(tempCp,overlapExt, snap="near")
#day 9 has too large of a missing extent
temp_cropc <- temp_crop[[-9]]
#calculate the mean for each day
temp_day_stat <- global(temp_cropc, fun="mean", na.rm=TRUE)
# Average temperature anom
temp_anom_day <- temp_cropc
for(i in 1:nlyr(temp_cropc)){
  temp_anom_day[[i]] <- temp_cropc[[i]]- temp_day_stat$mean[i]
  
}

temp_anom <- app(temp_anom_day,fun="mean", na.rm=TRUE)
temp_anomrs <- resample(temp_anom, trees17R, method="bilinear")

# crop census tracts to just include the area
income_crop <- terra::crop(income,overlapExt)
plot(income_crop)
income_cropR <- rasterize(income_crop,trees57R, field="GEOID")
# caclulate zonal stats
# to calculate total pixels covered by trees and average temp anom
zonesT57 <- terra::zonal(x=trees57R, z=income_cropR, fun="sum",na.rm=TRUE)
zonesT57$tree.area57 <- zonesT57$lc_1957*trees57R@ptr$res[1]*trees57R@ptr$res[2]
zonesT17 <- terra::zonal(x=trees17R, z=income_cropR, fun="sum",na.rm=TRUE)
zonesT17$tree.area17 <- zonesT17$lc_2017*trees17R@ptr$res[1]*trees17R@ptr$res[2]
zonesTemp <- terra::zonal(x=temp_anomrs, z=income_cropR, fun="mean",na.rm=TRUE)

# join all into table
zonesT1 <- inner_join(zonesT57, zonesT17, by="GEOID")
zonesT2 <- inner_join(zonesT1, zonesTemp, by="GEOID")
rental.sfTable <- st_drop_geometry(rental.sf)
zonesT3 <- inner_join(zonesT2, rental.sfTable, by="GEOID")
censusAllt <- inner_join(income.sf, zonesT3, by="GEOID")
censusAll <- st_crop(censusAllt,  xmin=356825,xmax=360870, ymin=342870,ymax=346415)
plot(censusAll["tree.area57"])

censusAll$area <- st_area(censusAll)
attributes(censusAll$area) <- NULL
censusAll$percTree57 <- (censusAll$tree.area57/censusAll$area)*100
attributes(censusAll$percTree57) <- NULL

censusAll$percTree17 <- (censusAll$tree.area17/censusAll$area)*100
attributes(censusAll$percTree17) <- NULL

# remove tracts that don't have a lot of area in the study extent
censusAll <- censusAll %>%
  filter(area > 200000)

# percent of census tract tree change 
censusAll$tree.change <- censusAll$percTree17 - censusAll$percTree57
plot(censusAll["mean"])



holc <- st_read("E:/Google Drive/research/projects/utica/holc/NYUtica1936/cartodb-query.shp")
holcp <- st_transform(holc, crs(trees57R))
holcv <- vect("E:/Google Drive/research/projects/utica/holc/NYUtica1936/cartodb-query.shp")
holcv2 <- project(holcv, crs(trees57R))
holcR <- rasterize(holcv2, trees57R, field="holc_id")
holcZ57 <- terra::zonal(x=trees57R, z=holcR, fun="sum",na.rm=TRUE)
holcZ57$tree.area57 <- holcZ57$lc_1957*trees57R@ptr$res[1]*trees57R@ptr$res[2]
holcZ17 <- terra::zonal(x=trees17R, z=holcR, fun="sum",na.rm=TRUE)
holcZ17$tree.area17 <- holcZ17$lc_2017*trees17R@ptr$res[1]*trees17R@ptr$res[2]
holcZones <- inner_join(holcZ17, holcZ57, by="holc_id")
ext(trees57R)[1]
holcC <- st_crop(holcp,xmin=356825,xmax=360870, ymin=342870,ymax=346415)
holcTree <- inner_join(holcC, holcZones, by="holc_id")
holcTree$area <- st_area(holcTree)
holcTree$perc17 <- (holcTree$tree.area17/holcTree$area)*100
holcTree$perc57 <- (holcTree$tree.area57/holcTree$area)*100
holcTree$percChange <- holcTree$perc17-holcTree$perc57

plot(holcR)
plot(holcp["holc_grade"])

origArea <- data.frame(holc_id = holcp$holc_id,
                       orig_area = st_area(holcp))

holcTree <- inner_join(holcTree, origArea, by="holc_id")
holcTree$percOrig <- (holcTree$area/holcTree$orig_area)*100
attributes(holcTree$percOrig) <- NULL
holcTreeCov <- holcTree %>%
  filter(percOrig > 75)

plot(trees57R)
plot(holcp["holc_grade"], add=TRUE)
#original area
holc


plot(as.numeric(as.factor(holcTreeCov$holc_grade))+rnorm(nrow(holcTreeCov),0,.05),holcTreeCov$perc57 )
plot(as.numeric(as.factor(holcTreeCov$holc_grade))+rnorm(nrow(holcTreeCov),0,.05),holcTreeCov$perc17 )

plot(trees17R)
plot(holcTreeCov["holc_grade"], add=TRUE)
plot(holcTreeCov["holc_grade"], add=TRUE)
plot(holcTreeCov["perc57"])
plot(holcTreeCov["perc17"])
library(mapview)
mapview(holcTreeCov)


# run regressions

temp.change.mod <- lm(censusAll$tree.change ~ 
                        censusAll$mean)
qqnorm(temp.change.mod$residuals)
qqline(temp.change.mod$residuals)
shapiro.test(temp.change.mod$residuals)
plot(censusAll$mean, temp.change.mod$residuals)
summary(temp.change.mod)
bptest(temp.change.mod)
tree.change.mod <- lm(censusAll$tree.change ~ 
                        censusAll$percTree17)
qqnorm(tree.change.mod$residuals)
qqline(tree.change.mod$residuals)
shapiro.test(tree.change.mod$residuals)
plot(censusAll$mean, tree.change.mod$residuals)
bptest(tree.change.mod)
summary(tree.change.mod)

income.change.mod <- lm(censusAll$tree.change ~ 
                          censusAll$med_income)
qqnorm(income.change.mod$residuals)
qqline(income.change.mod$residuals)
shapiro.test(income.change.mod$residuals)
plot(censusAll$med_income, tree.change.mod$residuals)
summary(income.change.mod)

renter.change.mod <- lm(censusAll$tree.change ~ 
                          censusAll$RentP)
qqnorm(renter.change.mod$residuals)
qqline(renter.change.mod$residuals)
shapiro.test(renter.change.mod$residuals)
plot(censusAll$RentP, tree.change.mod$residuals)
summary(renter.change.mod)


temp.tree.mod <- lm(censusAll$percTree17 ~ 
                      censusAll$mean)
qqnorm(temp.tree.mod$residuals)
qqline(temp.tree.mod$residuals)
shapiro.test(temp.tree.mod$residuals)
plot(censusAll$mean, temp.tree.mod$residuals)
abline(h=0)
summary(temp.tree.mod)
bptest(temp.tree.mod)

income.tree.mod <- lm(censusAll$percTree17 ~ 
                        censusAll$med_income)


shapiro.test(na.omit(income.tree.mod$residuals))
plot(censusAll$med_income,income.tree.mod$residuals)
summary(income.tree.mod)

renter.tree.mod <- lm(censusAll$percTree17 ~ 
                        censusAll$RentP)

shapiro.test(na.omit(renter.tree.mod$residuals))
plot(na.omit(censusAll$RentP),renter.tree.mod$residuals)
summary(renter.tree.mod)


##### Table 1. Accuracy metrics ----

# set up accuracy tables
overallacc <- data.frame(year=c(1957,1987,2017),
                         accuracy=c(conf_57$overall[1],conf_87$overall[1],conf_17$overall[1]))

confusion_all <- list(conf_57$table,conf_87$table,conf_17$table)

accuracy_table <- data.frame(year = rep(c(1957,1987,2017),each=4),
                             type = rep(c("other","tree","building","pavement"),times=3),
                             users = round(c(conf_57$table[1,1]/sum(conf_57$table[1,]),
                                             conf_57$table[2,2]/sum(conf_57$table[2,]),
                                             conf_57$table[3,3]/sum(conf_57$table[3,]),
                                             conf_57$table[4,4]/sum(conf_57$table[4,]),
                                             conf_87$table[1,1]/sum(conf_87$table[1,]),
                                             conf_87$table[2,2]/sum(conf_87$table[2,]),
                                             conf_87$table[3,3]/sum(conf_87$table[3,]),
                                             conf_87$table[4,4]/sum(conf_87$table[4,]),
                                             conf_17$table[1,1]/sum(conf_17$table[1,]),
                                             conf_17$table[2,2]/sum(conf_17$table[2,]),
                                             conf_17$table[3,3]/sum(conf_17$table[3,]),
                                             conf_17$table[4,4]/sum(conf_17$table[4,]))*100,1),
                             producers = round(c(conf_57$table[1,1]/sum(conf_57$table[,1]),
                                                 conf_57$table[2,2]/sum(conf_57$table[,2]),
                                                 conf_57$table[3,3]/sum(conf_57$table[,3]),
                                                 conf_57$table[4,4]/sum(conf_57$table[,4]),
                                                 conf_87$table[1,1]/sum(conf_87$table[,1]),
                                                 conf_87$table[2,2]/sum(conf_87$table[,2]),
                                                 conf_87$table[3,3]/sum(conf_87$table[,3]),
                                                 conf_87$table[4,4]/sum(conf_87$table[,4]),
                                                 conf_17$table[1,1]/sum(conf_17$table[,1]),
                                                 conf_17$table[2,2]/sum(conf_17$table[,2]),
                                                 conf_17$table[3,3]/sum(conf_17$table[,3]),
                                                 conf_17$table[4,4]/sum(conf_17$table[,4]))*100,1)) 

accuracy_table$omission_err <- 100 - accuracy_table$producers
accuracy_table$commission_err <- 100 - accuracy_table$users



##### save validation tables ----

write.table(round(overallacc,2), paste0(dirSave, "/overall_accuracy.csv"), sep=",", row.names=FALSE)

write.table(confusion_all[[1]], paste0(dirSave, "/confusion_1957.csv"), sep=",", row.names=FALSE)
write.table(confusion_all[[2]], paste0(dirSave, "/confusion_1987.csv"), sep=",", row.names=FALSE)
write.table(confusion_all[[3]], paste0(dirSave, "/confusion_2017.csv"), sep=",", row.names=FALSE)

write.table(accuracy_table, paste0(dirSave, "/accuracy_table.csv"), sep=",", row.names=FALSE)



##### Figure 1. Landcover comparison ----

# set up mapping variables

#0=other, 1=tree,2=build,3=pavement
colsClass <- c("#FFFFFF","#008C17","#9287A1","#3B3B3A")
#coordinates for area labels
areayoff <- c(-0.5,0.5,0.5,0.5)


# plot dim
wd <- 2.5
hd1 <- 2.5
hd2 <- 2
# arrow line width for scale bar
awd <- 1
# text size for scale bar
sce <- 1.2
#axis size for area plot
cap <- 1
# axis label size for area plot
lax <- 1
#border for bars
borderi <- c("black",NA,NA,NA)
#size for area text label
tcx <- 1.2
#panel label line
llc <- -1
#panel label size
pcx <- 1

png(paste0(dirSave, "/fig_1_cover_panel.png"), width=8.5, height=10, units="in", res=300)
layout(matrix(seq(1,9),ncol=3), width=lcm(rep(wd*2.54,3)),height=lcm(c(hd1,hd1,hd2)*2.54))

### 1957 ###
# 1957 image
par(mai=c(0.01,0.01,0.01,0.01))
plot(img57_crop, col=grey(1:100/100),axes=FALSE, mar=NA, legend=FALSE,
     maxcell=ncell(img57_crop))
mtext("a", side=3, at=360500,  line=llc, cex=pcx)

# 1957 land cover
par(mai=c(0.01,0.01,0.01,0.01))

plot(lc1957_crop, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsClass,
     legend=FALSE,  axes=FALSE, mar=NA,
     maxcell=ncell(lc1957_crop))

arrows(357000,342850,358000,342850, code=0, lwd=awd)
arrows(357000,342800,357000,342850, code=0, lwd=awd)
arrows(357500,342800,357500,342850, code=0, lwd=awd)
arrows(358000,342800,358000,342850, code=0, lwd=awd)
text(357000,342700,"0", cex=sce)
text(357500,342700,"0.5", cex=sce)
text(358000,342700,"1 km", cex=sce)
mtext("d", side=3, at=360500,  line=llc, cex=pcx)
# land cover area total
par(mai=c(0.01,0.01,0.01,0.01))

plot(c(0,1),c(0,1), xlim=c(0.5,4.5),ylim=c(0,8),
     xlab= " ", ylab = " ", xaxs="i", yaxs="i",axes=FALSE,
     type="n")
for(i in 1:4){
  polygon(c(i-0.25,i-0.25,i+0.25,i+0.25),
          c(0,area57DF$area.km2[i],area57DF$area.km2[i],0),
          col=colsClass[i], border=borderi[i])
  
}
text(seq(1,4), area57DF$area.km2 +areayoff , paste0(round(area57DF$area.km2,1)), cex=tcx)


axis(1, seq(0,5),labels=c("","Other","Tree","Building","Pavement",""), cex.axis=cap)
axis(2, seq(0,8, by=2), las=2, cex.axis= cap)
mtext("Land cover type", side=1, line=2.5, cex=lax )
mtext(expression(paste("Area (km"^"2",")")), side=2, line=1.5, cex=lax )
mtext("g", side=3, at=4.2,  line=llc, cex=pcx)
### 1987 ###
# 1987 image
par(mai=c(0.01,0.01,0.01,0.01))
plot(img87_crop, col=grey(1:100/100),axes=FALSE, mar=NA, legend=FALSE,
     maxcell=ncell(img87_crop))
mtext("b", side=3, at=360500,  line=llc, cex=pcx)
# 1987 land cover
par(mai=c(0.01,0.01,0.01,0.01))

plot(lc1987_crop, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsClass,
     legend=FALSE,  axes=FALSE, mar=NA,
     maxcell=ncell(lc1987_crop))
arrows(357000,342850,358000,342850, code=0, lwd=awd)
arrows(357000,342800,357000,342850, code=0, lwd=awd)
arrows(357500,342800,357500,342850, code=0, lwd=awd)
arrows(358000,342800,358000,342850, code=0, lwd=awd)
text(357000,342700,"0", cex=sce)
text(357500,342700,"0.5", cex=sce)
text(358000,342700,"1 km", cex=sce)
mtext("e", side=3, at=360500,  line=llc, cex=pcx)
par(mai=c(0.01,0.01,0.01,0.01))

plot(c(0,1),c(0,1), xlim=c(0.5,4.5),ylim=c(0,8),
     xlab= " ", ylab = " ", xaxs="i", yaxs="i",axes=FALSE,
     type="n")
for(i in 1:4){
  polygon(c(i-0.25,i-0.25,i+0.25,i+0.25),
          c(0,area87DF$area.km2[i],area87DF$area.km2[i],0),
          col=colsClass[i], border=borderi[i])
  
}
text(seq(1,4), area87DF$area.km2 +areayoff, 
     paste0(round(area87DF$area.km2,1),astrix), cex=tcx)

axis(1, seq(0,5),labels=c("","Other","Tree","Building","Pavement",""), cex.axis=cap)
mtext("Land cover type", side=1, line=2.5, cex=lax )
mtext("h", side=3, at=4.2,  line=llc, cex=pcx)


### 2017 ###
# 2017 image
par(mai=c(0.01,0.01,0.01,0.01))
plotRGB(img17_crop,axes=FALSE, mar=NA, 
     maxcell=ncell(img17_crop))
mtext("c", side=3, at=360500,  line=llc, cex=pcx)
# 2017 landcover
par(mai=c(0.01,0.01,0.01,0.01))


plot(lc2017_crop, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsClass,
    legend=FALSE, axes=FALSE, mar=NA,
     maxcell=ncell(lc2017_crop))
arrows(357000,342850,358000,342850, code=0, lwd=awd)
arrows(357000,342800,357000,342850, code=0, lwd=awd)
arrows(357500,342800,357500,342850, code=0, lwd=awd)
arrows(358000,342800,358000,342850, code=0, lwd=awd)
text(357000,342700,"0", cex=sce)
text(357500,342700,"0.5", cex=sce)
text(358000,342700,"1 km", cex=sce)
mtext("f", side=3, at=360500,  line=llc, cex=pcx)
par(mai=c(0.01,0.01,0.01,0.01))

plot(c(0,1),c(0,1), xlim=c(0.5,4.5),ylim=c(0,8),
     xlab= " ", ylab = " ", xaxs="i", yaxs="i",axes=FALSE,
     type="n")
for(i in 1:4){
  polygon(c(i-0.25,i-0.25,i+0.25,i+0.25),
          c(0,area17DF$area.km2[i],area17DF$area.km2[i],0),
          col=colsClass[i], border=borderi[i])
  
}
text(seq(1,4), area17DF$area.km2 +areayoff, paste0(round(area17DF$area.km2,1)), cex=tcx)
axis(1, seq(0,5),labels=c("","Other","Tree","Building","Pavement",""), cex.axis=cap)

mtext("Land cover type", side=1, line=2.5, cex=lax )
mtext("i", side=3, at=4.2,  line=llc, cex=pcx)

dev.off()



#### Figure 2: Tree change map and plot ----

# plot parameters
# arrow line width for scale bar
awd <- 1
# text size for scale bar
sce <- 1
cols <- c("#176611","#9D769A","#9BC101", "#FFFFFF")
wd <- 6
hd <- 6

png(paste0(dirSave,"/fig_2_tree_change_map.png"), width=12, height=6,
    units="in", res=300 )

layout(matrix(seq(1,2),ncol=2), width=lcm(rep(wd*2.54,2)),height=lcm(c(hd)*2.54))
par(mai=c(0,0,0,0))
plot(treeChange,breaks=c(0,1.5,2.5,3.5,4.5),
     col=cols,
     legend=FALSE,  axes=FALSE,
     maxcell=ncell(treeChange))

legend(357000,346650,
       c("Stable","Loss", "Gain","Other"),
       fill=cols,border=c(NA,NA,NA,"black"),
       bty="n",horiz=TRUE, cex=1)


arrows(357000,342850,358000,342850, code=0, lwd=awd)
arrows(357000,342800,357000,342850, code=0, lwd=awd)
arrows(357500,342800,357500,342850, code=0, lwd=awd)
arrows(358000,342800,358000,342850, code=0, lwd=awd)
text(357000,342700,"0", cex=sce)
text(357500,342700,"0.5", cex=sce)
text(358000,342700,"1 km", cex=sce)
mtext("a", side=3, at=360500,  line=-1, cex=1.1)
par(mai=c(1,1,1,1))
plot(c(0,1),c(0,1), xlim=c(0.5,4.5),ylim=c(0,11),
     xlab= " ", ylab = " ", xaxs="i", yaxs="i",axes=FALSE,
     type="n")
for(i in 1:4){
  polygon(c(i-0.25,i-0.25,i+0.25,i+0.25),
          c(0,TchangeDF$area.km2[i],TchangeDF$area.km2[i],0),
          col=cols[i])
  
}

axis(1, seq(0,5),labels=c("","stable","loss","gain","other",""), cex.axis=1)
axis(2, seq(0,10, by=2), las=2, cex.axis=1)
mtext("Tree cover change status", side=1, line=3, cex=1 )
mtext(expression(paste("Area (km"^"2",")")), side=2, line=2, cex=1 )

text(seq(1,4), TchangeDF$area.km2 +.5 , paste0(round(TchangeDF$area.km2,1)), cex=1)

mtext("b", side=3, at=4.2,  line=1, cex=1.1)
dev.off()





#### Figure 3: Tree cover patterns in census tracts ----


# plot dim
wd <- 3.5
hd1 <- 3.5

# arrow line width for scale bar
awd <- 1

#axis size for area plot
cap <- 1.25
# axis label size for area plot
lax <- 1.5

#panel label line
llc <- -1
#axis title label size
pcx <- 1.25
# point size
ptc <- 1.5
# panel text label size
tcx <- 1.5
# line width
lwx <- 2

png(paste0(dirSave, "/fig_3_census_panel.png"), width=10, height=9, units="in", res=300)
layout(matrix(seq(1,4),ncol=2, byrow=TRUE), width=lcm(rep(wd*2.54,2)),height=lcm(rep(hd1*2.54,2)))

par(mai=c(0.5,0,0,0))
plot(censusAll$percTree17, 
     censusAll$tree.change, 
     ylim=c(-30,22), xlim=c(0,50),
     xlab= " ", ylab = " ", xaxs="i", yaxs="i",axes=FALSE,
     type="n")
points(censusAll$percTree17, censusAll$tree.change, pch=19,cex=ptc)
abline(tree.change.mod, lwd=lwx)
axis(2, seq(-30,20, by=5), cex.axis=cap, las=2)
axis(1, seq(0,40, by=10), cex.axis=cap)

mtext("Percentage of tract ", side=1, line=3, cex=pcx)
mtext("with tree cover in 2017 (%)", side=1, line=5, cex=pcx)

mtext("Difference in percent tree cover", side=2, line=7, cex=pcx)
mtext("in tract from 2017-1957", side=2, line=5, cex=pcx)
mtext("(% of tract area)", side=2, line=3, cex=pcx)
text(47,19, "a", cex=tcx)

box(which="plot")
par(mai=c(0.5,0,0,0))

plot(censusAll$mean, 
     censusAll$tree.change, 
     ylim=c(-30,22), xlim=c(-7,6),
     xlab= " ", ylab = " ", xaxs="i", yaxs="i",axes=FALSE,
     type="n")
points(censusAll$mean, censusAll$tree.change, pch=19,cex=ptc)
axis(1, seq(-6,4, by=2), cex.axis=cap)
abline(temp.change.mod,lwd = lwx )
mtext(expression(paste("Surface temperature anomaly (",~degree,"C)")), side=1, line=3, cex=pcx)
box(which="plot")
text(5.5,19, "b", cex=tcx)


par(mai=c(0,0,0.5,0))
plot(censusAll$med_income, 
     censusAll$tree.change,
     ylim=c(-30,22), xlim=c(10000,70000),
     xlab= " ", ylab = " ", xaxs="i", yaxs="i",axes=FALSE,
     type="n")

points(censusAll$med_income, censusAll$tree.change, pch=19,cex=ptc)
axis(1, seq(10000,70000, by=10000), c("","20,000","", "40,000","","60,000",""), cex.axis=cap)
box(which="plot")
mtext("Median household income ($) ", side=1, line=3, cex=pcx)
mtext("Difference in percent tree cover", side=2, line=7, cex=pcx)
mtext("in tract from 2017-1957", side=2, line=5, cex=pcx)
mtext("(% of tract area)", side=2, line=3, cex=pcx)
text(67000,19, "c", cex=tcx)



par(mai=c(0,0,0.5,0))
plot(censusAll$RentP, 
     censusAll$tree.change,
     ylim=c(-30,22), xlim=c(0,90),
     xlab= " ", ylab = " ", xaxs="i", yaxs="i",axes=FALSE,
     type="n")

points(censusAll$RentP, censusAll$tree.change, pch=19,cex=ptc)
axis(1, seq(0,100, by=20), cex.axis=cap)
box(which="plot")
mtext("Renter occupied households (%) ", side=1, line=3, cex=pcx)
text(85,19, "d", cex=tcx)

dev.off()



#### Figure 4: Current tree cover patterns in census tracts ----


# plot dim
wd <- 4
hd1 <- 4

# arrow line width for scale bar
awd <- 1

#axis size for area plot
cap <- 2
# axis label size for area plot
lax <- 1.5

#panel label line
llc <- -1
#panel label size
pcx <- 1.5
#line width
rlw <- 2
# panel letter label size
tcx <- 2
#line size
lwx <- 2
# point size
ptcx <- 2

png(paste0(dirSave, "/fig_4_current_census_panel.png"), width=14, height=5.5, units="in", res=300)
layout(matrix(seq(1,3),ncol=3), width=lcm(rep(wd*2.54,3)),height=lcm(hd1*2.54))

par(mai=c(0,0,0,0))
plot(censusAll$mean, 
     censusAll$percTree17, 
     xlim=c(-7,6), ylim=c(0,50),
     xlab= " ", ylab = " ", xaxs="i", yaxs="i",axes=FALSE,
     type="n")
points(censusAll$mean, censusAll$percTree17, pch=19, cex=ptcx)
abline(temp.tree.mod,lwd = lwx )
axis(1, seq(-6,4, by=2), cex.axis=cap)
axis(2, seq(0,40, by=10), cex.axis=cap)
abline(tree.change.mod,lwd = rlw )
mtext("Percentage of tract ", side=2, line=5, cex=pcx)
mtext("with tree cover in 2017 (%)", side=2, line=3, cex=pcx)
text(5.5,47, "a", cex=tcx)
mtext(expression(paste("Surface temperature anomaly (",~degree,"C)")), side=1, line=3, cex=pcx)


box(which="plot")


par(mai=c(0,0,0,0))
plot(censusAll$med_income, 
     censusAll$percTree17,
     ylim=c(0,50), xlim=c(10000,70000),
     xlab= " ", ylab = " ", xaxs="i", yaxs="i",axes=FALSE,
     type="n")

points(censusAll$med_income, censusAll$percTree17, pch=19, cex=ptcx)

axis(1, seq(10000,70000, by=10000), c("","20,000","", "40,000","","60,000",""), cex.axis=cap)
box(which="plot")
mtext("Median household income ($) ", side=1, line=3, cex=pcx)
text(67000,47, "b", cex=tcx)

par(mai=c(0,0,0,0))
plot(censusAll$RentP, 
     censusAll$percTree17,
     ylim=c(0,50), xlim=c(-1,90),
     xlab= " ", ylab = " ", xaxs="i", yaxs="i",axes=FALSE,
     type="n")

points(censusAll$RentP, censusAll$percTree17, pch=19, cex=ptcx)
text(85,47, "c", cex=tcx)
axis(1, seq(0,100, by=20), cex.axis=cap)
box(which="plot")
mtext("Renter occupied households (%) ", side=1, line=3, cex=pcx)
dev.off()

#### Figure 5: Census maps ----
library(tmap)
par(mfrow=c(2,1))

tm_shape(censusAll)+
  tm_polygons("percTree17", 
              palette=rev(hcl.colors(5, palette="Greens")), 
              n=5,
              style="jenks")



censusAll$labelName <- gsub(", Oneida County, New York","",
                gsub("Census Tract ","",
                    censusAll$NAME.x))
censusAllCentroid <- st_centroid(censusAll)
censusAllcenter <- st_coordinates(censusAllCentroid)
labelyoffset <- ifelse(censusAll$labelName == "201.02",
                       125,
                ifelse(censusAll$labelName == "214.01",
                       -200,0))

labelxoffset <- ifelse(censusAll$labelName == "201.02",
                       -500,
                       ifelse(censusAll$labelName == "214.01",
                              -20,0))


colsTree <- rev(hcl.colors(5, palette="Greens"))
colsChange <- hcl.colors(5, palette="PRGn")
#get breaks
breaksT17 <- round(getJenksBreaks(censusAll$percTree17,6))
# make sure first break is rounded down
breaksT57 <- c(floor(getJenksBreaks(censusAll$percTree57,6)[1]),
               round(getJenksBreaks(censusAll$percTree57,6)[2:6]))

breaksChange <- seq(-30,20,by=10)

plot(censusAll["tree.change"],
     breaks=breaksChange, pal=colsChange)
# map panel
hdl <- 1
wd <- 6
hd1 <- 6* (nrow(img17_crop)/ncol(img17_crop))



png(paste0(dirSave, "/fig_5_current_census_maps.png"), width=14, height=14, units="in", res=300)
layout(matrix(seq(1,8),ncol=2, byrow=TRUE), width=lcm(rep(wd*2.54,4)),height=lcm(c(hdl,rep(hd1*2.54,2),hdl)))

par(mai=c(0,0,0,0))
plot(c(0,10),c(0,1), type="n", axes=FALSE, xaxs="i",yaxs="i")
#legend for 1957
par(mai=c(0,0,0,0))
plot(c(min(breaksT57)-6,max(breaksT57))+5,c(0,1), type="n", axes=FALSE, xaxs="i",yaxs="i")

for(i in 1:(length(breaksT17)-1)){
  polygon(c(breaksT57[i],breaksT57[i],breaksT57[i+1],breaksT57[i+1]),
          c(0.25,0.75,0.75,0.25),
          col=colsTree[i], border=NA)
}
arrows(breaksT57,0.75,breaksT57,1, code=0, lwd=2)
mtext(paste0(breaksT57),at=breaksT57,side=3,line=0)

# map of imagery and census tract geometry
par(mai=c(0,0,0,0))
plotRGB(img17_crop,axes=FALSE, mar=NA)
plot(censusAll$geometry, key.pos=NULL, main=NA,
      border="white", lwd=2, add=TRUE, reset=FALSE)
text(censusAllcenter[,1] + labelxoffset,
     censusAllcenter[,2]+ labelyoffset,
     censusAll$labelName, col="white",  font=2, cex=1.75)



par(mai=c(0,0,0,0))
plot(img57_crop,axes=FALSE, mar=NA, col="white", legend=FALSE)
plot(censusAll["percTree57"], key.pos=NULL, main=NA, reset=FALSE,
     add=TRUE,
     breaks=breaksT57, pal=colsTree)

par(mai=c(0,0,0,0))
plot(img57_crop,axes=FALSE, mar=NA, col="white", legend=FALSE)
plot(censusAll["percTree17"], key.pos=NULL, main=NA, reset=FALSE,
     add=TRUE,
     breaks=breaksT17, pal=colsTree)





par(mai=c(0,0,0,0))
plot(img57_crop,axes=FALSE, mar=NA, col="white", legend=FALSE)
plot(censusAll["tree.change"], key.pos=NULL, main=NA, reset=FALSE,
     add=TRUE,breaks=breaksChange, pal=colsChange)

#legend for 2017
par(mai=c(0,0,0,0))
plot(c(min(breaksT17)-6,max(breaksT17))+5,c(0,1), type="n", axes=FALSE, xaxs="i",yaxs="i")

for(i in 1:(length(breaksT17)-1)){
  polygon(c(breaksT17[i],breaksT17[i],breaksT17[i+1],breaksT17[i+1]),
          c(0.25,0.75,0.75,0.25),
          col=colsTree[i], border=NA)
}
arrows(breaksT17,0,breaksT17,0.25, code=0, lwd=2)
mtext(paste0(breaksT17),at=breaksT17,side=1,line=0)

par(mai=c(0,0,0,0))
plot(c(min(breaksChange)-6,max(breaksChange))+5,c(0,1), type="n", axes=FALSE, xaxs="i",yaxs="i")

for(i in 1:(length(breaksChange)-1)){
  polygon(c(breaksChange[i],breaksChange[i],breaksChange[i+1],breaksChange[i+1]),
          c(0.25,0.75,0.75,0.25),
          col=colsChange[i], border=NA)
}
arrows(breaksChange,0,breaksChange,0.25, code=0, lwd=2)
mtext(paste0(breaksChange),at=breaksChange,side=1,line=0)

dev.off()
