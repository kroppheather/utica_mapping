library(raster)
library(sf)
library(dplyr)
library(tmap)

#######################################
##### Read in data -----


dirI <- "E:/Google Drive/research/projects/utica/maps_save"
saveDir <- "E:/Google Drive/research/projects/utica/AGU_figures"
# read in land cover
lc57 <- raster(paste0(dirI,"/lc_1957.tif"))
lc17p <- raster(paste0(dirI,"/utica17_strat_fix.tif"))

lc17 <- projectRaster(lc17p, crs="+init=epsg:32116", method="ngb")
writeRaster(lc17, "E:/Google Drive/research/projects/utica/maps_save/lc_2017_fix.tif",
            format="GTiff")
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
##### Land cover  -----
#0=other, 1=tree,2=build,3=pavement
colsClass <- c("#545453","#187E4C","#E77002","grey90")

## 1957 ##
png(paste0(saveDir,"/cover_1957.png"), width=10, height=7,
    units="in", res=300 )
par(mai=c(0.5,0.5,0.5,0.5))
plot(lc57, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsClass,
  legend=FALSE, ylim=c(342400,347000), axes=FALSE,  box=FALSE,
  maxpixels=(lc57@nrows * lc57@ncols))

legend(355000,347000,
       c("Other","Tree","Building","Pavement"),
       fill=colsClass,
       bty="n",horiz=TRUE, cex=1.5)

arrows(355000,342500,356000,342500, code=0, lwd=2)
arrows(355000,342500,355000,342600, code=0, lwd=2)
arrows(355500,342500,355500,342600, code=0, lwd=2)
arrows(356000,342500,356000,342600, code=0, lwd=2)
text(356000,342700,"1 km", cex=1)
text(355500,342700,"0.5 km", cex=1)
text(355000,342700,"0 km", cex=1)

dev.off()


l57count <- freq(lc57)

area57DF <- data.frame(l57count)

area57DF$area.m2 <- area57DF$count * res(lc57)[1]*res(lc57)[2]
area57DF$area.km2 <- area57DF$area.m2*1e-6


png(paste0(saveDir,"/land area 1957.png"), width=5, height=5,
    units="in", res=300 )

plot(c(0,1),c(0,1), xlim=c(0.5,4.5),ylim=c(0,16),
     xlab= " ", ylab = " ", xaxs="i", yaxs="i",axes=FALSE,
     type="n")
for(i in 1:4){
  polygon(c(i-0.25,i-0.25,i+0.25,i+0.25),
          c(0,area57DF$area.km2[i],area57DF$area.km2[i],0),
          col=colsClass[i])
  
}

axis(1, seq(0,5),labels=c("","Other","Tree","Building","Pavement",""), cex.axis=1)
axis(2, seq(0,16, by=4), las=2, cex.axis=1.25)
mtext("Tree cover change status", side=1, line=3, cex=1.5 )
mtext(expression(paste("Area (km)"^"2")), side=2, line=2, cex=1.5 )
dev.off()


lc17Crop

png(paste0(saveDir,"/cover_2017.png"), width=10, height=7,
    units="in", res=300 )
par(mai=c(0.5,0.5,0.5,0.5))
plot(lc17Crop, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsClass,
     legend=FALSE, ylim=c(342400,347000), axes=FALSE,  box=FALSE,
     maxpixels=(lc57@nrows * lc57@ncols)/3)

legend(355000,347000,
       c("Other","Tree","Building","Pavement"),
       fill=colsClass,
       bty="n",horiz=TRUE, cex=1.5)

arrows(355000,342500,356000,342500, code=0, lwd=2)
arrows(355000,342500,355000,342600, code=0, lwd=2)
arrows(355500,342500,355500,342600, code=0, lwd=2)
arrows(356000,342500,356000,342600, code=0, lwd=2)
text(356000,342700,"1 km", cex=1)
text(355500,342700,"0.5 km", cex=1)
text(355000,342700,"0 km", cex=1)

dev.off()


l17count <- freq(lc17Crop)

area17DF <- data.frame(l17count)

area17DF$area.m2 <- area17DF$count * res(lc17Crop)[1]*res(lc17Crop)[2]
area17DF$area.km2 <- area17DF$area.m2*1e-6


png(paste0(saveDir,"/land area 2017.png"), width=5, height=5,
    units="in", res=300 )

plot(c(0,1),c(0,1), xlim=c(0.5,4.5),ylim=c(0,16),
     xlab= " ", ylab = " ", xaxs="i", yaxs="i",axes=FALSE,
     type="n")
for(i in 1:4){
  polygon(c(i-0.25,i-0.25,i+0.25,i+0.25),
          c(0,area17DF$area.km2[i],area17DF$area.km2[i],0),
          col=colsClass[i])
  
}

axis(1, seq(0,5),labels=c("","Other","Tree","Building","Pavement",""), cex.axis=1)
axis(2, seq(0,16, by=4), las=2, cex.axis=1.25)
mtext("Tree cover change status", side=1, line=3, cex=1.5 )
mtext(expression(paste("Area (km)"^"2")), side=2, line=2, cex=1.5 )
dev.off()

#######################################
##### Land cover change -----


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



treeChange <- overlay(trees57R, trees17R, fun=treeComp)
plot(treeChange)

treeChange@nrows/treeChange@ncols
6*0.6
png(paste0(saveDir,"/tree_change_map.png"), width=10, height=7,
    units="in", res=300 )
par(mai=c(0.5,0.5,0.5,0.5))
plot(treeChange,breaks=c(0,1.5,2.5,3.5,4.5),
     col=c("#176611","#9D769A","#9BC101", "grey85"),
      legend=FALSE, ylim=c(342400,347000), axes=FALSE,  box=FALSE,
     maxpixels=(treeChange@nrows * treeChange@ncols)/3)#down sample still

legend(355000,347000,
       c("Tree","Loss", "Gain","Other"),
       fill=c("#176611","#9D769A","#9BC101", "grey85"),
       bty="n",horiz=TRUE, cex=1.5)

arrows(355000,342500,356000,342500, code=0, lwd=2)
arrows(355000,342500,355000,342600, code=0, lwd=2)
arrows(355500,342500,355500,342600, code=0, lwd=2)
arrows(356000,342500,356000,342600, code=0, lwd=2)
text(356000,342700,"1 km", cex=1)
text(355500,342700,"0.5 km", cex=1)
text(355000,342700,"0 km", cex=1)
dev.off()

Tchange <- freq(treeChange)

TchangeDF <- data.frame(Tchange)

TchangeDF$area.m2 <- TchangeDF$count * res(treeChange)[1]*res(treeChange)[2]
TchangeDF$area.km2 <- TchangeDF$area.m2*1e-6
cols <- c("#176611","#9D769A","#9BC101", "grey85")

png(paste0(saveDir,"/tree_change_comp.png"), width=5, height=5,
    units="in", res=300 )

plot(c(0,1),c(0,1), xlim=c(0.5,4.5),ylim=c(0,16),
     xlab= " ", ylab = " ", xaxs="i", yaxs="i",axes=FALSE,
     type="n")
for(i in 1:4){
  polygon(c(i-0.25,i-0.25,i+0.25,i+0.25),
          c(0,TchangeDF$area.km2[i],TchangeDF$area.km2[i],0),
          col=cols[i])
  
}

axis(1, seq(0,5),labels=c("","tree","loss","gain","other",""), cex.axis=1.25)
axis(2, seq(0,16, by=4), las=2, cex.axis=1.25)
mtext("Tree cover change status", side=1, line=3, cex=1.5 )
mtext(expression(paste("Area (km)"^"2")), side=2, line=2, cex=1.5 )
dev.off()



# patterns with census data

# crop census data to 2017
renterCrop <- st_crop(renter,lc17Crop)
plot(renterCrop)

renterCrop$area <- as.numeric(st_area(renterCrop))
renterSub <- renterCrop[renterCrop$area > 200000,]

plot(renterSub["RentP"])
renterSub$Tract <- as.character(renterSub$GEOID)
renterSub$TractID <- seq(1,nrow(renterSub))

renterRast <- rasterize(renterSub,lc17Crop,field="TractID")

treeRenter <- raster::zonal(trees17R,renterRast,fun="sum" )

renterSub$TreePixels <- treeRenter[,2]

renterSub$tree.area.m2 <- renterSub$TreePixels * res(lc17Crop)[1]*res(lc17Crop)[2]
renterSub$tree.area.km2 <- renterSub$tree.area.m2*1e-6
renterSub$area.km2 <- renterSub$area*1e-6

renterSub$treePerc <- (renterSub$tree.area.km2/renterSub$area.km2)*100

plot(renterSub["treePerc"])

plot(renterSub$RentP, renterSub$treePerc, pch=19, ylim=c(0,35))



# 2017 tree cover vs income

incomeCrop <- st_crop(income,lc17Crop)
plot(incomeCrop)

incomeCrop$area <- as.numeric(st_area(incomeCrop))
incomeSub <- incomeCrop[incomeCrop$area > 200000,]

plot(incomeSub["med_income"])
incomeSub$Tract <- as.character(incomeSub$GEOID)
incomeSub$TractID <- seq(1,nrow(incomeSub))


incomeRast <- rasterize(incomeSub,lc17Crop,field="TractID")

treeIncome <- raster::zonal(trees17R,incomeRast,fun="sum" )

incomeSub$TreePixels <- treeIncome[,2]

incomeSub$tree.area.m2 <- incomeSub$TreePixels * res(lc17Crop)[1]*res(lc17Crop)[2]
incomeSub$tree.area.km2 <- incomeSub$tree.area.m2*1e-6
incomeSub$area.km2 <- incomeSub$area*1e-6

incomeSub$treePerc <- (incomeSub$tree.area.km2/incomeSub$area.km2)*100

plot(incomeSub["treePerc"])

plot(incomeSub$med_income, incomeSub$treePerc, pch=19, ylim=c(0,35))



# land cover change by tract
tempCRS <- resample(Temp_Anom, lc17Crop)

tempIncome <- raster::zonal(tempCRS,incomeRast,fun="mean" )
tempIncome

incomeSub$tempC <- tempIncome[,2]

plot(incomeSub$treePerc,incomeSub$tempC)
plot(incomeSub$med_income,incomeSub$tempC)


raceCrop <- st_crop(race,lc17Crop)
plot(raceCrop)

raceCrop$area <- as.numeric(st_area(raceCrop))
raceSub <- raceCrop[raceCrop$area > 200000,]
plot(raceSub)


censusAll <- cbind(incomeSub[,c(1,3,6,7,8,9,10,11,12,13,14)],
                   renterSub[,c(13)],
                   raceSub[,c(3:14)])
censusAll$blackPerc <- (censusAll$black/censusAll$total)*100
censusAll$asianPerc <- (censusAll$Asian/censusAll$total)*100

plot(censusAll$asianPerc, censusAll$treePerc)
plot(censusAll$blackPerc, censusAll$treePerc)




tree57Income <- raster::zonal(trees57R,incomeRast,fun="sum" )

tree57.area.m2 <- tree57Income[,2] * res(trees57R)[1]*res(trees57R)[2]
tree57.area.km2 <- tree57.area.m2*1e-6


tree57Perc <- (tree57.area.km2/censusAll$area.km2)*100

censusAll$tree57Perc <- tree57Perc

censusAll$areaDiff <-  censusAll$treePerc - censusAll$tree57Perc

plot(censusAll$treePerc,censusAll$tempC)
plot(censusAll$med_income,censusAll$areaDiff)
plot(censusAll$areaDiff,censusAll$tempC)

# temperature vs 2017 tree cover

# barplot of percent tree cover in tract by year

q57 <- quantile(censusAll$tree57Perc, probs=c(0,.25,.5,.75,1))

q17 <- quantile(censusAll$treePerc, probs=c(0,.25,.5,.75,1))

set.seed(12)
x1 <- rep(1,nrow(censusAll))+rnorm(nrow(censusAll),0,0.25)
set.seed(14)
x2 <- rep(3,nrow(censusAll))+rnorm(nrow(censusAll),0,0.25)

# 1957
plot(c(0,1),c(0,1), xlim=c(0,5),ylim=c(0,50),
    xlab= " ", ylab = " ", xaxs="i", yaxs="i",axes=FALSE,
    type="n")

polygon(c(0.5,0.5,1.5,1.5),
        c(q57[2],q57[4],q57[4],q57[2]), col=rgb(0.23,0.37,0.8,.7),
        border=rgb(0.23,0.37,0.8))
arrows(0.5,q57[3],1.5,q57[3], lwd=3, col=rgb(0.23,0.37,0.8), code=0)
arrows(1,q57[1],1,q57[5], lwd=3, col=rgb(0.23,0.37,0.8), code=0)

points(x1,censusAll$tree57Perc, pch=19, col=rgb(0.23,0.37,0.8))

# 1957

polygon(c(2.5,2.5,3.5,3.5),
        c(q17[2],q17[4],q17[4],q17[2]), col=rgb(0.23,0.37,0.8,.7),
        border=rgb(0.23,0.37,0.8))
arrows(2.5,q17[3],3.5,q17[3], lwd=3, col=rgb(0.23,0.37,0.8), code=0)
arrows(3,q17[1],3,q17[5], lwd=3, col=rgb(0.23,0.37,0.8), code=0)

points(x2,censusAll$treePerc, pch=19, col=rgb(0.23,0.37,0.8))

axis(2, seq(0,50, by=10), las=2)
axis(1, c(1,3), labels=c("1957","2017"))


arrows(x1,censusAll$tree57Perc,x2,censusAll$treePerc, code=0,
       col=rgb(0.23,0.37,0.8,.3), lty=2)


