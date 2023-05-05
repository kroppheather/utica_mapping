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

# lc1987 is still in WGS84 project to state plane

lc1987 <- project(lc1987,"+init=epsg:32116", method="near")

# census shape files from the ACS 2020
# average summer land surface temperature from landsat collection 2 level 2

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


##### Figure 1. Landcover comparision ----

##### organize land cover data for maps 
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

# set up mapping variables

#0=other, 1=tree,2=build,3=pavement
colsClass <- c("#545453","#187E4C","#E77002","grey90")
# plot dim
wd <- 2.5
hd1 <- 2.5
hd2 <- 1


png(paste0(dirSave, "/cover_panel.png"), width=7.5, height=5, units="in", res=300)
layout(matrix(seq(1,6),ncol=3), width=lcm(rep(wd*2.54,3)),height=lcm(c(hd1,hd2)*2.54))
# 1957
par(mai=c(0,0,0,0))
plot(lc1957_crop, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsClass,
     legend=FALSE,  axes=FALSE)
     #maxcell=ncell(lc1957_crop))
box(which="plot")

arrows(357000,342500,358000,342500, code=0, lwd=2)
arrows(357000,342500,357000,342600, code=0, lwd=2)
arrows(357500,342500,357500,342600, code=0, lwd=2)
arrows(358000,342500,358000,342600, code=0, lwd=2)
text(357000,342700,"1 km", cex=1)
text(357500,342700,"0.5 km", cex=1)
text(358000,342700,"0 km", cex=1)

par(mai=c(0.25,0.25,0,0.25))

plot(c(0,1),c(0,1), xlim=c(0.5,4.5),ylim=c(0,10),
     xlab= " ", ylab = " ", xaxs="i", yaxs="i",axes=FALSE,
     type="n")
for(i in 1:4){
  polygon(c(i-0.25,i-0.25,i+0.25,i+0.25),
          c(0,area57DF$area.km2[i],area57DF$area.km2[i],0),
          col=colsClass[i])
  
}

axis(1, seq(0,5),labels=c("","Other","Tree","Building","Pavement",""), cex.axis=1)
axis(2, seq(0,16, by=4), las=2, cex.axis=1.25)
mtext("Land cover type", side=1, line=3, cex=1.5 )
mtext(expression(paste("Area (km)"^"2")), side=2, line=2, cex=1.5 )


# 1987
par(mai=c(0,0,0,0))
plot(lc1987_crop, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsClass,
     legend=FALSE,  axes=FALSE)
    # maxcell=ncell(lc1987_crop))

par(mai=c(0,0,0,0))

plot(c(0,1),c(0,1), xlim=c(0.5,4.5),ylim=c(0,10),
     xlab= " ", ylab = " ", xaxs="i", yaxs="i",axes=FALSE,
     type="n")

par(mai=c(0,0,0,0))

plot(lc2017_crop, breaks=c(-0.5,0.5,1.5,2.5,3.5),col=colsClass,
     legend=FALSE, axes=FALSE)
    # maxcell=ncell(lc2017_crop))

par(mai=c(0,0,0,0))

plot(c(0,1),c(0,1), xlim=c(0.5,4.5),ylim=c(0,10),
     xlab= " ", ylab = " ", xaxs="i", yaxs="i",axes=FALSE,
     type="n")

dev.off()


