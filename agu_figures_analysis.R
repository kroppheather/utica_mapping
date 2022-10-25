
# try updating newer analysis to terra

library(raster)


lc1957 <- raster("E:/Google Drive/research/projects/utica/model_save/1950/all_maps/utica50s_128.tif")
lc2017 <-  raster("E:/Google Drive/research/projects/utica/model_save/2017/all_maps/utica17_256.tif")
plot(lc1957)
plot(lc2017)


# pull out just trees
trees57 <- reclassify(lc1957, rcl=matrix(c(0,NA,
                              1,1,
                              2,NA,
                              3,NA), ncol=2, byrow=TRUE))

plot(trees57)


trees17 <- reclassify(lc2017, rcl=matrix(c(0,NA,
                                         1,1,
                                         2,NA,
                                         3,NA), ncol=2, byrow=TRUE))

plot(trees17)


writeRaster(trees57, "E:/Google Drive/research/projects/utica/model_save/1950/all_maps/Utrees57_128.tif",
            format="GTiff")

writeRaster(trees17, "E:/Google Drive/research/projects/utica/model_save/2017/all_maps/Utrees17_256.tif",
            format="GTiff")

