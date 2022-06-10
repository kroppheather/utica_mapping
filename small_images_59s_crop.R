library(sf)
library(raster)
library(mapview)
library(mapedit)


dirM_orig <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica/mask")
dirM_small <- "/Volumes/GoogleDrive/My Drive/research/projects/utica/50s_small_mask"
dirM_save <- "/Volumes/GoogleDrive/My Drive/research/projects/utica/model_save/1950/data_128/train"

treeOrig <- list()

for(i in 1:60){
  treeOrig[[i]] <- raster(paste0(dirM_orig, "/trees/tree_mask_",i,".tif"))
}



for(i in 1:60){
  writeRaster(crop(treeOrig[[i]],
                        extent(treeOrig[[i]],75,75+127,75,75+127)),
                        paste0(dirM_save,"/trees/tree_mask_",i,".tif"),
                        format="GTiff")
                        
  
}

#trim reprojected small images
treeSmall<- list()

for(i in 1:40){
  treeSmall[[i]] <- raster(paste0(dirM_small, "/trees/tree_mask_",i+80,".tif"))
}