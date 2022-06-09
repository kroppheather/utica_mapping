library(sf)
library(raster)
library(mapview)
library(mapedit)


dirM_orig <- c("/Volumes/GoogleDrive/My Drive/research/projects/utica/mask")
dirM_small <- "/Volumes/GoogleDrive/My Drive/research/projects/utica/50s_small_mask"

treeOrig <- list()

for(i in 1:60){
  treeOrig[[i]] <- raster(paste0(dirM_orig, "/trees/tree_mask_",i,".tif"))
}

treeCrop <- list()

for( in in 1:60){
  treeCrop[[i]] <- crop(treeOrig[[i]],
                        extent(75,75+127,75,75+127))
  
}
