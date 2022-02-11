library(sf)
library(terra)
library(mapview)
library(mapedit)

files <- list.files("/Volumes/GoogleDrive/My Drive/GIS/utica/1990s_aerial/mosaic", ".tif",
                    full.names = TRUE)

tiles <- list()
for(i in 1:length(files)){
  tiles[[i]] <- rast(files[i])
}
plotRGB(tiles[[1]])



