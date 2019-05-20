PATH <- "/home/yurisa2/Downloads/itupararanga"
setwd(PATH)


library("imager")
library("raster")

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}



file_list <- list.files(PATH, recursive = TRUE)
dir_list <- list.dirs(PATH)

for(dir in dir_list) {
  dir.create(paste0("represa/",dir))
}

for(i in file_list){
  if(substrRight(i, 3) == "tif" || substrRight(i, 3) == "TIF") {
    rasto <- raster(i)
    represa <- load.image(i)
    represa <- extent(252000,275000,-2621000,-2608000)
    rasto_represa <- crop(rasto,represa)
    writeRaster(rasto_represa,paste0(i,"_represa.tif"),options=c('TFW=YES'))
  }
}
