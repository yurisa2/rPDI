PATH <- "C:/Bitnami/wampstack-7.0.16-1/apache2/htdocs/rPDI/artigo/sat"
PATH_IMG <- "C:/Users/Administrator/Documents/landsat"

setwd(PATH)


library("imager")
library("raster")

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

file_list <- list.files(PATH_IMG, recursive = TRUE, full.names = TRUE)
for(i in file_list){
  if(substrRight(i, 3) == "tif" || substrRight(i, 3) == "TIF") {
    rasto <- raster(i)
    represa <- load.image(i)
    represa <- extent(252000,275000,-2621000,-2608000)
    rasto_represa <- crop(rasto,represa)
    writeRaster(rasto_represa,paste0("represa/",basename(i)),options=c('TFW=NO'))
  }
}
