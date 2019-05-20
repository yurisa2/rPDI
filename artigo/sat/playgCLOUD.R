PATH <- "C:/Users/Administrator/Documents/rPDI/artigo/sat"

setwd(PATH)

library("imager")
library("raster")

source(file="functions.R")

rm(file_list)
file_list <- data.frame(filename = list.files(paste0(PATH,"/represa"), recursive = TRUE), stringsAsFactors = FALSE)


name_parser(file_list)



rm(imgs_df)
imgs_df <- NULL

for(i in file_list){
  f_name <- name_parser(i)
  target_band <- band_target(f_name$sat, "NIR")

}

imgs_df <- (data.frame(imgs_df, stringsAsFactors = FALSE))
str(imgs_df)
