PATH <- "C:/Users/Administrator/Documents/rPDI/artigo/sat"

setwd(PATH)

library("imager")
library("raster")

file_list <- list.files(paste0(PATH,"/represa"), recursive = TRUE)

name_parser <- function(name) {
  # substr(x, first, last)

  sensor <- substr(name, 2, 2)
  sat <- substr(name, 3, 4)
  path <- substr(name, 11, 13)
  row <- substr(name, 14, 16)
  aquisicao_ano <- substr(name, 18, 21)
  aquisicao_mes <- substr(name, 22, 23)
  aquisicao_dia <- substr(name, 24, 25)
  band <- substr(name, nchar(name)-4, nchar(name)-4)
  if(is.na(as.numeric(substr(name, nchar(name)-5, nchar(name)-5))) == FALSE) {
    band <- paste0(substr(name, nchar(name)-5, nchar(name)-5),band)
  }

  resp_df <- data.frame(sensor = "",
                        sat = "",
                        path = "",
                        row = "",
                        aquisicao_ano = "",
                        aquisicao_mes ="",
                        aquisicao_dia = "",
                        band = "",
                        stringsAsFactors=FALSE)

  resp_df <- rbind(resp_df,c(sensor,sat,path,row,aquisicao_ano,aquisicao_mes,aquisicao_dia,band))

  resp_df <- resp_df[2,]

  return(resp_df)
}

# name_parser(gl_i)



band_target <- function(sat,name)  {
  ret <- NULL
  sat <- as.numeric(sat)
  if(sat == 4 || sat == 5) {
    if(name == "blue") ret <- 1
    if(name == "green") ret <- 2
    if(name == "red") ret <- 3
    if(name == "NIR") ret <- 4
    if(name == "SWIR") ret <- 5
    if(name == "TIR") ret <- 6
    if(name == "SWIR2") ret <- 7
    if(name == "pan") ret <- 8
  }

  if(sat == 7 || sat == 8) {
    if(name == "blue") ret <- 2
    if(name == "green") ret <- 3
    if(name == "red") ret <- 4
    if(name == "NIR") ret <- 5
    if(name == "SWIR") ret <- 6
    if(name == "SWIR2") ret <- 7
    if(name == "pan") ret <- 8
    if(name == "cirrus") ret <- 9
    if(name == "TIRS1") ret <- 10
    if(name == "TIRS2") ret <- 11
  }
  return(as.numeric(ret))
}
#
# band_target(8,"red")
# band_target( name_parser(gl_i)$sat, "NIR")

rm(imgs_df)
imgs_df <- NULL

for(i in file_list){
  f_name <- name_parser(i)
  target_band <- band_target(f_name$sat, "NIR")

  if(f_name$band == target_band) {

  img_alvo <- load.image(paste0("represa/",i))
  img_alvo <- renorm(img_alvo)
  img_alvo_df <- as.data.frame(img_alvo)
  pixel_ref <- img_alvo_df[img_alvo_df$x == 300 & img_alvo_df$y == 180,]

  imgs_df <- rbind(imgs_df,c(i,pixel_ref$value))

  }
}

summary(imgs_df)
