
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
  #
  # resp_df <- data.frame(sensor = "",
  #                       sat = "",
  #                       path = "",
  #                       row = "",
  #                       aquisicao_ano = "",
  #                       aquisicao_mes ="",
  #                       aquisicao_dia = "",
  #                       band = "",
  #                       stringsAsFactors=FALSE)

  resp_df <- c(sensor,sat,path,row,aquisicao_ano,aquisicao_mes,aquisicao_dia,band)

  # resp_df <- resp_df[2,]

  return(resp_df)
}

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

ndvi <- function(nir_img, red_img)  {
  ret <- NULL
  ret <- (nir_img - red_img) / (nir_img + red_img)
  return(ret)
}

ndwi <- function(nir_img, green_img)  {
  ret <- NULL
  ret <- (nir_img - green_img) / (nir_img + green_img)
  return(ret)
}

area_water <- function(px_coords, image , sigma_value = .2) {

  seg <- threshold(image,thr = "12%")
  px <- px.flood(seg, x = px_coords[1], y = px_coords[2], sigma = sigma_value)

  return_percentage <- sum(px) / length(px) *100

  return(as.numeric(return_percentage))
}
