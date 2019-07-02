
name_parser <- function(name) {
  matrix(name, ncol= 1)
  resp_df <- NULL


  sensor <- substr(name, 2, 2)
  sat <- substr(name, 3, 4)
  path <- substr(name, 11, 13)
  row <- substr(name, 14, 16)
  aquisicao_ano <- substr(name, 18, 21)
  aquisicao_mes <- substr(name, 22, 23)
  aquisicao_dia <- substr(name, 24, 25)
  band <- substr(name, nchar(name)-4, nchar(name)-4)
  band <- sub("NA", "", paste0(as.numeric(substr(name, nchar(name)-5, nchar(name)-5)),band))


  resp_df <- c(name, sensor,
    sat,
    path,
    row,
    aquisicao_ano,
    aquisicao_mes,
    aquisicao_dia,
    band)


    resp_df <- as.data.frame(matrix(resp_df,ncol = 9),stringAsFactors=FALSE)

    colnames(resp_df) <- c("name", "sensor",
    "sat",
    "path",
    "row",
    "aquisicao_ano",
    "aquisicao_mes",
    "aquisicao_dia",
    "band")

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

  get_table_files <- function(file_list) {
    parser_name <- name_parser(file_list)
    add_names <- add_band_name(parser_name)
    return(add_names)
  }

  add_band_name <- function(parsed_name) {
    band_name <- NULL
    for(i in 1:nrow(parsed_name)) {
      band_name <- rbind(band_name,band_name(as.character(parsed_name$sat[i]),as.character(parsed_name$band[i])))
    }

    band_name <- cbind(parsed_name,band_name)

    return(band_name)
  }

  band_name <- function(sat = 0,band = 0)  {

    ret <- "ERROR"
    sat <- as.numeric(sat)
    band <- as.numeric(band)
    if(is.na(sat) || is.na(band)) return("NO")


    if((sat == 4 || sat == 5) && band == 1) ret <- "blue"
    if((sat == 4 || sat == 5) && band == 2) ret <- "green"
    if((sat == 4 || sat == 5) && band == 3) ret <- "red"
    if((sat == 4 || sat == 5) && band == 4) ret <- "NIR"
    if((sat == 4 || sat == 5) && band == 5) ret <- "SWIR"
    if((sat == 4 || sat == 5) && band == 6) ret <- "TIR"
    if((sat == 4 || sat == 5) && band == 7) ret <- "SWIR2"
    if((sat == 4 || sat == 5) && band == 8) ret <- "pan"

    if((sat == 7 || sat == 8) && band == 2) ret <- "blue"
    if((sat == 7 || sat == 8) && band == 3) ret <- "green"
    if((sat == 7 || sat == 8) && band == 4) ret <- "red"
    if((sat == 7 || sat == 8) && band == 5) ret <- "NIR"
    if((sat == 7 || sat == 8) && band == 6) ret <- "SWIR"
    if((sat == 7 || sat == 8) && band == 7) ret <- "SWIR2"
    if((sat == 7 || sat == 8) && band == 8) ret <- "pan"
    if((sat == 7 || sat == 8) && band == 9) ret <- "cirrus"
    if((sat == 7 || sat == 8) && band == 10) ret <- "TIRS1"
    if((sat == 7 || sat == 8) && band == 11) ret <- "TIRS2"

    return(ret)
  }


  ndvi <- function(nir_img, red_img)  {
    nir_img <- resize(nir_img, size_x = 733, size_y = 466)
    red_img <- resize(red_img, size_x = 733, size_y = 466)

    ret <- NULL
    ret <- (nir_img - red_img) / (nir_img + red_img)
    return(ret)
  }

  ndwi <- function(nir_img, green_img)  {
    nir_img <- resize(nir_img, size_x = 733, size_y = 466)
    green_img <- resize(green_img, size_x = 733, size_y = 466)

    ret <- NULL
    ret <- (nir_img - green_img) / (nir_img + green_img)
    return(ret)
  }

  area_water <- function(px_coords, image , sigma_value = .2) {
    value_px_ref <- 250
    value_px_ref <- at(image,px_coords[1],px_coords[2])
    if(is.na(value_px_ref)) return(0)
    if(value_px_ref > 100) return(0)

    image <- inpaint(image,1)

    seg <- threshold(image,thr = "12%")
    px <- px.flood(seg, x = px_coords[1], y = px_coords[2], sigma = sigma_value)

    return_percentage <- sum(px) / length(px) *100

    return(as.numeric(return_percentage))
  }

  get_key_values <- function(image) {
    image <- renorm(image)

    key_coords <- c(130,185)
    key_coords <- rbind(key_coords,c(111,225))
    key_coords <- rbind(key_coords,c(140,252))
    key_coords <- rbind(key_coords,c(168,248))
    key_coords <- rbind(key_coords,c(200,258))
    key_coords <- rbind(key_coords,c(220,260))
    key_coords <- rbind(key_coords,c(245,215))
    key_coords <- rbind(key_coords,c(300,186))
    key_coords <- rbind(key_coords,c(355,210))
    key_coords <- rbind(key_coords,c(435,195))
    key_coords <- rbind(key_coords,c(450,150))
    key_coords <- rbind(key_coords,c(495,125))
    key_coords <- rbind(key_coords,c(527,115))
    key_coords <- rbind(key_coords,c(550,145))
    key_coords <- rbind(key_coords,c(565,182))
    key_coords <- rbind(key_coords,c(608,197))
    key_coords <- rbind(key_coords,c(670,192))

    value_px_ref <- at(image,key_coords[,1],key_coords[,2])

    return(value_px_ref[,1])
  }


  get_water_size <- function(files_info) {

    files_nir <- files_info[files_info$band_name == "NIR",]
    files_red <- files_info[files_info$band_name == "red",]

    results <- NULL
    for(i in 1:nrow(files_nir)) {
      nir_filename <- files_nir[i,]$name
      nir_ano <- files_nir[i,]$aquisicao_ano
      nir_mes <- files_nir[i,]$aquisicao_mes
      nir_dia <- files_nir[i,]$aquisicao_dia
      red_filename <- files_red[files_red$aquisicao_ano == nir_ano &
      files_red$aquisicao_mes == nir_mes &
      files_red$aquisicao_dia == nir_dia,]$name

      red_filename <- red_filename[1]
      nir_filename <- nir_filename[1]

      nir_img <- load.image(paste0("represa/",nir_filename))
      red_img <- load.image(paste0("represa/",red_filename))

      img_ndvi <- ndvi(nir_img,red_img)
      area_represa <- area_water(c(300,180),img_ndvi)

      results_vector <- c(as.character(nir_ano),as.character(nir_mes),as.character(nir_dia),area_represa)
      results <- rbind(results,results_vector)
    }
    results <- as.data.frame(results, stringsAsFactors = FALSE)
    colnames(results) <- c("year","month","day","area")
    # results <- as.numeric(results)
    return(results)
  }

  get_all_areas <- function(files_info) {

    files_nir <- files_info[files_info$band_name == "NIR",]
    files_red <- files_info[files_info$band_name == "red",]

    results <- list()
    for(i in 1:nrow(files_nir)) {
      nir_filename <- files_nir[i,]$name
      nir_ano <- files_nir[i,]$aquisicao_ano
      nir_mes <- files_nir[i,]$aquisicao_mes
      nir_dia <- files_nir[i,]$aquisicao_dia
      red_filename <- files_red[files_red$aquisicao_ano == nir_ano &
      files_red$aquisicao_mes == nir_mes &
      files_red$aquisicao_dia == nir_dia,]$name

      red_filename <- red_filename[1]
      nir_filename <- nir_filename[1]

      nir_img <- load.image(paste0("represa/",nir_filename))
      red_img <- load.image(paste0("represa/",red_filename))

      nir_img <- resize(nir_img, size_x = 733, size_y = 466)
      red_img <- resize(red_img, size_x = 733, size_y = 466)

      img_ndvi <- ndvi(nir_img,red_img)

      # img_ndvi <- inpaint(img_ndvi,1)
      img_ndvi <- inpaint(img_ndvi,1)

      seg <- threshold(img_ndvi,thr = "12%")
      px <- px.flood(seg, x = 300, y = 180, sigma=.2)
      results[[paste0(nir_ano,nir_mes,nir_dia)]] <- px
    }
    return(results)
  }


    get_area_ndvi <- function(files_info,area_img) {

      files_nir <- files_info[files_info$band_name == "NIR",]
      files_red <- files_info[files_info$band_name == "red",]

      results <- list()
      for(i in 1:nrow(files_nir)) {
        nir_filename <- files_nir[i,]$name
        nir_ano <- files_nir[i,]$aquisicao_ano
        nir_mes <- files_nir[i,]$aquisicao_mes
        nir_dia <- files_nir[i,]$aquisicao_dia
        red_filename <- files_red[files_red$aquisicao_ano == nir_ano &
        files_red$aquisicao_mes == nir_mes &
        files_red$aquisicao_dia == nir_dia,]$name

        red_filename <- red_filename[1]
        nir_filename <- nir_filename[1]

        nir_img <- load.image(paste0("represa/",nir_filename))
        red_img <- load.image(paste0("represa/",red_filename))

        nir_img <- resize(nir_img, size_x = 733, size_y = 466)
        red_img <- resize(red_img, size_x = 733, size_y = 466)

        img_ndvi <- ndvi(nir_img,red_img)

        # img_ndvi <- inpaint(img_ndvi,1)
        img_ndvi <- inpaint(img_ndvi,1)

        px <- area_img * img_ndvi

        px <- renorm(px, min = -1, max = 1)

        results[[paste0(nir_ano,nir_mes,nir_dia)]] <- px
      }
      return(results)
    }


        get_area_ndvi_inside <- function(img_ndvi,area_img) {
          results <- NULL

            px <- area_img * img_ndvi

            px <- renorm(px, min = -1, max = 1)

            results <- px

          return(results)
        }

  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }

  normalize <- function(x) {
    return ((x - (min(x,na.rm=TRUE))) / (max(x,na.rm=TRUE) - min(x,na.rm=TRUE)))
  }

  get_below_count <- function(img,thresh) {

    img_df <- as.data.frame(img)
    # str(img_df)
    img_df <- img_df[img_df$value > 0,]
    # str(img_df)
    img_df$value <- normalize(img_df$value)
    img_df$value <- ifelse(img_df$value < thresh,1,0)
    sum(img_df$value)
    length(img_df$value)

    return (sum(img_df$value) / length(img_df$value))
  }


    get_ndvi_counts <- function(files_info,thresh,faixa_500,faixa_1000,faixa_2000) {

      files_nir <- files_info[files_info$band_name == "NIR",]
      files_red <- files_info[files_info$band_name == "red",]

      results <- NULL
      for(i in 1:nrow(files_nir)) {
        nir_filename <- files_nir[i,]$name
        nir_ano <- files_nir[i,]$aquisicao_ano
        nir_mes <- files_nir[i,]$aquisicao_mes
        nir_dia <- files_nir[i,]$aquisicao_dia
        red_filename <- files_red[files_red$aquisicao_ano == nir_ano &
        files_red$aquisicao_mes == nir_mes &
        files_red$aquisicao_dia == nir_dia,]$name

        red_filename <- red_filename[1]
        nir_filename <- nir_filename[1]

        nir_img <- load.image(paste0("represa/",nir_filename))
        red_img <- load.image(paste0("represa/",red_filename))

        img_ndvi <- ndvi(nir_img,red_img)

        fx_500_ndvi <- get_area_ndvi_inside(img_ndvi,faixa_500)
        fx_1000_ndvi <- get_area_ndvi_inside(img_ndvi,faixa_1000)
        fx_2000_ndvi <- get_area_ndvi_inside(img_ndvi,faixa_2000)

        fx_500_ndvi_count <- get_below_count(fx_500_ndvi,thresh)
        fx_1000_ndvi_count <- get_below_count(fx_1000_ndvi,thresh)
        fx_2000_ndvi_count <- get_below_count(fx_2000_ndvi,thresh)

        results_vector <- c(as.character(nir_ano),as.character(nir_mes),as.character(nir_dia),
        fx_500_ndvi_count,fx_1000_ndvi_count,fx_2000_ndvi_count)
        results <- rbind(results,results_vector)
      }
      results <- as.data.frame(results, stringsAsFactors = FALSE)
      colnames(results) <- c("year","month","day","m500","m1000","m2000")
      # results <- as.numeric(results)
      return(results)
    }
