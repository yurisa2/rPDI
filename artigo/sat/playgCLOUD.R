PATH <- "/home/yurisa2/rPDI/artigo/sat"

setwd(PATH)

library("imager")
library("raster")

source(file="functions.R")

rm(file_list)
file_list <- list.files(paste0(PATH,"/represa"), recursive = TRUE)

files_info <- get_table_files(file_list)
str(files_info)

water_size <- get_water_size(files_info)
median_water_size <- median(as.numeric(water_size$area))

water_size <- water_size[as.numeric(water_size$area) < (1.1 * median_water_size) &
                        as.numeric(water_size$area) > (0.8 * median_water_size),  ]

get_aa <- get_all_areas(files_info)
# str(get_aa)
# new_get_aa <- list()
result_image <- get_aa[[paste0(water_size$year[1],water_size$month[1],water_size$day[1])]]
for(i in 2:nrow(water_size)) {
  # new_get_aa[[paste0(water_size$year[i],water_size$month[i],water_size$day[i])]] <- get_aa[[paste0(water_size$year[i],water_size$month[i],water_size$day[i])]]

  result_image <-   result_image + get_aa[[paste0(water_size$year[i],water_size$month[i],water_size$day[i])]]
}


plot(result_image)
