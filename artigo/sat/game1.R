PATH <- "C:/Users/Administrator/Documents/rPDI/artigo/sat"

setwd(PATH)

source(file="functions.R")

library("imager")
library("raster")

list_viable <- list.files("C:/Users/Administrator/Documents/rPDI/artigo/sat/trained/0")
list_viable <- substr(list_viable,1,  nchar(list_viable)-11)

file_list <- list.files(paste0(PATH,"/represa/"), recursive = TRUE)

file_list_clean <- NULL

for(i in file_list) {
  for(j in  list_viable) {
      if(j == substr(i,1,  nchar(j))) {
          file_list_clean <- c(file_list_clean, i)
          break
      }
  }
}

files_info <- get_table_files(file_list_clean)
str(files_info)

water_size <- get_water_size(files_info)
median_water_size <- median(as.numeric(water_size$area))

water_size <- water_size[as.numeric(water_size$area) < (1.2 * median_water_size) &
                        as.numeric(water_size$area) > (0.8 * median_water_size),  ]

str(water_size)

watersort <- water_size[order(water_size$year,water_size$month,water_size$day),]

str(watersort)

jpeg("AreaRepresa.jpg")
boxplot(as.numeric(area) ~ year, data=watersort)
dev.off()

get_aa <- get_all_areas(files_info)
# str(get_aa)
# new_get_aa <- list()
result_image <- get_aa[[paste0(water_size$year[1],water_size$month[1],water_size$day[1])]]
for(i in 2:nrow(water_size)) {
  # new_get_aa[[paste0(water_size$year[i],water_size$month[i],water_size$day[i])]] <- get_aa[[paste0(water_size$year[i],water_size$month[i],water_size$day[i])]]

  result_image <-   result_image + get_aa[[paste0(water_size$year[i],water_size$month[i],water_size$day[i])]]
}

plot(result_image)

# Determinação das faixas
thresh_imagem <- threshold(result_image)
plot(thresh_imagem)

result_invert <- thresh_imagem * -1

grown_500 <- grow(result_invert,17)
grown_1000 <- grow(result_invert,34)
grown_2000 <- grow(result_invert,67)

grown_500 <- renorm(grown_500)
grown_1000 <- renorm(grown_1000)
grown_2000 <- renorm(grown_2000)
result_invert <- renorm(result_invert)

plot(grown_2000)

faixa_500 <- grown_500 *  result_invert
faixa_1000 <- grown_1000 *  result_invert
faixa_2000 <- grown_2000 *  result_invert

plot(faixa_500)


plot(faixa_1000)
plot(faixa_2000)

#Imagens recortadas pelas faixas
fx_500_ndvi <- get_area_ndvi(files_info,faixa_500)
plot(fx_500_ndvi[[1]])
fx_1000_ndvi <- get_area_ndvi(files_info,faixa_1000)
plot(fx_1000_ndvi[[1]])
fx_2000_ndvi <- get_area_ndvi(files_info,faixa_2000)
plot(fx_2000_ndvi[[1]])


ndvi_counts <- get_ndvi_counts(files_info,0.2,faixa_500,faixa_1000,faixa_2000)

ndvi_counts$m500 <- (as.numeric(ndvi_counts$m500))
ndvi_counts$m1000 <- (as.numeric(ndvi_counts$m1000))
ndvi_counts$m2000 <- (as.numeric(ndvi_counts$m2000))


ndvi_counts_sort <- ndvi_counts[order(ndvi_counts$year,ndvi_counts$month,ndvi_counts$day),]

boxplot(as.numeric(m500) ~ year, data=ndvi_counts_sort)
regline <- lm(year ~ as.numeric(m500), data = ndvi_counts_sort)
ab(regline)

boxplot(as.numeric(m1000) ~ year, data=ndvi_counts_sort)
boxplot(as.numeric(m2000) ~ year, data=ndvi_counts_sort)



#
