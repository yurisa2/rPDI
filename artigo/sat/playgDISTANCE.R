PATH <- "C:/Users/Administrator/Documents/rPDI/artigo/sat"

setwd(PATH)

library("imager")
library("raster")

source(file="functions.R")

file_list <- list.files(paste0(PATH,"/represa"), recursive = TRUE)

files_info <- get_table_files(file_list)

str(files_info)

full_nuvem <- load.image("represa/LT05_L1TP_219076_20000202_20161215_01_T1_B4.tif")
teste_full_nuvem <- get_key_values(full_nuvem)
summary(teste_full_nuvem)
no_meio_nuvem <- load.image("represa/LT05_L1TP_219076_20000218_20161216_01_T1_B4.tif")
teste_no_meio_nuvem <- get_key_values(no_meio_nuvem)
plot(no_meio_nuvem)
summary(teste_no_meio_nuvem)
sem_nuvem <- load.image("represa/LT05_L1TP_219076_19991216_20161215_01_T1_B4.tif")
plot(sem_nuvem)
teste_sem_nuvem <- get_key_values(sem_nuvem)
summary(teste_sem_nuvem)



water_size <- get_water_size(files_info)
# summary(as.numeric(water_size$area))
median_water_size <- median(as.numeric(water_size$area))

water_size <- water_size[as.numeric(water_size$area) < (1.5 * median_water_size) &
                        as.numeric(water_size$area) > (0.4 * median_water_size),  ]

get_aa <- get_all_areas(files_info)
# str(get_aa)
# new_get_aa <- list()
result_image <- get_aa[[paste0(water_size$year[1],water_size$month[1],water_size$day[1])]]
plot(result_image)
#
# result_invert <- result_image * -1
#
# grown <- grow(result_image,20)
# par(mfrow=c(2,2))
# plot(result_invert)
# plot(grown)
#
# str(grown)
# str(result_invert)
#
# plot(grown * result_invert)
#
#
# l <- imlist(result_invert,as.cimg(grown))
#
# parall(l) %>% plot
#
# mult <-
# plot(mult)


# for(i in 2:nrow(water_size)) {
#   # new_get_aa[[paste0(water_size$year[i],water_size$month[i],water_size$day[i])]] <- get_aa[[paste0(water_size$year[i],water_size$month[i],water_size$day[i])]]
#
#   result_image <-   result_image + get_aa[[paste0(water_size$year[i],water_size$month[i],water_size$day[i])]]
# }
#
# img_resultante <- (result_image)
# plot(img_resultante)
# img_resultante_df <- as.data.frame(img_resultante)
# summary(img_resultante_df)
# faixa <- img_resultante_df
#
# faixa_d0 <- faixa[faixa$value > 15,]
# summary(faixa_d0)
