PATH <- "C:/Users/Administrator/Documents/rPDI/artigo/sat"

setwd(PATH)

source(file="functions.R")

library("imager")
library("raster")

img_alvo <- load.image("represa/LT05_L1TP_219076_20070613_20161114_01_T1_B5.tif")

# img_alvo_df <- as.data.frame(img_alvo)
#
#
# # pixel_ref <- img_alvo_df[img_alvo_df$x == 300 & img_alvo_df$y == 180,]
#
# seg <- threshold(img_alvo,thr = "12%")
#
# px <- px.flood(seg, x = 300, y = 180, sigma=.2)
#
#
#
# par(mfrow=c(2,2))
# plot(img_alvo)
# plot(px)
# sum(px) / length(px) *100


area_water(c(300,180),img_alvo)
