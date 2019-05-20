imgB5 <- "/home/yurisa2/Downloads/itupararanga/imgMestrado1/LC08_L1TP_219076_20190326_20190403_01_T1_B5.TIF_represa.tif"
imgB4 <- "/home/yurisa2/Downloads/itupararanga/imgMestrado1/LC08_L1TP_219076_20190326_20190403_01_T1_B4.TIF_represa.tif"
imgB3 <- "/home/yurisa2/Downloads/itupararanga/imgMestrado1/LC08_L1TP_219076_20190326_20190403_01_T1_B3.TIF_represa.tif"

library(imager)

imgB5_file <- load.image(imgB5)
imgB4_file <- load.image(imgB4)
imgB3_file <- load.image(imgB3)

NDVI <- (imgB5_file - imgB4_file)/(imgB5_file + imgB4_file)
NDWI <- (imgB5_file - imgB3_file)/(imgB5_file + imgB3_file)

par(mfrow=c(2,2))
plot(NDVI)
plot(NDWI)

NDVI_DF <- as.data.frame(NDVI)
NDWI_DF <- as.data.frame(NDWI)

valor <- NDVI_DF[NDVI_DF$x == 300 & NDVI_DF$y == 200,]
valor <- NDWI_DF[NDWI_DF$x == 300 & NDWI_DF$y == 200,]



summary(NDWI)
plot(threshold(NDWI, thr = "auto"))

