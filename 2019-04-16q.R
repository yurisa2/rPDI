library(imager)
library(magick)
setwd("/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi")
A <- load.image("img/nico.jpg")
PBA <- grayscale(A)
par(mfrow=c(2,2))
plot(PBA)
hist(PBA)
PBAb <- threshold(PBA,(150/255))
plot(PBAb)


hema <- load.image("img/hemacias.jpg")
hema <- grayscale(hema)
par(mfrow=c(2,2))
plot(hema)
hist(hema)
hema2 <- threshold(hema,(180/255))
plot(hema2)

# Exibir somente as laranjas
C_Original <- load.image("img/laranjas.jpg")
Binaria <- ifelse(C_Original[,,,1] < (140/255),0,1)
C_Original[,,,1] <- C_Original[,,,1] * Binaria
C_Original[,,,2] <- C_Original[,,,2] * Binaria
C_Original[,,,3] <- C_Original[,,,3] * Binaria
plot(C_Original)

# Convolucao Folha
folha <- load.image("img/folha2.jpg")
folha <-grayscale(folha)
kernel <- as.cimg(matrix(c(-1,-1,-1,-1,8,-1,-1,-1,-1),nrow=3,ncol=3))
conv_folha <- convolve(folha,kernel)
conv_folha[,,,1] <- ifelse(conv_folha[,,,1] < 0, conv_folha*0, conv_folha*1)
conv_folha[,,,1] <- ifelse(conv_folha[,,,1] > 1, 1, conv_folha*1)
plot(conv_folha, main = "Convolução Folha")

# Convolucao Nico
nico <- load.image("img/nico.jpg")
nico <- grayscale(nico)
nico_t <- threshold(nico,(150/255))
kernel <- as.cimg(matrix(c(-1,-1,-1,-1,8,-1,-1,-1,-1),nrow=3,ncol=3))
nico_mask <- convolve(nico,kernel)
nico_mask[,,,1] <- ifelse(nico_mask[,,,1] < 0, nico_mask*0, nico_mask*1)
nico_mask[,,,1] <- ifelse(nico_mask[,,,1] > 1, 1, nico_mask*1)

nico_t_mask <- convolve(nico_t,kernel)
nico_t_mask[,,,1] <- ifelse(nico_t_mask[,,,1] < 0, nico_t_mask*0, nico_t_mask*1)
nico_t_mask[,,,1] <- ifelse(nico_t_mask[,,,1] > 1, 1, nico_t_mask*1)

par(mfrow=c(2,2))
plot(nico, main = "Img Original")
plot(nico_mask, main = "Mascara Img Original")
plot(nico_t, main = "Detecção de limiar (>150)")
plot(nico_t_mask, main = "Mascara c/ Det. Limiar")

# Convolucao Canal R Laranjas
laranjas <- load.image("img/laranjas.jpg")
laranjas_R <- as.matrix(channels(laranjas,1)[[1]])
kernel <- as.cimg(matrix(c(-1,-1,-1,-1,8,-1,-1,-1,-1),nrow=3,ncol=3))
laranjas_R_conv_result <- laranjas
laranjas_R <- as.cimg(laranjas_R)
laranjas_R_conv <- convolve(laranjas_R,kernel)
laranjas_R_conv[,,,1] <- ifelse(laranjas_R_conv[,,,1] < 0, laranjas_R_conv*0, laranjas_R_conv*1)
laranjas_R_conv[,,,1] <- ifelse(laranjas_R_conv[,,,1] > 1, 1, laranjas_R_conv*1)
laranjas_R_conv_result[,,,1] <- laranjas_R_conv

par(mfrow=c(2,2))
plot(laranjas, main = "Imagem Original")
plot(laranjas_R, main = "Canal Vermelho (R)")
plot(laranjas_R_conv, main = "Convolução do canal Vermelho")
plot(laranjas_R_conv_result, main = "Imagem reconstituida")
