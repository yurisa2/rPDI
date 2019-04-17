library(imager)

A <- load.image("img/nico.jpg")
PBA <- grayscale(A)
par(mfrow=c(2,2))
plot(PBA)
hist(PBA)
PBAb <- threshold(PBA,0.58)
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
folha <- grayscale(folha)
kernel <- as.cimg(matrix(c(-1,-1,-1,-1,8,-1,-1,-1,-1),nrow=3,ncol=3))
convolutada <- convolve(folha,kernel)
plot(convolutada)

# Convolucao Nico
nico <- load.image("img/nico.jpg")
nico <- grayscale(nico)
nico <- threshold(nico,0.58)
kernel <- as.cimg(matrix(c(-1,-1,-1,-1,8,-1,-1,-1,-1),nrow=3,ncol=3))
convolutada <- convolve(nico,kernel)
plot(convolutada)

# Convolucao Canal R Laranjas
laranjas <- load.image("img/laranjas.jpg")
laranjas_R <- channels(laranjas,1)
kernel <- as.cimg(matrix(c(-1,-1,-1,-1,8,-1,-1,-1,-1),nrow=3,ncol=3))
laranjas_R <- convolve(laranjas_R[[1]],kernel)
plot(laranjas_R)
laranjas[,,,1] <- laranjas_R
plot(laranjas)
