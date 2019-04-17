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

C_R <- channels(C_Original,1)[[1]]
str(C_R)
Binaria <- threshold(C_R,(160/255))
C_G <- channels(C_Original,2)[[1]]
C_B <- channels(C_Original,3)[[1]]

C_B_ <- C_B * Binaria


as.cimg(rep(1:155,3),rep(1:155,3),NA,cores) #10x10 RGB


plot(test_image)

# Convolucao
folha <- load.image("img/folha2.jpg")
plot(folha)
