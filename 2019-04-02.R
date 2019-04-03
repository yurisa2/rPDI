library(imager) # TEM QUE INSTALAR, UTILIZANDO O INSTALADOR DE PACOTES DO R MESMO

# 1 Laranjas
laranjaspath <- '/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi/img/laranjas.jpg' # CAMINHO DA IMAGEM NA SUA MAQUINA
c <- load.image(laranjaspath)
plot(c)
cr <- R(c)
plot(cr)
cg <- G(c)
plot(cg)
cb <- B(c)
plot(cb)

par(mfrow=c(2,2))
plot(c)
plot(cr)
plot(cg)
plot(cb)
par(mfrow=c(1,1))


# 2 MASP

masppath <- '/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi/img/masp.jpg'
c <- load.image(masppath)
plot(c);
PBA <- grayscale(c)
plot(PBA)
hist(PBA)


# 3 Contraste Melhorado
masppath <- '/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi/img/masp.jpg'
c <- load.image(masppath)
plot(c);
PBA <- grayscale(c)
PBA <- PBA + 100
hist(PBA)
PBA <- 255*(PBA-100)/128
plot(PBA)

# 4 Ajuste imagem original

masppath <- '/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi/img/masp.jpg'
c <- load.image(masppath)
plot(c);
PBA <- grayscale(c)
PBA <- 255*(PBA-100)/132
plot(PBA)


