library(imager)

setwd("/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi")
quarto <- load.image("img/quarto.jpg")
plot(quarto)

df <- as.data.frame(quarto)


summary(df)

plot(df$value[df$y==188])


m <- lm(value ~ poly(x,2,raw=TRUE),data=df) #linear trend
summary(m)




m$coefficient[1]
m$coefficient[2]
m$coefficient[3]

plot(im)
plot(quarto)
