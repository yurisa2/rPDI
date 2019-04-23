library(imager)

# setwd("/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi")
quarto <- load.image("img/quarto.jpg")
quarto <- grayscale(quarto)
plot(quarto)

df <- as.data.frame(quarto)

summary(df)

ln_250 <- df[df$x==250,3]

summary(ln_250)
str(ln_250)

fit2b <- lm(ln_250 ~ poly(1:375, 2, raw=TRUE))

summary(fit2b)

fit2b_hecho <- ln_250-fitted(fit2b)

plot(fit2b_hecho)
