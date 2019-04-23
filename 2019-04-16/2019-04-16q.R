library(imager)

# setwd("/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi")
quarto <- load.image("img/quarto.jpg")
quarto <- grayscale(quarto)
plot(quarto)

df <- as.data.frame(quarto)

# summary(df)

ln_250 <- df[df$x==250,3]

# summary(ln_250)
# str(ln_250)

fit2b <- lm(ln_250 ~ poly(1:375, 2, raw=TRUE))

# summary(fit2b)

fit2b_hecho <- ln_250-fitted(fit2b)

# plot(fit2b_hecho)

df2 <- df

for(i in 1:max(df$x)) {

line_temp <- df[df$x==i,3]
fit_temp <- lm(line_temp ~ poly(1:max(df$y), 2, raw=TRUE))
line_temp <- line_temp-fitted(fit_temp)

df2[df2$x==i,3]<- line_temp

}

# str(df2)
# plot(as.cimg(df2))
df2_t <- threshold(as.cimg(df2),thr="15%")

jpeg("quarto_seg.jpg")
par(mfrow=c(2,2))
plot(ln_250,main = "Linha x=250")
plot(fit2b_hecho, main = "Fit ln x=250")
plot(quarto, main = "Quarto original" )
plot(df2_t, main = "Quarto Segmentado" )
dev.off()
