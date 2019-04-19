library(imager)

setwd("/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi")
im <- load.image("img/placa.jpg")
im <- grayscale(im)

k <- 2
# Convolucao Placa
# kernel <- as.cimg(matrix(c(-k/8,-k/8,-k/8,-k/8,k+1,-k/8,-k/8,-k/8,-k/8),nrow=3,ncol=3))
# kernel <- as.cimg(matrix(c(-k/8,-k/8,-k/8,-k/8,k+1,-k/8,-k/8,-k/8,1),nrow=3,ncol=3))
# im <- convolve(im,kernel)


df <- as.data.frame(im)


m <- lm(value ~ x + y,data=df) #linear trend
# summary(m)


im <- im-fitted(m)
hist(renorm(im))

w_size <- 6

medias <- NULL
for(ix in 1:max(df$x)) for (iy in 1:max(df$y)) {
  media_calc <- mean(subset(df, x > ix-w_size & x < ix+w_size & y > iy-w_size & y < iy+w_size )$value)
  medias <- c(medias,media_calc)
}





dfc <- cbind(df,medias)

bin <- ifelse(dfc$value > dfc$medias , 1,0)
comb_total <- cbind(dfc,bin)

result <- NULL
result$x <- comb_total$x
result$y <- comb_total$y
result$value <- comb_total$bin

imagem <- as.cimg(as.data.frame(result))
plot(imagem)
