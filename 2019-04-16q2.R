library(imager)

setwd("/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi")
im <- load.image("img/placa.jpg")
im <- grayscale(im)

# k <- 2
# Convolucao Placa
# kernel <- as.cimg(matrix(c(-k/8,-k/8,-k/8,-k/8,k+1,-k/8,-k/8,-k/8,-k/8),nrow=3,ncol=3))
# kernel <- as.cimg(matrix(c(-k/8,-k/8,-k/8,-k/8,k+1,-k/8,-k/8,-k/8,1),nrow=3,ncol=3))
# im <- convolve(im,kernel)


df <- as.data.frame(im)
# head(df)

line_mean <- aggregate(df$value,by=list(x=df$x), FUN=mean)
colnames(line_mean)[2] <- "media"

# plot(line_mean)
# head(line_mean)



line_mean_lm <- lm(df$value ~ df$x)

# head(df)

df2 <- df

df2$value <- df$value + (df$value * (-1 * line_mean_lm$coefficients[2] * df$x *  3))

# head(df2)
# hist(im)
# hist(as.cimg(df2))

line_mean_lm2 <- lm(df2$value ~ df2$x)

plot(im)
plot(as.cimg(df2))

thres2 <- renorm(as.cimg(df2))
thres2 <- as.cimg(thres2)
thres2 <- renorm(thres2)

hist(thres2)
thres3 <- threshold(thres2, thr = 120)

plot(thres3)

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
