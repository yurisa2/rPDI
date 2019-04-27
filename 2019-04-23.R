library(imager)

setwd('/home/yurisa2/rPDI')

matrix <- c(0,0,0,0,0,0)
matrix <- rbind(matrix,c(0,3,2,4,9,0))
matrix <- rbind(matrix,c(0,7,1,6,5,0))
matrix <- rbind(matrix,c(0,4,2,1,5,0))
matrix <- rbind(matrix,c(0,3,1,2,4,0))
matrix <- rbind(matrix,c(0,0,0,0,0,0))

kernel <- c(2,3,2)
kernel <- rbind(kernel,c(3,4,3))
kernel <- rbind(kernel,c(2,3,2))

result <- convolve(as.cimg(matrix),as.cimg(kernel))


plot(result)

as.matrix(result)


resultado_mat <- as.matrix(result)

str(resultado_mat)

resultado_mat[1,] <- 0
resultado_mat[6,] <- 0

resultado_mat[,1] <- 0
resultado_mat[,6] <- 0

resultado_mat



resultado_sub <- resultado_mat[2:5,2:5]

resultado_img <- round(renorm(as.cimg(resultado_sub)))

as.matrix(resultado_img)


filtro_medio <- matrix(c(1,1,1,1,1,1,1,1,1), nrow=3)

filtro_laplaciano <- matrix(c(0,-1,0,-1,4,-1,0,-1,0), nrow=3)

imagem <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0), nrow=5)

resultado_medio_laplaciano <- convolve(as.cimg(imagem),as.cimg(filtro_medio))
resultado_medio_laplaciano <- convolve(as.cimg(resultado_medio_laplaciano),as.cimg(filtro_laplaciano))
as.matrix(resultado_medio_laplaciano)

resultado_laplaciano_medio <- convolve(as.cimg(imagem),as.cimg(filtro_laplaciano))
resultado_laplaciano_medio <- convolve(as.cimg(resultado_laplaciano_medio),as.cimg(filtro_medio))
as.matrix(resultado_laplaciano_medio)

masp_ruido <- load.image('img/maspcomruido.jpg')
masp_ruido <- grayscale(masp_ruido)
masp_medio_laplaciano <- convolve(as.cimg(masp_ruido),as.cimg(filtro_laplaciano))
masp_medio_laplaciano <- convolve(as.cimg(masp_medio_laplaciano),as.cimg(filtro_medio))
summary(masp_medio_laplaciano)

plot(masp_medio_laplaciano)

masp_puro <- load.image('img/masp.jpg')
masp_puro <- grayscale(masp_puro)
masp_medio <- convolve(as.cimg(masp_puro),as.cimg(filtro_medio))
plot(masp_medio)
