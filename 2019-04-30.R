# Q1
library(imager)

install.packages('imager')

laplaciano <- matrix(c(0,-1,0,-1,4,-1,0,-1,0), nrow=3)

prewitt_0 <- matrix(c(-1,-1,-1,0,0,0,1,1,1), nrow=3)
prewitt_90 <- matrix(c(-1,0,1,-1,0,1,-1,0,1), nrow=3)

q1ln1 <- c(0,2,4,2,2,1)
q1ln2 <- c(1,0,121,124,123,0)
q1ln3 <- c(1,120,123,122,121,1)
q1ln4 <- c(1,121,125,123,1,2)
q1ln5 <- c(3,121,125,125,129,3)
q1ln6 <- c(2,0,1,1,2,1)
matrixq1 <- rbind(q1ln1,q1ln2,q1ln3,q1ln4,q1ln5,q1ln6)
matrixq1 <- as.cimg(matrixq1)

matrixq1_laplaciano <- convolve(matrixq1,as.cimg(laplaciano))
print("Matrix com filtro laplaciano")
laplaciano
as.matrix(matrixq1_laplaciano, nrow=6) # OK, issaque

matrixq1_prewitt <- convolve(matrixq1,as.cimg(prewitt_0))
print("Matrix com filtro Prewitt 0o")
as.matrix(matrixq1_prewitt, nrow=6) # OK, issaque

matrixq1_prewitt <- convolve(matrixq1_prewitt,as.cimg(prewitt_90))
print("Matrix com filtro Prewitt 90o")
as.matrix(matrixq1_prewitt, nrow=6) # OK, issaque







# 3

kernel <- matrix(nrow=3)
kernel <- c(2,3,2)
kernel <- rbind(kernel,c(3,4,3))
kernel <- rbind(kernel,c(2,3,2))

matrixq3 <- c(1,2,1,3,1,1)
matrixq3 <- rbind(matrixq3,c(1,1,2,1,4,2))
matrixq3 <- rbind(matrixq3,c(2,3,15,15,1,2))
matrixq3 <- rbind(matrixq3,c(2,3,16,15,0,1))
matrixq3 <- rbind(matrixq3,c(4,2,1,5,3,1))
matrixq3 <- rbind(matrixq3,c(3,1,2,4,1,2))


kernel <- matrix(nrow=3)
kernel <- c(2,3,2)
kernel <- rbind(kernel,c(3,4,3))
kernel <- rbind(kernel,c(2,3,2))


matrix_q3_r <- convolve(as.cimg(matrixq3),as.cimg(kernel))
as.matrix(matrix_q3_r,max=255,min=0)

as.matrix(round(renorm(matrix_q3_r,max=255,min=0)))


# 4

matrix_4 <- c(1,3,5)
matrix_4 <- rbind(matrix_4,c(2,0,3))
matrix_4 <- rbind(matrix_4,c(0,1,0))
matrix_4 <- as.cimg(matrix_4)
matrix_4_resize <- resize(matrix_4,size_x = 5, size_y = 5, interpolation_type = 3)
as.matrix(round(matrix_4_resize))
