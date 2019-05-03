library(imager)
setwd("/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi/artigo/")

source(file="afis_fuzzysis/include/functions.R")

# doente <- load.image("/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi/artigo/test/Parasitized/C39P4thinF_original_IMG_20150622_105335_cell_9.png")
# doente_df <- as.data.frame(doente)
# doente_r <- subset(doente_df$value,doente_df$cc==1)
# doente_r <- doente_r[doente_r!=0]
# doente_g <- subset(doente_df$value,doente_df$cc==2)
# doente_g <- doente_g[doente_g!=0]
# doente_b <- subset(doente_df$value,doente_df$cc==3)
# doente_b <- doente_b[doente_b!=0]




# sadia <- load.image("/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi/artigo/test/Uninfected/C2NThinF_IMG_20150604_114631_cell_187.png")
# sadia_df <- as.data.frame(sadia)
# sadia_r <- subset(sadia_df$value,sadia_df$cc==1)
# sadia_r <- sadia_r[sadia_r!=0]
# sadia_g <- subset(sadia_df$value,sadia_df$cc==2)
# sadia_g <- sadia_g[sadia_g!=0]
# sadia_b <- subset(sadia_df$value,sadia_df$cc==3)
# sadia_b <- sadia_b[sadia_b!=0]
# par(mfrow=c(3,2))
# hist(sadia_r, xlim=c(0,1))
# hist(doente_r, xlim=c(0,1))
# hist(sadia_g, xlim=c(0,1))
# hist(doente_g, xlim=c(0,1))
# hist(sadia_b, xlim=c(0,1))
# hist(doente_b, xlim=c(0,1))




process_images <- function(path_set, dataset_type_bin, limit = 20, start = 0) {
  img_list <- list.files(path_set)

  table_img <- NULL

  counter <- 1
  for (i in img_list) {
    if(counter > start) {
    img <- load.image(paste0(path_set,i))
    img_df <- as.data.frame(img)
    # REMOVER o RECORTE AQUI, POSSIVELMENTE FAZER AS CONTAS PARA SABER
    # SE A INFECCAO TEM ALguM TOM DIFERENTE DE 0
    img_r <- subset(img_df$value,img_df$cc==1)
    img_r <- img_r[img_r!=0] #MELHORAR AS FUNCOES DE RECORTE
    img_g <- subset(img_df$value,img_df$cc==2)
    img_g <- img_g[img_g!=0]
    img_b <- subset(img_df$value,img_df$cc==3)
    img_b <- img_b[img_b!=0]
    feature_img <- dataset_type_bin

    feature_img <- c(feature_img,as.array(mean(img_r)))
    feature_img <- c(feature_img,as.array(mean(img_g)))
    feature_img <- c(feature_img,as.array(mean(img_b)))
    feature_img <- c(feature_img,as.array(min(img_r)))
    feature_img <- c(feature_img,as.array(min(img_g)))
    feature_img <- c(feature_img,as.array(min(img_b)))
    feature_img <- c(feature_img,as.array((max(img_r) - min(img_r))))
    feature_img <- c(feature_img,as.array((max(img_g) - min(img_g))))
    feature_img <- c(feature_img,as.array((max(img_b) - min(img_b))))

    # FAZER AQUI UMA SERIE COM CALCULOS NORMALIZADOS
    # FAZER AQUI UMA SERIE COM CALCULOS GRAYSCALE
    # Transformar os dados de imagens em funcao.

    table_img <- rbind(table_img,feature_img)

    table_img <- as.data.frame(table_img)
    colnames(table_img) <- NULL
    rownames(table_img) <- NULL
  }

    counter <- counter + 1
    if(counter == (limit + 1 + start)) break
  }
  return(as.data.frame(table_img))
}

training_1 <- process_images("/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi/artigo/cell_images/Parasitized/",1, limit = 100)
training_0 <- process_images("/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi/artigo/cell_images/Uninfected/",0, limit = 100)

total_training <- rbind(as.matrix(training_0),as.matrix(training_1))
total_training <- as.data.frame(total_training)


actual_1 <- process_images("/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi/artigo/cell_images/Parasitized/",1, limit = 500, start = 101)
actual_0 <- process_images("/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi/artigo/cell_images/Uninfected/",0, limit = 500, start = 101)
total_actual <- rbind(as.matrix(actual_0),as.matrix(actual_1))
total_actual <- as.data.frame(total_actual)

possb_feat <- 2:ncol(total_actual)
nbin <- 1

# weights_type <- "dynamic"

results <- result_matrix(total_training,total_actual,possb_feat,nbin)
# str(results)

confusionMatrix(factor(results$Eval1),factor(results$Benchmark),positive = "1")
