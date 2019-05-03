library(imager)
setwd("/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi/artigo/")

source(file="afis_fuzzysis/include/functions.R")

process_images <- function(path_set, dataset_type_bin, limit = 20) {
  img_list <- list.files(path_set)

  table_img <- NULL

  counter <- 1
  for (i in img_list) {
    img <- load.image(paste0(path_set,i))
    img_df <- as.data.frame(img)
    img_r <- subset(img_df$value,img_df$cc==1)
    img_r <- img_r[img_r!=0]
    img_g <- subset(img_df$value,img_df$cc==2)
    img_g <- img_g[img_g!=0]
    img_b <- subset(img_df$value,img_df$cc==3)
    img_b <- img_b[img_b!=0]
    feature_img <- dataset_type_bin
    feature_img <- c(feature_img,as.array(summary(img_r)))
    feature_img <- c(feature_img,as.array(summary(img_g)))
    feature_img <- c(feature_img,as.array(summary(img_b)))

    table_img <- rbind(table_img,feature_img)

    table_img <- as.data.frame(table_img)
    colnames(table_img) <- NULL
    rownames(table_img) <- NULL

    counter <- counter + 1
    if(counter == (limit + 1)) break
  }
  return(as.data.frame(table_img))
}

training_1 <- process_images("/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi/artigo/cell_images/Parasitized/",1, limit = 100)
training_0 <- process_images("/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi/artigo/cell_images/Uninfected/",0, limit = 100)

total_training <- rbind(as.matrix(training_0),as.matrix(training_1))
total_training <- as.data.frame(total_training)


actual_1 <- process_images("/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi/artigo/cell_images/Parasitized/",1, limit = 1000)
actual_0 <- process_images("/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi/artigo/cell_images/Uninfected/",0, limit = 1000)
total_actual <- rbind(as.matrix(actual_0),as.matrix(actual_1))
total_actual <- as.data.frame(total_actual)
str(total_actual)

possb_feat <- 2:19
nbin <- 1

weights_type <- "fixed"

results <- result_matrix(total_training,total_actual,possb_feat,nbin, plots = FALSE)
str(results)

confusionMatrix(factor(results$Eval1),factor(results$Benchmark))
