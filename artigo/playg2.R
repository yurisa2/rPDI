library(imager)
setwd("/home/yurisa2/lampstack-5.6.22-0/apache2/htdocs/rpdi/artigo/")

source(file="afis_fuzzysis/include/functions.R")

doente <- load.image("test/Parasitized/C39P4thinF_original_IMG_20150622_105335_cell_9.png")


doente <- grayscale(doente)

doente_df <- as.data.frame(doente)
str(doente_df)

doente_new <- doente_df

media_new <- mean(doente_df$value[doente_df$value !=0])

for(i in 4:length(doente_df$value)) {

doente_new$value[i] <- doente_df$value[i] ^ doente_df$value[i-1]

}

doente_new <- doente_df

doente_new$value <- ifelse(doente_new$value > media_new, doente_new$value * 1.1, doente_new$value * 0.9)

# summary(doente_new)
# summary(doente_df)
par(mfrow = c(2,2))
plot(as.cimg(doente_df))
plot(as.cimg(doente_new))

norm <- doente_df
norm$value <- ifelse(norm$value != 0, normalize(norm$value),norm$value)

# doente_df <- norm

hist(doente_df$value)
hist(norm$value)

thr <- quantile(doente_df[doente_df$value!=0,]$value)[2]

doente_df$value <- ifelse(doente_df$value > thr,1,0)

head(doente_df)

plot(as.cimg(doente_df))



    #
    # img_hsv <- RGBtoHSV(doente)
    # img_df_hsv <- as.data.frame(img_hsv)
    #
    # img_1_hsv <- subset(img_df_hsv$value,img_df_hsv$cc==1)
    # img_1_hsv <- img_1_hsv[img_1_hsv!=0] #MELHORAR AS FUNCOES DE RECORTE
    # img_2_hsv <- subset(img_df_hsv$value,img_df_hsv$cc==2)
    # img_2_hsv <- img_2_hsv[img_2_hsv!=0]
    # img_3_hsv <- subset(img_df_hsv$value,img_df_hsv$cc==3)
    # img_3_hsv <- img_3_hsv[img_3_hsv!=0]



# img_df <- img_df[img_df$value!=0,] #MELHORAR AS FUNCOES DE RECORTE
#
# doente_f <- convert_image(doente)

convert_image <- function(img,dataset_type_bin = 0) {
  img_df <- as.data.frame(img)

  img_r <- subset(img_df$value,img_df$cc==1)
  img_r <- img_r[img_r!=0] #MELHORAR AS FUNCOES DE RECORTE
  img_g <- subset(img_df$value,img_df$cc==2)
  img_g <- img_g[img_g!=0]
  img_b <- subset(img_df$value,img_df$cc==3)
  img_b <- img_b[img_b!=0]
  feature_img <- dataset_type_bin

  feature_img <- c(feature_img,mean(img_r))
  feature_img <- c(feature_img,mean(img_g))
  feature_img <- c(feature_img,mean(img_b))

  feature_img <- c(feature_img,min(img_r))
  feature_img <- c(feature_img,min(img_g))
  feature_img <- c(feature_img,min(img_b))
  #
  # feature_img <- c(feature_img,(max(img_r) - min(img_r)))
  # feature_img <- c(feature_img,(max(img_g) - min(img_g)))
  # feature_img <- c(feature_img,(max(img_b) - min(img_b)))

  feature_img <- c(feature_img,length(img_df$value[img_df$value < quantile(img_df$value)[2]]))


  img_hsv <- RGBtoHSV(img)
  img_df_hsv <- as.data.frame(img_hsv)

  img_1_hsv <- subset(img_df_hsv$value,img_df_hsv$cc==1)
  img_1_hsv <- img_1_hsv[img_1_hsv!=0] #MELHORAR AS FUNCOES DE RECORTE
  img_2_hsv <- subset(img_df_hsv$value,img_df_hsv$cc==2)
  img_2_hsv <- img_2_hsv[img_2_hsv!=0]
  img_3_hsv <- subset(img_df_hsv$value,img_df_hsv$cc==3)
  img_3_hsv <- img_3_hsv[img_3_hsv!=0]

  # feature_img <- c(feature_img,mean(img_1_hsv))
  # feature_img <- c(feature_img,mean(img_2_hsv))
  # feature_img <- c(feature_img,mean(img_3_hsv))

  feature_img <- c(feature_img,min(img_1_hsv))
  feature_img <- c(feature_img,min(img_2_hsv))
  feature_img <- c(feature_img,min(img_3_hsv))

  return(feature_img)
}


process_images <- function(path_set, dataset_type_bin, limit = 20, start = 0) {
  img_list <- list.files(path_set)

  table_img <- NULL

  counter <- 1
  for (i in img_list) {
    if(counter > start) {
    img <- load.image(paste0(path_set,i))

    feature_img <- convert_image(img,dataset_type_bin)

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


# summary(results)
lista_res <- NULL
resultado_ext <- NULL

for(limite_treino in c(100))  {



training_1 <- process_images("cell_images/Parasitized/",1, limit = limite_treino)
training_0 <- process_images("cell_images/Uninfected/",0, limit = limite_treino)

total_training <- rbind(as.matrix(training_1),as.matrix(training_0))
total_training <- as.data.frame(total_training)


actual_1 <- process_images("cell_images/Parasitized/",1, limit = 500, start = (limite_treino + 1))
actual_0 <- process_images("cell_images/Uninfected/",0, limit = 500, start = (limite_treino + 1))
total_actual <- rbind(as.matrix(actual_1),as.matrix(actual_0))
total_actual <- as.data.frame(total_actual)

possb_feat <- 2:ncol(total_actual)
nbin <- 1


results <- result_matrix(total_training,total_actual,possb_feat,nbin,weights = "dynamic", method = "only_1", rule_set = "partial")


  lista_res <- rbind(lista_res,
  c(limite_treino,
  confusionMatrix(factor(results$Eval1),factor(results$Benchmark))$overall["Accuracy"]
  ))
  resultado_ext <- confusionMatrix(factor(results$Eval1),factor(results$Benchmark))

                                                  }
