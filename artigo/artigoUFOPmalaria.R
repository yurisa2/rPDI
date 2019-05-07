library(imager)
setwd("/home/yurisa2/rPDI/artigo")

source(file="afis_fuzzysis/include/functions.R")

doente <- load.image("test/Parasitized/C39P4thinF_original_IMG_20150622_105335_cell_9.png")
png(filename = "doente.png")
par(mfrow=c(2,2))

plot(doente)
hist(doente[,,,1], main = "R", xlab = "Célula Doente")
hist(doente[,,,2], main = "G", xlab = "Célula Doente")
hist(doente[,,,3], main = "B", xlab = "Célula Doente")
dev.off()


saudavel <- load.image("test/Uninfected/C2NThinF_IMG_20150604_114631_cell_141.png")
png(filename = "saudavel.png")
par(mfrow=c(2,2))

plot(saudavel)
hist(saudavel[,,,1], main = "R", xlab = "Célula Saudável")
hist(saudavel[,,,2], main = "G", xlab = "Célula Saudável")
hist(saudavel[,,,3], main = "B", xlab = "Célula Saudável")
dev.off()

img_df <- as.data.frame(doente)

img_df <- img_df[img_df$value!=0,] #MELHORAR AS FUNCOES DE RECORTE

doente_f <- convert_image(doente)

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


  # FAZER AQUI UMA SERIE COM CALCULOS NORMALIZADOS
  # FAZER AQUI UMA SERIE COM CALCULOS GRAYSCALE

  # img_g_df <- as.data.frame(grayscale(img))
  # img_g_df <- img_g_df[img_g_df!=0]
  #
  # feature_img <- c(feature_img,mean(img_g_df))
  # feature_img <- c(feature_img,min(img_g_df))


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

training_1 <- process_images("cell_images/Parasitized/",1, limit = 100)
training_0 <- process_images("cell_images/Uninfected/",0, limit = 100)

total_training <- rbind(as.matrix(training_1),as.matrix(training_0))
total_training <- as.data.frame(total_training)


actual_1 <- process_images("cell_images/Parasitized/",1, limit = 999999, start = 101)
actual_0 <- process_images("cell_images/Uninfected/",0, limit = 999999, start = 101)
total_actual <- rbind(as.matrix(actual_1),as.matrix(actual_0))
total_actual <- as.data.frame(total_actual)

possb_feat <- 2:ncol(total_actual)
nbin <- 1

# results <- result_matrix(total_training,total_actual,possb_feat,nbin,weights = "dynamic", method = "only_1", rule_set = "partial")
# 
# confusionMatrix(factor(results$Eval1),factor(results$Benchmark))
# 

# summary(results)
lista_res <- NULL
for(pesos in c("fixed", "dynamic")) for (metodos in c("only_1",
                                                    "conservative",
                                                    "conservative2",
                                                    "sc",
                                                    "sc2",
                                                    "fuzzy50",
                                                    "fuzzy55",
                                                    "fuzzy60",
                                                    "fuzzy65",
                                                    "fuzzy70"
                                                    )
                                                  ) {
results <- result_matrix(total_training,total_actual,possb_feat,nbin,weights = pesos, method = metodos)


  Q <- rbind(lista_res,
  c(pesos, metodos,
  confusionMatrix(factor(results$Eval1),factor(results$Benchmark))$overall["Accuracy"]
  ))

      
                                                }
