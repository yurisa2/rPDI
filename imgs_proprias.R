library(imager)

# Segmentar bolas sinuca
pool_table <- load.image("img/sinuca.JPG")
# str(pool_table)
pool_table_R <- channels(pool_table,1)
pool_table_G <- channels(pool_table,2)
pool_table_B <- channels(pool_table,3)
#
# par(mfrow=c(2,2))
# plot(pool_table)
# plot(pool_table_R)
# plot(pool_table_G)
# plot(pool_table_B)

r_t <- threshold(as.cimg(as.data.frame(pool_table_R)),"90%")
g_t <- threshold(as.cimg(as.data.frame(pool_table_G)),"99%")
b_t <- threshold(as.cimg(as.data.frame(pool_table_B)))


res_bin <- matrix(nrow=1005)
res_bin <- ifelse(r_t[,,,1] == 0 & g_t[,,,1] == 0 & b_t[,,,1] == 0,0,1)

res_color <- pool_table
res_color[,,,1] <- pool_table[,,,1] * res_bin
res_color[,,,2] <- pool_table[,,,2] * res_bin
res_color[,,,3] <- pool_table[,,,3] * res_bin
plot(res_color)
