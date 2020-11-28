
 for( i in 1:6){
     df_filt <- filter(datos_clust, cluster == i)
     vol_total <- sum(df_filt$Vol_Entrega)
     n_elementos <- nrow(df_filt)
     promedio <- mean(df_filt$Vol_Entrega)
     
       ms <- paste0("Vol_total= ", vol_total,
                                     " n_elementos= ", n_elementos,
                                     " Promedio= ", promedio)
       
         message(ms)
       print(quantile(df_filt$Vol_Total))
 }
 
 
zona1_final <-  filter(datos_clust, cluster == 1)
zona2_final <-  filter(datos_clust, cluster == 2)
zona3_final <-  filter(datos_clust, cluster == 3)
zona4_final <-  filter(datos_clust, cluster == 4)
zona5_final <-  filter(datos_clust, cluster == 5)
zona6_final <-  filter(datos_clust, cluster == 6)


####Pegarles los 2 y 3
filt1 <- filter(clientes_2_y_3_frecuencia, cluster_predicted == 1)
zona1 <- rbind(zona1_final, filt1[,1:9])

filt2 <- filter(clientes_2_y_3_frecuencia, cluster_predicted == 2)
zona2 <- rbind(zona2_final, filt2[,1:9])

filt3 <- filter(clientes_2_y_3_frecuencia, cluster_predicted == 3)
zona3 <- rbind(zona3_final, filt3[,1:9])

filt4 <- filter(clientes_2_y_3_frecuencia, cluster_predicted == 4)
zona4 <- rbind(zona4_final, filt4[,1:9])

filt5 <- filter(clientes_2_y_3_frecuencia, cluster_predicted == 5)
zona5 <- rbind(zona5_final, filt5[,1:9])

filt6 <- filter(clientes_2_y_3_frecuencia, cluster_predicted == 6)
zona6 <- rbind(zona6_final, filt6[,1:9])

sum(nrow(zona1),nrow(zona2),nrow(zona3),nrow(zona4),nrow(zona5),nrow(zona6))
