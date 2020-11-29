library(dplyr)
library(cluster)
library(MASS)


###Cargar funciones auxiliares
source("AUX.R")

datos <- read.csv("ubicaciones.csv",
                  header = TRUE)



#plot(datos$lat, datos$lon)


####Clustering
set.seed(95)
####CLUSTER POR DISTANCIA
df_long <- datos[,5:6]
###Normalizar las coordenadas
df_long_normalized <-  scale(df_long ,center = TRUE, scale = TRUE)

###Clustering
k2 <- kmeans(df_long_normalized, centers = 6, nstart = 100,
             algorithm = "MacQueen")
#str(k2)

k2_centers <- as.data.frame(k2$centers)

###Visualizacion
#fviz_cluster(k2, data = df_long_normalized)

####asignar clusters a datos

datos_clust <- datos
datos_clust[,5:6] <- df_long_normalized
datos_clust$Vol_Total <- datos_clust$Frecuencia * datos_clust$Vol_Entrega
datos_clust$cluster <- k2$cluster


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

clust1 <- filter(datos_clust, cluster == 1)
clust2 <- filter(datos_clust, cluster == 2)


vol_clust1 <- sum(clust1$Vol_Entrega)
vol_clust2 <- sum(clust2$Vol_Entrega)


#####CLUSTER POR VOLUMEN TOTAL
df_vol <- datos_clust$Vol_Total
df_vol <-  scale(df_vol ,center = TRUE, scale = TRUE)

k2_dist <- kmeans(df_vol, centers = 6, nstart = 25)
str(k2_dist)

datos_clust$cluster_vol_total <- k2_dist$cluster

#fviz_cluster(k2_dist, data = df_long_normalized) #Visualizamos con 2 clusters


df_cluster_original <- k2_centers

df_cluster_original$cluster <- rownames(df_cluster_original)



###Fin de script_algo




############# DONDE SE HACE LA MAGIA ##################################
## CLUSTER 1
cluster1_x <- df_cluster_original[1,1]
cluster1_y <- df_cluster_original[1,2]
##  EL MAS OPTIMO ES 599
m = 599
distancias_cluster1 <- c()
for (i in 1:nrow(datos_clust)) {
  distancias_cluster1 <- c(distancias_cluster1, distancia_test(datos_clust[i, 5], datos_clust[i, 6],
                                                               cluster1_x, cluster1_y))
}
length(distancias_cluster1)

datos_prueba <- cbind(datos_clust, distancias_cluster1)

#Normalicemos el volumen de entrega
df_vol <- datos_clust[,4]
df_vol_normalized <- scale(df_vol, center = FALSE, scale=TRUE)
datos_prueba$Vol_Entrega <- df_vol_normalized

datos_prueba <- datos_prueba[order(datos_prueba$distancias_cluster),]
colnames(datos_prueba)[10] <- "distancias_cluster"
zona1_prueba <- head(datos_prueba, m)
zona1_prueba$Frecuencia <- 1
zona1_prueba$cluster <- 1
datos_prueba1 <- datos_prueba
for (i in 1:m) {
  datos_prueba1[i, 3] <- datos_prueba1[i, 3] - 1 
  datos_prueba[i, 8] <- 1
}

datos_prueba1 <- datos_prueba1[which(datos_prueba1$Frecuencia > 0),]


cluster2_x <- df_cluster_original[2,1]
cluster2_y <- df_cluster_original[2,2]

distancias_cluster2 <- c()
for (i in 1:nrow(datos_prueba1)) {
  distancias_cluster2 <- c(distancias_cluster2,
                           distancia_test(datos_prueba1[i, 5],
                                          datos_prueba1[i, 6],
                                          cluster2_x, cluster2_y))
}

datos_prueba1$distancias_cluster <- distancias_cluster2
colnames(datos_prueba1)[10] <- "distancias_cluster"
datos_prueba1 <- datos_prueba1[order(datos_prueba1$distancias_cluster),]

zona2_prueba <- head(datos_prueba1, m)
zona2_prueba$cluster <- 2
datos_prueba2 <- datos_prueba1
zona2_prueba$Frecuencia <- 1
for (i in 1:m) {
  datos_prueba2[i, 3] <- datos_prueba2[i, 3] - 1 
  datos_prueba1[i, 3] <- datos_prueba1[i, 3] - 1 
  datos_prueba[i, 8] <- 2
}

datos_prueba2 <- datos_prueba2[which(datos_prueba2$Frecuencia > 0),]



cluster3_x <- df_cluster_original[3,1]
cluster3_y <- df_cluster_original[3,2]

distancias_cluster3 <- c()
for (i in 1:nrow(datos_prueba2)) {
  distancias_cluster3 <- c(distancias_cluster3,
                           distancia_test(datos_prueba2[i, 5],
                                          datos_prueba2[i, 6],
                                          cluster3_x, cluster3_y))
}

datos_prueba2$distancias_cluster <- distancias_cluster3
colnames(datos_prueba2)[10] <- "distancias_cluster"
datos_prueba2 <- datos_prueba2[order(datos_prueba2$distancias_cluster),]

zona3_prueba <- head(datos_prueba2, m)
zona3_prueba$cluster <- 3
datos_prueba3 <- datos_prueba2
zona3_prueba$Frecuencia <- 1
for (i in 1:m) {
  datos_prueba3[i, 3] <- datos_prueba3[i, 3] - 1 
  datos_prueba1[i, 3] <- datos_prueba1[i, 3] - 1 
  datos_prueba[i, 8] <- 3
}

datos_prueba3 <- datos_prueba3[which(datos_prueba3$Frecuencia > 0),]



cluster4_x <- df_cluster_original[4,1]
cluster4_y <- df_cluster_original[4,2]

distancias_cluster4 <- c()
for (i in 1:nrow(datos_prueba3)) {
  distancias_cluster4 <- c(distancias_cluster4,
                           distancia_test(datos_prueba3[i, 5],
                                          datos_prueba3[i, 6],
                                          cluster4_x, cluster4_y))
}

datos_prueba3$distancias_cluster <- distancias_cluster4
colnames(datos_prueba3)[10] <- "distancias_cluster"
datos_prueba3 <- datos_prueba3[order(datos_prueba3$distancias_cluster),]

zona4_prueba <- head(datos_prueba3, m)
zona4_prueba$cluster <- 4
datos_prueba4 <- datos_prueba3
zona4_prueba$Frecuencia <- 1
for (i in 1:m) {
  datos_prueba4[i, 3] <- datos_prueba4[i, 3] - 1 
  datos_prueba1[i, 3] <- datos_prueba1[i, 3] - 1 
  datos_prueba[i, 8] <- 4
}

datos_prueba4 <- datos_prueba4[which(datos_prueba4$Frecuencia > 0),]



cluster5_x <- df_cluster_original[5,1]
cluster5_y <- df_cluster_original[5,2]

distancias_cluster5 <- c()
for (i in 1:nrow(datos_prueba4)) {
  distancias_cluster5 <- c(distancias_cluster5,
                           distancia_test(datos_prueba4[i, 5],
                                          datos_prueba4[i, 6],
                                          cluster5_x, cluster5_y))
}

datos_prueba4$distancias_cluster <- distancias_cluster5
colnames(datos_prueba4)[10] <- "distancias_cluster"
datos_prueba4 <- datos_prueba4[order(datos_prueba4$distancias_cluster),]

zona5_prueba <- head(datos_prueba4, m)
zona5_prueba$cluster <- 5
datos_prueba5 <- datos_prueba4
zona5_prueba$Frecuencia <- 1
for (i in 1:m) {
  datos_prueba5[i, 3] <- datos_prueba5[i, 3] - 1 
  datos_prueba1[i, 3] <- datos_prueba1[i, 3] - 1 
  datos_prueba[i, 8] <- 5
}

datos_prueba5 <- datos_prueba5[which(datos_prueba5$Frecuencia > 0),]



cluster6_x <- df_cluster_original[6,1]
cluster6_y <- df_cluster_original[6,2]

distancias_cluster6 <- c()
for (i in 1:nrow(datos_prueba5)) {
  distancias_cluster6 <- c(distancias_cluster6,
                           distancia_test(datos_prueba5[i, 5],
                                          datos_prueba5[i, 6],
                                          cluster6_x, cluster6_y))
}

datos_prueba5$distancias_cluster <- distancias_cluster6
colnames(datos_prueba5)[10] <- "distancias_cluster"
datos_prueba5 <- datos_prueba5[order(datos_prueba5$distancias_cluster),]

zona6_prueba <- head(datos_prueba5, m)
zona6_prueba$cluster <- 6
datos_prueba6 <- datos_prueba5


for (i in 1:min(m, nrow(datos_prueba6))) {
  datos_prueba6[i, 3] <- datos_prueba6[i, 3] - 1 
  datos_prueba1[i, 3] <- datos_prueba1[i, 3] - 1 
  datos_prueba[i, 8] <- 6
}

datos_prueba6 <- datos_prueba6[which(datos_prueba6$Frecuencia > 0),]



#rm(df_vol_total)







zona_prueba_final <- rbind(zona1_prueba,zona2_prueba,zona3_prueba,
                           zona4_prueba,zona5_prueba,zona6_prueba)

cont <- 1
cont1 <- 1
valor <- 0
print(nrow(datos_prueba6))
while (1 <= nrow(datos_prueba6)) {
  if(cont%%100 == 0){print(cont)}
  ban <- FALSE
  #print(cont1)
  cont1 = cont1 + 1
  quitar <- c(datos_prueba6[1, 8])
  aa<-which(zona1_prueba[,1] == datos_prueba6)
  #print(which(zona1_prueba[,1] == datos_prueba6))
  b <- FALSE
  #if(datos_prueba6[1,1] == 1247){print(datos_prueba6[1,]); b <- TRUE}
  if (sum(zona1_prueba[,1] == datos_prueba6[1,1]) != 0){quitar <- c(quitar, 1)}
  if (sum(zona2_prueba[,1] == datos_prueba6[1,1]) != 0){quitar <- c(quitar, 2)}
  if (sum(zona3_prueba[,1] == datos_prueba6[1,1]) != 0){quitar <- c(quitar, 3)}
  if (sum(zona4_prueba[,1] == datos_prueba6[1,1]) != 0){quitar <- c(quitar, 4)}
  if (sum(zona5_prueba[,1] == datos_prueba6[1,1]) != 0){quitar <- c(quitar, 5)}
  if (sum(zona6_prueba[,1] == datos_prueba6[1,1]) != 0){quitar <- c(quitar, 6)}
  if(b){print(quitar)}
  cont = cont + 1
  valor <- pesos(datos_prueba6[1, ], quitar,b)
  if(b){print("EL VALOR ES");print(valor)}
  datos_prueba6[1, 8] <- valor
  if (valor == 1){
    zona1_prueba <- rbind(zona1_prueba, datos_prueba6[1,])
    ban <- TRUE
  }
  else if (valor == 2){
    zona2_prueba <- rbind(zona2_prueba, datos_prueba6[1,])
    ban <- TRUE
  }
  else if (valor == 3){
    zona3_prueba <- rbind(zona3_prueba, datos_prueba6[1,])
    ban <- TRUE
  }
  else if (valor == 4){
    zona4_prueba <- rbind(zona4_prueba, datos_prueba6[1,])
    ban <- TRUE
  }
  else if (valor == 5){
    zona5_prueba <- rbind(zona5_prueba, datos_prueba6[1,])
    ban <- TRUE
  }
  else if (valor == 6){
    zona6_prueba <- rbind(zona6_prueba, datos_prueba6[1,])
    ban <- TRUE
  }
  
  if(!ban){print(datos_prueba6[1,]);print(valor);print("DEBUG")}
  #contador = contador + 1
  datos_prueba6[1, 3] <- datos_prueba6[1, 3] - 1 
  
  
  datos_prueba6 <- datos_prueba6[which(datos_prueba6$Frecuencia > 0),]
  
}
zona_prueba_final <- rbind(zona1_prueba,zona2_prueba,zona3_prueba,
                           zona4_prueba,zona5_prueba,zona6_prueba)

print("VALORES FINALES")
final <- calcula_vol_total(zona_prueba_final, TRUE)
#funcion_prepara_output()


####Obtener el promedio recomendado para las 6 zonas
prom_recom <- sum(final$vol_total) / 6
###Obtener cluster que tiene el mayor volumen

clust_mayor_vol <- rownames(final[final$vol_total == max(final$vol_total),])

######Obtener los puntos mas lejanos del cluster mayor
df_clust_mayor <- filter(zona_prueba_final, 
                         cluster == as.integer(clust_mayor_vol))
df_clust_mayor <- df_clust_mayor[with(df_clust_mayor, 
                                      order(-distancias_cluster,
                                            -Vol_Total)), ]

####Obtener los 20 mas lejanos y que tengan frecuencia de 1

df_clust_lejanos <- df_clust_mayor[1:404,]
df_clust_lejanos <- filter(df_clust_lejanos)#, Frecuencia == 1)

####Calcular el cluster mas cercano, con excepcion del original

df_cluster_lejano <- filter(df_cluster_original, cluster != clust_mayor_vol)

for( i in 1:nrow(df_clust_lejanos)){
  a <- calcula_distancia(df_clust_lejanos[i,5], df_clust_lejanos[i,6],
                         df_cluster_lejano$lat, 
                         df_cluster_lejano$lon,
                         df_cluster_lejano$cluster)
  df_c <- df_clust_lejanos[i,]
  a <- a[3]
  df_c$cluster <- a
  #####Añadir al cluster_correspondiente
  if(a == "1"){
    zona1_prueba <- rbind(zona1_prueba, df_c)
    
  } else if (a == "2"){
    zona2_prueba <- rbind(zona2_prueba, df_c)
  } else if (a == "3"){
    zona3_prueba <- rbind(zona3_prueba, df_c)
  } else if (a == "4"){
    zona4_prueba <- rbind(zona4_prueba, df_c)
  } else if (a == "5"){
    zona5_prueba <- rbind(zona5_prueba, df_c)
  }else if (a == "6"){
    zona6_prueba <- rbind(zona6_prueba, df_c)
  }
  
  ### eliminar el elemento de su cluster original
  if(clust_mayor_vol == "1"){
    zona1_prueba <- zona1_prueba[-which(zona1_prueba$Id_Cliente == df_c$Id_Cliente),]
    
  } else if (clust_mayor_vol == "2"){
    zona2_prueba <- zona2_prueba[-which(zona2_prueba$Id_Cliente == df_c$Id_Cliente),]
  } else if (clust_mayor_vol == "3"){
    zona3_prueba <- zona3_prueba[-which(zona3_prueba$Id_Cliente == df_c$Id_Cliente),]
  } else if (clust_mayor_vol == "4"){
    zona4_prueba <- zona4_prueba[-which(zona4_prueba$Id_Cliente == df_c$Id_Cliente),]
  } else if (clust_mayor_vol == "5"){
    zona5_prueba <- zona5_prueba[-which(zona5_prueba$Id_Cliente == df_c$Id_Cliente),]
  }else if (clust_mayor_vol == "6"){
    zona6_prueba <- zona6_prueba[-which(zona6_prueba$Id_Cliente == df_c$Id_Cliente),]
  }
  
}


zona_prueba_final_final <- rbind(zona1_prueba,zona2_prueba,zona3_prueba,
                                 zona4_prueba,zona5_prueba,zona6_prueba)

print("VALORES FINALES REBALANCEADOS")
final_final <- calcula_vol_total(zona_prueba_final_final, TRUE)


###Guardamos el output
funcion_prepara_output()
