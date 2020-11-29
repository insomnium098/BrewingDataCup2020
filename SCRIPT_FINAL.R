library(dplyr)
library(cluster)
#library(factoextra)
library(MASS)

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
#fviz_cluster(k2, data = df_long_normalized) #Visualizamos con 2 clusters

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

k2_dist <- kmeans(df_vol, centers = 6, nstart = 25) #Agrupamos k-medias con 25 configuraciones
str(k2_dist)

datos_clust$cluster_vol_total <- k2_dist$cluster

#fviz_cluster(k2_dist, data = df_long_normalized) #Visualizamos con 2 clusters



############OBTENER AQUELLOS CON 2 Y 3 FRECUENCIAS DE ENTREGA

df_2 <- filter(datos_clust, Frecuencia == 2)
df_3 <- filter(datos_clust, Frecuencia == 3)
df_2_y_3 <- rbind(df_2, df_3)


####Funcion calcula_distancia
##Esta funcion recibe 4 vectores, 2 de lat y long y calcula los
##puntos que tienen una menor distancia entre ellos
##Devuelve las coordenadas del mas cercano y el numero del centro
#x1 <- 0.8308095811
#y1 <- 1.113070869
#x2 <- k2_centers$lat
#y2 <- k2_centers$lon
#i <- 1

calcula_distancia <- function(x1,y1,x2,y2,string_nombres){
  #c2 y y2 son los centros
  veces_loop <- (length(x2))
  for (i in 1:veces_loop){
    xi2 <- x2[i]
    yi2 <- y2[i]
    distancia_x <- sqrt(((xi2 - x1)^2) + ((yi2 - y1)^2))
    
    if(!exists("mas_cercano")){
      mas_cercano <- c(xi2, yi2)
      mas_cercano_dist <- distancia_x[1]
      p_cercano <- string_nombres[i]
    } else {
      if(distancia_x < mas_cercano_dist){
        mas_cercano <- c(xi2, yi2)
        mas_cercano_dist <- distancia_x[1]
        p_cercano <- string_nombres[i]
      }
    }
    
  }
  
  return(paste0(c(mas_cercano, p_cercano)))
  
}

#a <- calcula_distancia(0.8308095811, 1.113070869,
#                       df_cluster_original$lat, 
#                       df_cluster_original$lon,
#                       df_cluster_original$cluster)

####FUNCION CALCULA_DIAS
##Esta funcion recibe un df con los puntos que tienen una frecuencia de
##2 y 3 con su cluster original y busca la distancia minima a otros clusters
##para definir que otro dia debe de ser visitado el punto

calcula_dias_aux <- function(df_dias_filt, cluster_dias){
  frec <- df_dias_filt$Frecuencia - 1
  for (j in 1:frec){
    #print(cluster_dias)
    dias <- calcula_distancia(df_dias_filt$lat[1],
                              df_dias_filt$lon[1],
                              cluster_dias$lat,
                              cluster_dias$lon,
                              cluster_dias$cluster)
    dias <- dias[3]
    dias <- as.integer(dias)
    
    if(!exists("dias_final")){
      dias_final <- dias
      cluster_dias <- cluster_dias[!cluster_dias$cluster == dias_final,]
      df_dias_filt$cluster_predicted <- "Original"#df_dias_filt$cluster#"Original"
      df_dias_filt <- rbind(df_dias_filt, df_dias_filt)
      df_dias_filt$cluster_predicted[j+1] <- dias_final
      
    } else {
      dias_final <- dias
      cluster_dias <- cluster_dias[!cluster_dias$cluster == dias_final,]
      df_dias_filt <- rbind(df_dias_filt, df_dias_filt[1,])
      df_dias_filt$cluster_predicted[j+1] <- dias_final
      
    }
    
  }
  
  rm(dias_final)
  
  return(df_dias_filt)
  
}

df_frecuencia <- df_2_y_3
df_cluster_original <- k2_centers
#i <- 1

df_cluster_original$cluster <- rownames(df_cluster_original)
calcula_dias <- function(df_frecuencia, df_cluster_original){
  
  
  for(i in 1:nrow(df_frecuencia)){
    #print(i)
    
    df_dias_filt <- df_frecuencia[i,]
    ###Remover el cluster original antes de llamar a calcula dias
    cluster_dias <- df_cluster_original[!df_cluster_original$cluster == df_dias_filt$cluster,]
    ###Llamar a cluster dias las veces necesarias y en cada iteracion
    ## remover el cluster que ya salio
    frec <- df_dias_filt$Frecuencia - 1
    calcula_dias_temp <- calcula_dias_aux(df_dias_filt, cluster_dias)
    
    if(!exists("calcula_dias_final")){
      calcula_dias_final <- calcula_dias_temp
    } else {
      calcula_dias_final <- rbind(calcula_dias_final, 
                                  calcula_dias_temp)
    }
    
    
  }
  
  return(calcula_dias_final)
  
}

####Aqui calculamos los clusters mas cercanos a aquellos
#### puntos que tienen >1 frecuencia
clientes_2_y_3_frecuencia <- calcula_dias(df_frecuencia, df_cluster_original)






##########JUNTAMOS LAS ZONAS PREDICHAS CON LAS FIJAS
zona1_final <-  filter(datos_clust, cluster == 1)
zona2_final <-  filter(datos_clust, cluster == 2)
zona3_final <-  filter(datos_clust, cluster == 3)
zona4_final <-  filter(datos_clust, cluster == 4)
zona5_final <-  filter(datos_clust, cluster == 5)
zona6_final <-  filter(datos_clust, cluster == 6)


####Pegarles los 2 y 3
filt1 <- filter(clientes_2_y_3_frecuencia, cluster_predicted == 1)
filt1$cluster <- filt1$cluster_predicted

zona1 <- rbind(zona1_final, filt1[,1:9])

filt2 <- filter(clientes_2_y_3_frecuencia, cluster_predicted == 2)
filt2$cluster <- filt2$cluster_predicted

zona2 <- rbind(zona2_final, filt2[,1:9])

filt3 <- filter(clientes_2_y_3_frecuencia, cluster_predicted == 3)
filt3$cluster <- filt3$cluster_predicted

zona3 <- rbind(zona3_final, filt3[,1:9])


filt4 <- filter(clientes_2_y_3_frecuencia, cluster_predicted == 4)
filt4$cluster <- filt4$cluster_predicted

zona4 <- rbind(zona4_final, filt4[,1:9])

filt5 <- filter(clientes_2_y_3_frecuencia, cluster_predicted == 5)
filt5$cluster <- filt5$cluster_predicted

zona5 <- rbind(zona5_final, filt5[,1:9])

filt6 <- filter(clientes_2_y_3_frecuencia, cluster_predicted == 6)
filt6$cluster <- filt6$cluster_predicted

zona6 <- rbind(zona6_final, filt6[,1:9])

#####
zona_final <- rbind(zona1,zona2,zona3,zona4,zona5,zona6)

##Remover vol_total
zona_final <- zona_final[,!names(zona_final) %in% ("Vol_Total")]

###Fin de script_algo

distancia_test <- function(x1,y1,x2,y2){
  distancia <- sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
  return(distancia)
}

###Funcion para mandar a csv


funcion_prepara_output <- function(){
  message("Preparando Output")
  output <- data.frame(Id_Cliente = integer(), D1=integer(), D2=integer()
                       , D3= integer(), D4=integer(),D5=integer(),D6=integer())
  clientes <- datos[,1]
  for (i in clientes) {
    output[i,1] <- i
    output[i,2] <- sum(zona1_prueba[,1] == i)
    output[i,3] <- sum(zona2_prueba[,1] == i)
    output[i,4] <- sum(zona3_prueba[,1] == i)
    output[i,5] <- sum(zona4_prueba[,1] == i)
    output[i,6] <- sum(zona5_prueba[,1] == i)
    output[i,7] <- sum(zona6_prueba[,1] == i)
  }
  
  which(output[,2] >= 2)
  which(output[,3] >= 2)
  which(output[,4] >= 2)
  which(output[,5] >= 2)
  which(output[,6] >= 2)
  which(output[,7] >= 2)
  write.csv(output,"output.csv", row.names = FALSE)
}

############# DONDE SE HACE LA MAGIA ##################################
## CLUSTER 1
cluster1_x <- df_cluster_original[1,1]
cluster1_y <- df_cluster_original[1,2]
##  EL MAMALON ES 590
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




calcula_vol_total <- function(zona_prueba_final, bandera){
  for( i in 1:6){
    df_filt <- filter(zona_prueba_final, cluster == i)
    vol_total <- sum(df_filt$Vol_Entrega)
    n_elementos <- nrow(df_filt)
    promedio <- mean(df_filt$Vol_Entrega)
    
    ms <- paste0("Vol_total= ", vol_total,
                 " n_elementos= ", n_elementos,
                 " Promedio= ", promedio)
    
    if(bandera){
      message(ms)
      print(quantile(df_filt$Vol_Entrega))
    }
    t <- as.data.frame(vol_total)
    if(!exists("df_vol_total")){
      df_vol_total <- t
    } else {
      df_vol_total <- rbind(df_vol_total, t)
    }
    
    if(!exists("n_final")){
      n_final <- n_elementos
    } else {
      n_final <- c(n_final, n_elementos)
    }
  }
  df_vol_total$n <- n_final
  return(df_vol_total)
  
}

pesos <- function(x, quitar, bb){
  test <- calcula_vol_total(zona_prueba_final, FALSE)
  peso <- 100000
  val = 0
  df <- df_cluster_original
  if(bb){print(test)}
  if(bb){print(x[1,]);print("LOS DATOS")}
  for (i in 1:nrow(df)) {
    if(bb) {print(!sum(i == quitar) >= 1)}
    if (!sum(i == quitar) >= 1){
      temp <- distancia_test(x[1,5], x[1,6], df[i,1],df[i, 2])
      if (x[1, 4] == 0){temp1 = 0}
      else{
        temp1 <- test[i, 1]# / x[1,4]
      }
      temp2 <- log(test[i,2]/3977)
      #message(paste0(temp,", ", temp1, ", ", temp2))
      temp_final = (.3/2) * (temp) +  .25* (temp1) + (.1/2)*(temp2)
      if(bb){print("TEMP FINAL"); print(temp)}
      if (peso > temp_final){
        peso = temp_final
        val = i
      }
    }
    if(bb){print(val)}
  }
  return(val)
}
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
  
  if(!ban){print(datos_prueba6[1,]);print(valor);print("AAAAAAAAAA")}
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






funcion_prepara_output()