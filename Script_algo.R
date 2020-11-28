library(dplyr)
library(cluster)
#library(factoextra)
library(MASS)

source("AUX.R")

datos <- read.csv("ubicaciones.csv",
                  header = TRUE)

##TO-DO: REMOVER LUGARES CON VOLUMEN DE ENTREGA 0

nrow(datos[datos$Vol_Entrega == 0,])



plot(datos$lat, datos$lon)


####Clustering
set.seed(95)
####CLUSTER POR DISTANCIA
df_long <- datos[,5:6]
###Normalizar las coordenadas
df_long_normalized <-  scale(df_long ,center = TRUE, scale = TRUE)

#distance <- get_dist(df_long_normalized)

k2 <- kmeans(df_long_normalized, centers = 6, nstart = 25) #Agrupamos k-medias con 25 configuraciones
str(k2)

k2_centers <- as.data.frame(k2$centers)

fviz_cluster(k2, data = df_long_normalized) #Visualizamos con 2 clusters

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

fviz_cluster(k2_dist, data = df_long_normalized) #Visualizamos con 2 clusters



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
    distancia <- sqrt(((xi2 - x1)^2) + ((yi2 - y1)^2))
    
    if(!exists("mas_cercano")){
      mas_cercano <- c(xi2, yi2)
      mas_cercano_dist <- distancia[1]
      p_cercano <- string_nombres[i]
    } else {
      if(distancia < mas_cercano_dist){
        mas_cercano <- c(xi2, yi2)
        mas_cercano_dist <- distancia[1]
        p_cercano <- string_nombres[i]
      }
    }
    
  }
  
  return(paste0(c(mas_cercano, p_cercano)))
  
}

#calcula_distancia(0.8308095811, 1.113070869, k2_centers$lat, k2_centers$lon)

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
    print(i)
    
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


#####Actualizar volumen total
zona1 <- actualiza_volumen(zona1)
zona2 <- actualiza_volumen(zona2)
zona2 <- actualiza_volumen(zona3)
zona3 <- actualiza_volumen(zona4)
zona4 <- actualiza_volumen(zona5)
zona5 <- actualiza_volumen(zona6)

#####
#zona_final <- rbind(zona1,zona2,zona3,zona4,zona5,zona6)


