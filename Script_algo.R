library(dplyr)
library(cluster)
library(factoextra)
library(MASS)

datos <- read.csv("ubicaciones.csv",
                  header = TRUE, row.names = 1)

##TO-DO: REMOVER LUGARES CON VOLUMEN DE ENTREGA 0

nrow(datos[datos$Vol_Entrega == 0,])



plot(datos$lat, datos$lon)


####Clustering

####CLUSTER POR DISTANCIA
df_long <- datos[,4:5]
###Normalizar las coordenadas
df_long_normalized <-  scale(df_long ,center = TRUE, scale = TRUE)

distance <- get_dist(df_long_normalized)

k2 <- kmeans(df_long_normalized, centers = 6, nstart = 25) #Agrupamos k-medias con 25 configuraciones
str(k2)

k2_centers <- as.data.frame(k2$centers)

fviz_cluster(k2, data = df_long_normalized) #Visualizamos con 2 clusters

####asignar clusters a datos

datos_clust <- datos
datos_clust[,4:5] <- df_long_normalized
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

k2_dist <- kmeans(df_vol, centers = 2, nstart = 25) #Agrupamos k-medias con 25 configuraciones
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
      df_dias_filt$cluster_predicted <- df_dias_filt$cluster#"Original"
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
i <- 1

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


a <- calcula_dias(df_frecuencia, df_cluster_original)





