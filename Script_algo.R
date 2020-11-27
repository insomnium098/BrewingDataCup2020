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

fviz_cluster(k2, data = df_long_normalized) #Visualizamos con 2 clusters

####asignar clusters a datos

datos_clust <- datos
datos_clust[,4:5] <- df_long_normalized
datos_clust$Vol_Total <- datos_clust$Frecuencia * datos_clust$Vol_Entrega
datos_clust$cluster <- k2$cluster

clust1 <- filter(datos_clust, cluster == 1)
clust2 <- filter(datos_clust, cluster == 2)

vol_clust1 <- sum(clust1$Vol_Entrega)
vol_clust2 <- sum(clust2$Vol_Entrega)


#####CLUSTER POR VOLUMEN TOTAL
df_vol <- datos_clust$Vol_Total
df_vol <-  scale(df_vol ,center = TRUE, scale = TRUE)

k2_dist <- kmeans(df_vol, centers = 2, nstart = 25) #Agrupamos k-medias con 25 configuraciones
str(k2_dist)

fviz_cluster(k2_dist, data = df_vol) #Visualizamos con 2 clusters



