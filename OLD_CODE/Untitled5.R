
cluster1_x <- df_cluster_original[1,1]
cluster1_y <- df_cluster_original[1,2]
cluster2_x <- df_cluster_original[2,1]
cluster2_y <- df_cluster_original[2,2]
cluster3_x <- df_cluster_original[3,1]
cluster3_y <- df_cluster_original[3,2]
cluster4_x <- df_cluster_original[4,1]
cluster4_y <- df_cluster_original[4,2]
cluster5_x <- df_cluster_original[5,1]
cluster5_y <- df_cluster_original[5,2]
cluster6_x <- df_cluster_original[6,1]
cluster6_y <- df_cluster_original[6,2]


funcion_revisa_zona <- function(id,zona){
  
  if(id %in% zona$Id_Cliente){
    return(TRUE)
  } else {
    return(FALSE)
  }
  
  
}


###Aquì deberà ir el ciclo for
m = 100
datos_prueba <- datos_clust
rm(zona1_prueba)
rm(zona2_prueba)
rm(zona3_prueba)
rm(zona4_prueba)
rm(zona5_prueba)
rm(zona6_prueba)
for(j in 1:m){
  print(j)
  distancias_cluster1 <- c()
  for (i in 1:nrow(datos_prueba)) {
    distancias_cluster1 <- c(distancias_cluster1, distancia_test(datos_prueba[i, 5],
                                                                 datos_prueba[i, 6],
                                                                 cluster1_x, cluster1_y))
  }
  
  ###Añadimos un if para que la primera vez cree la columna distancias cluster
  if(j == 1){
    datos_prueba <- cbind(datos_prueba, distancias_cluster1)
  }
  
  colnames(datos_prueba)[10] <- "distancias_cluster"
  datos_prueba <- datos_prueba[order(datos_prueba$distancias_cluster),]
  
  #Añadimos el primer elemento a la zona correspondiente
  
  ########
  if(!exists("zona1_prueba")){
    zona1_prueba <- head(datos_prueba, 1)
  } else {
      if(funcion_revisa_zona(datos_prueba[1,1], zona1_prueba)){
        zona1_prueba <- rbind(zona1_prueba, head(datos_prueba,1))
      }
    
  }
  
  zona1_prueba$Frecuencia <- 1
  zona1_prueba$cluster <- 1
  
  #Borramos El elemento añadido
  datos_prueba[1, 3] <- datos_prueba[1, 3] - 1
  datos_prueba <- datos_prueba[which(datos_prueba$Frecuencia > 0),]
  #}
  
  distancias_cluster2 <- c()
  for (i in 1:nrow(datos_prueba)) {
    distancias_cluster2 <- c(distancias_cluster2, distancia_test(datos_prueba[i, 5],
                                                                 datos_prueba[i, 6],
                                                                 cluster2_x, cluster2_y))
  }
  
  datos_prueba$distancias_cluster <- distancias_cluster2
  datos_prueba <- datos_prueba[order(datos_prueba$distancias_cluster),]
  
  
  if(!exists("zona2_prueba")){
    zona2_prueba <- head(datos_prueba, 1)
  } else {
    if(funcion_revisa_zona(datos_prueba[1,1], zona2_prueba)){
      zona2_prueba <- rbind(zona2_prueba, head(datos_prueba,1))
    }
    
  }
  zona2_prueba$Frecuencia <- 1
  zona2_prueba$cluster <- 2
  
  datos_prueba[1, 3] <- datos_prueba[1, 3] - 1
  datos_prueba <- datos_prueba[which(datos_prueba$Frecuencia > 0),]
  #}
  
  distancias_cluster3 <- c()
  for (i in 1:nrow(datos_prueba)) {
    distancias_cluster3 <- c(distancias_cluster3, distancia_test(datos_prueba[i, 5],
                                                                 datos_prueba[i, 6],
                                                                 cluster3_x, cluster3_y))
  }
  
  datos_prueba$distancias_cluster <- distancias_cluster3
  datos_prueba <- datos_prueba[order(datos_prueba$distancias_cluster),]
  
  
  if(!exists("zona3_prueba")){
    zona3_prueba <- head(datos_prueba, 1)
  } else {
    if(funcion_revisa_zona(datos_prueba[1,1], zona3_prueba)){
      zona3_prueba <- rbind(zona3_prueba, head(datos_prueba,1))
    }
    
  }
  zona3_prueba$Frecuencia <- 1
  zona3_prueba$cluster <- 3
  
  datos_prueba[1, 3] <- datos_prueba[1, 3] - 1
  datos_prueba <- datos_prueba[which(datos_prueba$Frecuencia > 0),]
  #}
  
  
  distancias_cluster4 <- c()
  for (i in 1:nrow(datos_prueba)) {
    distancias_cluster4 <- c(distancias_cluster4, distancia_test(datos_prueba[i, 5],
                                                                 datos_prueba[i, 6],
                                                                 cluster4_x, cluster4_y))
  }
  
  datos_prueba$distancias_cluster <- distancias_cluster4
  datos_prueba <- datos_prueba[order(datos_prueba$distancias_cluster),]
  
  
  if(!exists("zona4_prueba")){
    zona4_prueba <- head(datos_prueba, 1)
  } else {
    if(funcion_revisa_zona(datos_prueba[1,1], zona4_prueba)){
      zona4_prueba <- rbind(zona4_prueba, head(datos_prueba,1))
    }
    
  }
  zona4_prueba$Frecuencia <- 1
  zona4_prueba$cluster <- 4
  
  datos_prueba[1, 3] <- datos_prueba[1, 3] - 1
  datos_prueba <- datos_prueba[which(datos_prueba$Frecuencia > 0),]
  #}
  
  distancias_cluster5 <- c()
  for (i in 1:nrow(datos_prueba)) {
    distancias_cluster5 <- c(distancias_cluster5, distancia_test(datos_prueba[i, 5],
                                                                 datos_prueba[i, 6],
                                                                 cluster5_x, cluster5_y))
  }
  
  datos_prueba$distancias_cluster <- distancias_cluster5
  datos_prueba <- datos_prueba[order(datos_prueba$distancias_cluster),]
  
  
  if(!exists("zona5_prueba")){
    zona5_prueba <- head(datos_prueba, 1)
  } else {
    if(funcion_revisa_zona(datos_prueba[1,1], zona5_prueba)){
      zona5_prueba <- rbind(zona5_prueba, head(datos_prueba,1))
    }
    
  }
  zona5_prueba$Frecuencia <- 1
  zona5_prueba$cluster <- 5
  
  datos_prueba[1, 3] <- datos_prueba[1, 3] - 1
  datos_prueba <- datos_prueba[which(datos_prueba$Frecuencia > 0),]
  #}
  
  distancias_cluster6 <- c()
  for (i in 1:nrow(datos_prueba)) {
    distancias_cluster6 <- c(distancias_cluster6, distancia_test(datos_prueba[i, 5],
                                                                 datos_prueba[i, 6],
                                                                 cluster6_x, cluster6_y))
  }
  
  datos_prueba$distancias_cluster <- distancias_cluster6
  datos_prueba <- datos_prueba[order(datos_prueba$distancias_cluster),]
  
  
  if(!exists("zona6_prueba")){
    zona6_prueba <- head(datos_prueba, 1)
  } else {
    if(funcion_revisa_zona(datos_prueba[1,1], zona6_prueba)){
      zona6_prueba <- rbind(zona6_prueba, head(datos_prueba,1))
    }
    
  }
  zona6_prueba$Frecuencia <- 1
  zona6_prueba$cluster <- 6
  
  datos_prueba[1, 3] <- datos_prueba[1, 3] - 1
  datos_prueba <- datos_prueba[which(datos_prueba$Frecuencia > 0),]
  #}
  
}

zona1_backup <- zona1_prueba
zona2_backup <- zona2_prueba
zona3_backup <- zona3_prueba
zona4_backup <- zona4_prueba
zona5_backup <- zona5_prueba
zona6_backup <- zona6_prueba
datos_backup <- datos_prueba


zona1_prueba <- zona1_backup
zona2_prueba <- zona2_backup
zona3_prueba <- zona3_backup
zona4_prueba <- zona4_backup
zona5_prueba <- zona5_backup
zona6_prueba <- zona6_backup
datos_prueba <- datos_backup

cont <- 1
cont1 <- 1
valor <- 0
print(nrow(datos_prueba))
while (1 <= nrow(datos_prueba)) {
  ban <- FALSE
  #print(cont1)
  cont1 = cont1 + 1
  quitar <- c(datos_prueba[1, 8])
  #print(which(zona1_prueba[,1] == datos_prueba6))
  b <- FALSE
  #if(datos_prueba6[1,1] == 1247){print(datos_prueba6[1,]); b <- TRUE}
  if (sum(zona1_prueba[,1] == datos_prueba[1,1]) != 0){quitar <- c(quitar, 1)}
  if (sum(zona2_prueba[,1] == datos_prueba[1,1]) != 0){quitar <- c(quitar, 2)}
  if (sum(zona3_prueba[,1] == datos_prueba[1,1]) != 0){quitar <- c(quitar, 3)}
  if (sum(zona4_prueba[,1] == datos_prueba[1,1]) != 0){quitar <- c(quitar, 4)}
  if (sum(zona5_prueba[,1] == datos_prueba[1,1]) != 0){quitar <- c(quitar, 5)}
  if (sum(zona6_prueba[,1] == datos_prueba[1,1]) != 0){quitar <- c(quitar, 6)}
  if(b){print(quitar)}
  cont = cont + 1
  valor <- pesos(datos_prueba[1, ], quitar,b)
  if(b){print("EL VALOR ES");print(valor)}
  datos_prueba[1, 8] <- valor
  if (valor == 1){
    zona1_prueba <- rbind(zona1_prueba, datos_prueba[1,])
    ban <- TRUE
  }
  else if (valor == 2){
    zona2_prueba <- rbind(zona2_prueba, datos_prueba[1,])
    ban <- TRUE
  }
  else if (valor == 3){
    zona3_prueba <- rbind(zona3_prueba, datos_prueba[1,])
    ban <- TRUE
  }
  else if (valor == 4){
    zona4_prueba <- rbind(zona4_prueba, datos_prueba[1,])
    ban <- TRUE
  }
  else if (valor == 5){
    zona5_prueba <- rbind(zona5_prueba, datos_prueba[1,])
    ban <- TRUE
  }
  else if (valor == 6){
    zona6_prueba <- rbind(zona6_prueba, datos_prueba[1,])
    ban <- TRUE
  }
  
  #if(!ban){print(datos_prueba6[1,]);print(valor);print("AAAAAAAAAA")}
  #contador = contador + 1
  datos_prueba[1, 3] <- datos_prueba[1, 3] - 1 
  
  
  datos_prueba <- datos_prueba[which(datos_prueba$Frecuencia > 0),]
  
}



zona_prueba_final <- rbind(zona1_prueba,zona2_prueba,zona3_prueba,
                           zona4_prueba,zona5_prueba,zona6_prueba)

print("VALORES FINALES")
test <- calcula_vol_total(zona_prueba_final, TRUE)

