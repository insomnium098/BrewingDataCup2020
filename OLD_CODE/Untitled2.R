
distancia_test <- function(x1,y1,x2,y2){
  distancia <- sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
  return(distancia)
}
vals <- c()
for (i in 1:nrow(alejados)) {

  temp <- alejados[i, 8]
  cluster_mamalon <- df_cluster_original[!df_cluster_original$cluster == temp,]
  m <- calcula_distancia(alejados[i,5], alejados[i,6], cluster_mamalon$lat,
                  cluster_mamalon$lon, cluster_mamalon$cluster)
  message(m[length(m)])
  vals <- c(vals, m[length(m)])
}
zona1_bruno <- zona1
zona2_bruno <- zona2
zona3_bruno <- zona3
zona4_bruno <- zona4
zona5_bruno <- zona5
zona6_bruno <- zona6

alejados[,8 ] <- vals


for (i in 1:nrow(alejados)) {
  zona5_bruno <- zona5_bruno[!zona5_bruno$Id_Cliente == alejados[i, 1],]
  if (vals[i] == 6){
    zona6_bruno <- rbind(zona6_bruno, alejados[i,1:9])
  }else if (vals[i] == 5){
    zona5_bruno <- rbind(zona5_bruno, alejados[i,1:9])
  }else if (vals[i] == 4){
    zona4_bruno <- rbind(zona4_bruno, alejados[i,1:9])
  }else if (vals[i] == 3){
    zona3_bruno <- rbind(zona3_bruno, alejados[i,1:9])
  }else if (vals[i] == 2){
    zona2_bruno <- rbind(zona2_bruno, alejados[i,1:9])
  }else if (vals[i] == 1){
    zona1_bruno <- rbind(zona1_bruno, alejados[i,1:9])
  }
    
}

zona_bruno_final <- rbind(zona1_bruno,zona2_bruno,
                    zona3_bruno,
                    zona4_bruno,
                    zona5_bruno,
                    zona6_bruno)


