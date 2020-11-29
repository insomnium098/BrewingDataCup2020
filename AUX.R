
###Funciones Auxiliares

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

########
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
#####

######
distancia_test <- function(x1,y1,x2,y2){
  distancia <- sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
  return(distancia)
}


#####


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


##########
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

########


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

######




