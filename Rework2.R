calcula_vol_total <- function(zona_prueba_final){
  for( i in 1:6){
    df_filt <- filter(zona_prueba_final, cluster == i)
    vol_total <- sum(df_filt$Vol_Entrega)
    n_elementos <- nrow(df_filt)
    promedio <- mean(df_filt$Vol_Entrega)
    
    ms <- paste0("Vol_total= ", vol_total,
                 " n_elementos= ", n_elementos,
                 " Promedio= ", promedio)
    

      #print(quantile(df_filt$Vol_Entrega))
    message(ms)

  
  }
}


##df_cluster_original


####Funcion output para este script

funcion_output <- function(tabla_salida){
  
  output <- data.frame(Id_Cliente = integer(), D1=integer(), D2=integer()
                       , D3= integer(), D4=integer(),D5=integer(),D6=integer())
  clientes <- datos[,1]
  
  for (i in clientes) {
    
    ###Cluster1
    
    filt <- tabla_salida[tabla_salida$Id_Cliente %in% i,]
    output[i,1] <- i
    output[i,2] <- sum(tabla_salida[i,8] == 1)
    output[i,3] <- sum(tabla_salida[i,8] == 2)
    output[i,4] <- sum(tabla_salida[i,8] == 3)
    output[i,5] <- sum(tabla_salida[i,8] == 4)
    output[i,6] <- sum(tabla_salida[i,8] == 5)
    output[i,7] <- sum(tabla_salida[i,8] == 6)
  }
  
  which(output[,2] == 0)
  which(output[,3] >= 2)
  which(output[,4] >= 2)
  which(output[,5] >= 2)
  which(output[,6] >= 2)
  which(output[,7] >= 2)
  write.csv(output,"output.csv", row.names = FALSE)
  
}




#####


funcion_revisa_zona <- function(id,zona){
  
  if(id %in% zona$Id_Cliente){
    return(TRUE)
  } else {
    return(FALSE)
  }
  
  
}



######
calcula_distancia_centro <- function(x1,y1,x2,y2){
  #c2 y y2 son los centros
  distancia <- sqrt(((x2 - x1)^2) + ((y2 - y1)^2))
  return(distancia)
}


######


m = 100
datos_prueba <- datos_clust
rm(zona1_prueba)
rm(zona2_prueba)
rm(zona3_prueba)
rm(zona4_prueba)
rm(zona5_prueba)
rm(zona6_prueba)


######

#####Para zona 1 calculamos todas las distancias de los puntos a la zona 1
#####Para encontrar el punto mas cercano a zona 1, este punto no debera estar ya en
##zona1 #, este se le pegara a zona1.

a <- funcion_busca_y_pega (datos_prueba, zona1)
centros_cluster <- df_cluster_original
zona_a_buscar <- 1
zona_todos <- datos_clust


funcion_busca_y_pega <- function(zona_todos,zona_a_buscar,centros_cluster){
  ####Calcular las distancias de zona todos a la zona a buscar
  filt_zona <- centros_cluster[centros_cluster[,3] == zona_a_buscar,]
  x_zona_buscar <- filt_zona[1,1]
  y_zona_buscar <- filt_zona[1,2]
  ####Comparar todos los puntos de zona_todos contra
  ###x_zona_buscar y y_zona_buscar
  
  for (i in 1:nrow(zona_todos)){
    x_cliente <- zona_todos[i,5]
    y_cliente <- zona_todos[i,6]
    
    dist_cliente <- calcula_distancia_centro(x_zona_buscar, y_zona_buscar,
                                             x_cliente, y_cliente)
    if(!exists("dist_cliente_final")){
      dist_cliente_final <- dist_cliente
    } else {
      dist_cliente_final <- c(dist_cliente_final, dist_cliente)
    }
  }
  
  ###Añadimos la columna
  zona_final <- zona_todos
  zona_final$distancia_a_cluster <- dist_cliente_final
  rm(dist_cliente_final)
  
  return(zona_final)
  
}

##########
datos_test_compara_cluster_1 <- funcion_busca_y_pega(zona_todos, 1, centros_cluster)
###los ordenamos
datos_test_compara_cluster_1 <- datos_test_compara_cluster_1[order(datos_test_compara_cluster_1$distancia_a_cluster),]

###El elemento de hasta arriba(el mas cercano) se le pega a la zona correspondiente
##en este caso zona 1 y se le reduce la frecuencia



zona_final <- zona_todos

funcion_elige_minimo_todos <- function(zona_final, zona_cluster, contador){
  contador = contador + 1
  zona_final <- zona_final[order(zona_final$distancia_a_cluster),]
  #zona_elige <- zona_final[1,]
  zona_elige <- zona_final[contador,]
  
  if(funcion_revisa_zona(zona_elige[1,1], zona_cluster)) {
    valores <- funcion_elige_minimo_todos(zona_final, zona_cluster, contador)
    zona_elige <- valores[1]
    contador <- valores[2]
  }
  #zona_elige <- valores[1]
  ###Si el elemento ya esta en el cluster no se hace nada
  #id <- zona_elige[1,1]
  
  
  ##Actualizar frecuencia
  #Si es 0 se elimina
  else {
    zona_final[contador,3] <- zona_final[contador,3] - 1
    if(zona_final[contador,3] == 0){
      zona_final <- zona_final[-c(contador),]
    }
  }
  
  return(c(zona_elige, contador))
}

funcion_elige_minimo_primer_caso <- function(zona_final){
  zona_final <- zona_final[order(zona_final$distancia_a_cluster),]
  zona_elige <- zona_final[1,]
  
  
  ###Si el elemento ya esta en el cluster no se hace nada
  #id <- zona_elige[1,1]
  
  
  ##Actualizar frecuencia
  #Si es 0 se elimina
  
  zona_final[1,3] <- zona_final[1,3] - 1
  if(zona_final[1,3] == 0){
    zona_final <- zona_final[2:nrow(zona_final),]
    
  }
  
  return(zona_elige)
}

###Esta funcion recibe un df y actualiza la frecuencia de su
##primer elemento, lo elimina si es 0
##Debe ser llamada inmediatamente despues de
###funcion_elige_minimo
funcion_actualiza_primero <- function(zona_final){
  
  zona_final[1,3] <- zona_final[1,3] - 1
  if(zona_final[1,3] == 0){
    zona_final <- zona_final[2:nrow(zona_final),]
  }
  
  return(zona_final)
}

funcion_actualiza_todos <- function(zona_final, numero){
  print(numero)
  zona_final[numero,3] <- zona_final[numero,3] - 1
  if(zona_final[numero,3] == 0){
    zona_final <- zona_final[-c(numero),]
  }
  return(zona_final)
}




######TESTING FUNCIONES
zona_final <- datos_test_compara_cluster_1
###Despues de llamar la funcion se debe de actualizar manualmente la zona
test <- funcion_elige_minimo_y_actualiza(zona_final)
####Actualizar
zona_final <- funcion_actualiza(zona_final)

####Debemos de crear un dataframe para cada cluster

if(!exists(zona1_test)){
  zona1_test <- test
} else{
  zona1_test <- rbind(zona1_test, test)
}

####Automatizar
zona_todos <- datos_clust
centros_cluster$cluster <- as.integer(centros_cluster$cluster)
total <- as.integer(sum((datos_prueba$Frecuencia))) # length(centros_cluster$cluster))

##HCER RM
rm(zona1_test,zona2_test,zona3_test,
   zona4_test,zona5_test,zona6_test)

###


clientes <- datos[,1]

visitas <- as.data.frame(clientes)
#visitas$Id_Cliente <- clientes
visitas$D1 <- 0
visitas$D2 <- 0
visitas$D3 <- 0
visitas$D4 <- 0
visitas$D5 <- 0
visitas$D6 <- 0

repeticiones <- rep(1:6, total + 1)
##inicializar zona de salida
tabla_salida <- as.data.frame(0)
tabla_salida$Id_1 <- 0
tabla_salida$Id_2 <- 0
tabla_salida$Id_3 <- 0
tabla_salida$Id_4 <- 0
tabla_salida$Id_5 <- 0
tabla_salida$Id_6 <- 0
tabla_salida$Id_7 <- 0
tabla_salida$Id_8 <- 0
tabla_salida$Id_9 <- 0
colnames(tabla_salida) <- colnames(zona_todos)

total <- as.integer(sum((datos_prueba$Frecuencia))) + 4
repeticiones <- rep(1:6, as.integer(total/6))
zona_todos <- datos_clust

##
for (i in 1:total){
#for (i in 1:100){
  print(i)
  
  
  
  #####Validar que el elemento no este en el cluster
  
  #####
  #print(paste0("El i es = ", repeticiones[i]))
  zona_todos <- funcion_busca_y_pega(zona_todos, repeticiones[i], centros_cluster)
  zona_todos <- zona_todos[order(zona_todos$distancia_a_cluster),]
  #####Aqui el primer elemento es el mas cercano al cluster i
  ####verificar que no exista en la tabla de salida, si no ir por el siguiente
  j <- 1
  elemento_salida <- zona_todos[j,]
  #message(paste0(elemento_salida$Id_Cliente))
  ###Asignar su cluster correcto en columna cluster
  elemento_salida$cluster <- repeticiones[i]
  
  if(elemento_salida$Id_Cliente %in% tabla_salida$Id_Cliente == FALSE){

    ###Agregarlo a la tabla de salida
    tabla_salida <- rbind(tabla_salida, elemento_salida)
    #####Quitar -1 a la frecuencia del elemento en zona todos
    zona_todos[j,3] <- zona_todos[j,3] - 1
    ###Si la frecuencia es 0, se remueve
    if(zona_todos[j,3] == 0){
      ###Remover el elemento de zona todos
      zona_todos <- zona_todos[-j,]
    }
    test <- zona_todos[is.na(zona_todos),]
    if(nrow(zona_todos[is.na(zona_todos),]) != 0){
      message(i)
    }
    
  } else {
    #print("repetido")
    #print(repeticiones[i])
    #print(elemento_salida$Id_Cliente)
    ###Caso donde ya exista en ese cluster
    j <- j + 1
    elemento_salida <- zona_todos[j,]
    k <- elemento_salida$Id_Cliente %in% tabla_salida$Id_Cliente &&
      elemento_salida$cluster %in% tabla_salida$cluster
    while(k == TRUE){
      j <- j+1
      elemento_salida <- zona_todos[j,]
      k <- elemento_salida$Id_Cliente %in% tabla_salida$Id_Cliente &&
        elemento_salida$cluster %in% tabla_salida$cluster
    }
    #print("El elemento elegido en su lugar es:")
    #print(elemento_salida$Id_Cliente)
    #print("Su cluster anterior es:")
    #print(elemento_salida$cluster)
    
    ##Asignar su cluster correcto en columna cluster
    elemento_salida$cluster <- repeticiones[i]
    
    elemento_salida$Frecuencia <- elemento_salida$Frecuencia - 1
    #elemento_salida[1,1] <- paste0(elemento_salida[1,1],j)
    #print("Su cluster Nuevo es:")
    #print(elemento_salida$cluster)
    ###Agregarlo a la tabla de salida
    tabla_salida <- rbind(tabla_salida, elemento_salida)
    #####Quitar -1 a la frecuencia del elemento en zona todos
    if(is.na(zona_todos[j,3])){
      j <- 1
    }
    #message("Inicio")
    #print(zona_todos[j,3])
    #message("------")
    zona_todos[j,3] <- zona_todos[j,3] - 1
    #message("------")
    #print(zona_todos[j,3])
    ###Si la frecuencia es 0, se remueve
    if(zona_todos[j,3] == 0){
      ###Remover el elemento de zona todos
      zona_todos <- zona_todos[-j,]
    }
    test <- zona_todos[is.na(zona_todos),]
    if(nrow(zona_todos[is.na(zona_todos),]) != 0){
      message(i)
    }
    
    
    
    
    
    
  }
  

  
  
}
#zona_test_final <- rbind(zona1_test,zona2_test,zona3_test,
#                         zona4_test,zona5_test,zona6_test)

###Remover el primer row de tabla salida ya que tiene el valor
### inicializado de 0 y el ultimo, ya que es un NA

tabla_salida <- tabla_salida[2:nrow(tabla_salida),]
###Remover NAS
test <- tabla_salida[!is.na(tabla_salida$cluster),]
test <- tabla_salida[!is.na(tabla_salida$Vol_Total),]

print("VALORES FINALES")

calcula_vol_total(test)

fin <- funcion_output(tabla_salida)













