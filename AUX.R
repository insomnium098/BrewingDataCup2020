
###Funcion actualiza volumen
###Actualiza el volumen total por zonas
#zona <- zona1

actualiza_volumen <- function(zona){
  unique_clients <- unique(zona$Id_Cliente)
  
  for (i in unique_clients){
    df_client <- filter(zona, Id_Cliente == i)
    df_client$Vol_Total <- df_client$Vol_Entrega
    zona[zona$Id_Cliente == i,] <- df_client
    
  }
  
  return(zona)
  
  
  
}

#zona1_actualiza <- actualiza_volumen(zona1)


####Obtener los clientes con mayor distancia contra el centro del cluster


