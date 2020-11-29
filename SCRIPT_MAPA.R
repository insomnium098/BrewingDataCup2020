##ordenar zona_rpeuba_final_final


zona_mapa1 <- zona1_prueba
zona_mapa2 <- zona2_prueba
zona_mapa3 <- zona3_prueba
zona_mapa4 <- zona4_prueba
zona_mapa5 <- zona5_prueba
zona_mapa6 <- zona6_prueba

###MAPA1


funcion_consigue_coord <- function(zona_mapa1){
  for(i in 1:nrow(zona_mapa1)){
    filt <- datos[datos$Id_Cliente %in% zona_mapa1[i,1],]
    zona_mapa1[i,5] <- filt$lat
    zona_mapa1[i,6] <- filt$lon
  }
  
  return(zona_mapa1)
  
}

zona_mapa1 <- funcion_consigue_coord(zona_mapa1)
zona_mapa2 <- funcion_consigue_coord(zona_mapa2)
zona_mapa3 <- funcion_consigue_coord(zona_mapa3)
zona_mapa4 <- funcion_consigue_coord(zona_mapa4)
zona_mapa5 <- funcion_consigue_coord(zona_mapa5)
zona_mapa6 <- funcion_consigue_coord(zona_mapa6)





write.csv(zona_mapa1,"zona_mapa1.csv")
write.csv(zona_mapa2,"zona_mapa2.csv")
write.csv(zona_mapa3,"zona_mapa3.csv")
write.csv(zona_mapa4,"zona_mapa4.csv")
write.csv(zona_mapa5,"zona_mapa5.csv")
write.csv(zona_mapa6,"zona_mapa6.csv")




