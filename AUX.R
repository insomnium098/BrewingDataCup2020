df_dias_filt <- df_frecuencia[i,]
###Remover el cluster original antes de llamar a calcula dias
cluster_dias <- df_cluster_original[!df_cluster_original$cluster == df_dias_filt$cluster,]
###L


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
      
      print("dsdaasd")
      
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


a <- calcula_dias_aux(df_dias_filt, cluster_dias)
