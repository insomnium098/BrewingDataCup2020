
names <- colnames(zona1_prueba)
colnames(zona2_prueba) <- names
colnames(zona3_prueba) <- names
colnames(zona4_prueba) <- names
colnames(zona5_prueba) <- names
colnames(zona6_prueba) <- names


prueba_zona <- rbind(zona1_prueba,zona2_prueba,zona3_prueba,
                     zona4_prueba,zona5_prueba,zona6_prueba)

ggplot(zona1_prueba, aes(x=lat, y=lon, color=cluster)) + 
  geom_point()

ggplot(zona2_prueba, aes(x=lat, y=lon)) + 
  geom_point()

cbp1 <- c("chartreuse1", "tomato2", "darkorchid", "sienna1",
          "dodgerblue1", "darkslategrey")

ggplot(zona_prueba_final, aes(x=lon, y=lat, color=as.factor(cluster)),
       fill=as.factor(cluster)) + 
  geom_point() + theme(legend.title = element_blank()) + scale_colour_manual(values=cbp1)


ggplot(zona_prueba_final_final, aes(x=lon, y=lat, color=as.factor(cluster)),
       fill=as.factor(cluster)) + 
  geom_point() + theme(legend.title = element_blank()) + scale_colour_manual(values=cbp1)

