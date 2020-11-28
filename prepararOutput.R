
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
##Puntaje de 1.5 con m = 640