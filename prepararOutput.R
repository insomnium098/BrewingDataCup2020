
output <- data.frame(Id_Cliente = integer(), D1=integer(), D2=integer()
                     , D3= integer(), D4=integer(),D5=integer(),D6=integer())
clientes <- datos[,1]
for (i in clientes) {
        output[i,1] <- i
        output[i,2] <- sum(zona1[,1] == i)
        output[i,3] <- sum(zona2[,1] == i)
        output[i,4] <- sum(zona3[,1] == i)
        output[i,5] <- sum(zona4[,1] == i)
        output[i,6] <- sum(zona5[,1] == i)
        output[i,7] <- sum(zona6[,1] == i)
}

write.csv(output,"output.csv", row.names = FALSE)
