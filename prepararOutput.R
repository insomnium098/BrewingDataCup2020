
output <- data.frame(Id_Cliente = integer(), D1=integer(), D2=integer()
                     , D3= integer(), D4=integer(),D5=integer(),D6=integer())
clientes <- datos[,1]
for (i in clientes) {
        output[i,1] <- i
        output[i,2] <- sum(zona1_test[,1] == i)
        output[i,3] <- sum(zona2_test[,1] == i)
        output[i,4] <- sum(zona3_test[,1] == i)
        output[i,5] <- sum(zona4_test[,1] == i)
        output[i,6] <- sum(zona5_test[,1] == i)
        output[i,7] <- sum(zona6_test[,1] == i)
}

for (i in 2:7) {
        print(which(output[,i]>=2))
}
which(output[,2] >= 2)
write.csv(output,"output.csv", row.names = FALSE)
##Puntaje de 1.5 con m = 640