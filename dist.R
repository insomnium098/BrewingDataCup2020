
#library(e1071)
distancia <- function(x, y) {
        
        return( ((x[1,4] - y[1,4])^2 + (x[1,5] - y[1,5])^2)^(1/2) )
}
distancia1 <- function(x, y, l) {
        print(x)
        
        return( ((x[1,5] - y[l,1])^2 + (x[1,6] - y[l,2])^2)^(1/2) )
}
n = nrow(zona5)
matriz_adyacencia <- matrix(NA, n, n)
for (i in 1:n) {
        for (j in 1:n) {
                matriz_adyacencia[i, j] = distancia(zona5[i,], zona5[j,])
        }
}
#a <- allShortestPaths(matriz_adyacencia)

d <- c()
for (i in 1:n) {
        d <- c((sum(matriz_adyacencia[i,])/n), d)
}
###
for (i in 1:n) {
        print(distancia_test(zona5[i,5],
                               zona5[i,6],
                               df_cluster_original[5,1],
                               df_cluster_original[5,2]))
        d <- c(distancia_test(zona5[i,5],
                              zona5[i,6],
                              df_cluster_original[5,1],
                              df_cluster_original[5,2]), d)
}

###Funcion mamalona

for(i in 1:n){
        dist <- distancia_test(zona5[i,5],
                               zona5[i,6],
                               df_cluster_original[5,1],
                               df_cluster_original[5,2])
        if(!exists("dist_final")){
                dist_final <- dist
        } else {
                dist_final <- c(dist_final, dist)
        }
}
##3
#d <- as.data.frame(d)

a <- which( dist_final >= 1.3)
#zona5[a,]

plot(dist_final, zona5$Vol_Entrega)
hist(dist_final)

temp = 0
for (i in a) {
        temp = temp + zona5[i,4]*zona5[i, 3] 
}

alejados <- zona5[a,]
alejados <- cbind(alejados, dist_final[a])
colnames(alejados)[10] <- "Distancia"
