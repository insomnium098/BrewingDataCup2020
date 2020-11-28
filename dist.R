
#library(e1071)
distancia <- function(x, y) {
        
        return( ((x[1,4] - y[1,4])**2 + (x[1,5] - y[1,5])**2)**(1/2) )
}
distancia1 <- function(x, y, l) {
        
        return( ((x[1,4] - y[l,1])**2 + (x[1,5] - y[l,2])**2)**(1/2) )
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
#d <- as.data.frame(d)

a <- which( d >= 60)
#zona5[a,]

plot(d, zona5$Vol_Entrega)
hist(d)

temp = 0
for (i in a) {
        temp = temp + zona5[i,4]*zona5[i, 3] 
}

alejados <- zona5[a,]
alejados <- cbind(alejados, d[a])
colnames(alejados)[10] <- "Distancia"
