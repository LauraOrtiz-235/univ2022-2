install.packages("expm")

library(expm)

c1 = cbind(8, 2, 3, 1)
c2 = cbind(2, 5, -1, 3)
c3 = cbind(3, -1, 6, -1)
c4 = cbind(1, 3, -2, 7)

#Matriz de medias y covarianzas 
med = rbind(-3, 2, 0, 1)
cov = rbind(c1, c2, c3, c4)

#DIvisiones de la matriz SIGMA
sigma_11 = rbind( cbind(8, 2) , cbind(2,5) )
sigma_12 = rbind( cbind(3, 1) , cbind(-1, 3) )
sigma_21 = rbind( cbind(3, -1) , cbind(1, 3) )
sigma_22 = rbind( cbind(6, -2) , cbind(-2, 7) )


#Se obtiene la raiz de la matriz invertida de sig11 y la 
#invertida de sig22
sigma_11_sq = sqrtm(solve(sigma_11))
sigma_22_inv = solve(sigma_22)

mult = sigma_11_sq %*% sigma_12 %*% sigma_22_inv %*% sigma_21 %*% sigma_11_sq
mult

#SE OBTIENEN LOS EIGEN VALORES Y LOS EIGENVECTORES 

ev = eigen(mult)
values = ev$values
vectors = ev$vectors
vectors

#Se obtiene b1

b1 = sigma_22_inv = sigma_22_inv %*% sigma_21 %*% vectors[,1]
b1

esc = t(b1) %*% sigma_22 %*% b1 
esc

b1esc = (1 / sqrt(esc)) * b1
b1esc

Sigma11 = cbind(c(100,0),c(0,1))
Sigma22 = cbind(c(1,0),c(0,100))
Sigma12 = cbind(c(0,0),c(0.95,0))
Sigma21 = cbind(c(0,0),c(0.95,0))

newMatrix1= solve(sqrtm(Sigma11))%*%Sigma12%*%solve(Sigma22)%*%t(Sigma12)%*%solve(sqrtm(Sigma11))
eigenvectors1= eigen(newMatrix1)$vectors

newMatrix1

newMatrix2= solve(sqrtm(Sigma22))%*%t(Sigma12)%*%solve(Sigma11)%*%Sigma12%*%solve(sqrtm(Sigma22))
eigenvectors2 = eigen(newMatrix2)$vectors

b1 = eigenvectors1[,1]%*%solve(sqrtm(Sigma22))
b1
b2 = eigenvectors1[,2]%*%solve(sqrtm(Sigma22))

