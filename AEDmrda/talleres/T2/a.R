
# PUNTO 1

x1 = c(9, 2, 6, 5, 8)
x2 = c(12, 8, 6, 4, 10)
x3 = c(3, 4, 0, 2, 1)

# uwu

df = data.frame(x1, x2, x3)
df

# Vector de medias
X = colMeans(df)
X

# Matriz de covarianzas
S = cov(df)
S

# Matriz de correlación
R = cor(df)
R



# PUNTO 3

x1 = c(2, -sqrt(2)*0.8)
x2 = c(-sqrt(2)*0.8, 1)

A = data.frame(x1, x2)
A


# a). Como la transpuesta de la matriz es igual a la matriz (A = A^T), es simetrica
t(A)

lambda = eigen(A) 

# b). Como todos los valores propios de A son positivos, A esta definida positiva.

# c).
# Vectores propios
lambda$vectors

# Valores propios
lambda$values

# d). Descomposición espectral de A
A_desc = (lambda$values[1]*lambda$vectors[,1]%*%t(lambda$vectors[,1])) + 
         (lambda$values[2]*lambda$vectors[,2]%*%t(lambda$vectors[,2]))

A_desc

# e). Determine la inversa de A
inv_A = solve(A)
inv_A

# f). 
lambda_inv_A = eigen(A_inv) 

# Vectores propios
lambda_inv_A$vectors

# Valores propios
lambda_inv_A$values



# PUNTO 5






