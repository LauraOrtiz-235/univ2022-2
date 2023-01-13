# NO ME DIO TIEMPO DE DESCARGAR MARKDOWN Y DE HACER UN PDF PORQUE SE 
# ME FUE EL INTERNET EN EL PC Y TUVE QUE EMPEZAR DESDE UN PC DE LA U
# IGUALMENTE EL .R ESTA COMENTADO

# PUNTO 1

cars

df = data.frame(cars)

# Se crea la matriz del conjunto de datos cars
m = data.matrix(df)
m

# A). Calcule X, S y R

# Se calcula las medias de cada variable, para calcular el vector de medias:

X_mean = colMeans(m)
X_mean
# El vector de medias es: (15.40 42.98)

# Matriz de covarianzas:

S_cov = cov(m)
S_cov

# Matriz de correlación:

R_cor = cor(m)
R_cor



# B). Evalúe la hipótesis nula H0 : µ0' = [16, 40] usando el estadístico de T^2
#     ¿Qué distribución sigue T^2? ¿Qué conclusión saca?

# Datos para evaluar la hipótesis

m0 = c(16,40)
mu = colMeans(m)
n = nrow(m)
p = ncol(m)

s = cov(m) * ((n - 1)/(n))

t_calculo = n*rbind(mu - m0) %*% solve(s) %*% t(rbind(mu - m0))
t_calculo

# T^2 = 6.934651

# T^2 esta distribuido como:
# (((n - 1) * p) / (n - p)) * F p, n-p grados de libertad:

((n - 1) * p) / (n - p)

# Entonces T^2 ~ 2.04*F 2, 48

# Ahora tomemos alpha con un nivel de 0.05

alpha = 0.05
t_val = qf(1 - alpha, df1 = p, df2 = n-p, lower.tail = FALSE) * ((n - 1) * p) / (n - p)
t_val

abs(t_calculo) > t_val

# Se puede concluir que como t_calculado es mayor a t_val, se rechaza H0.



# C). Obtenga los intervalos de T^2 simultaneos para las dos variables

# Vamos a usar el intervalo de confianza de muestra grande.

m = data.matrix(df)
x_bar = matrix(c(15.40, 42.98))
S = matrix(c(27.95918, 109.9469,
             109.9469, 664.0608), byrow=FALSE,ncol=2)
n = nrow(m)
p = ncol(m)

a = matrix(c(1, 0,
             0, 1), byrow=FALSE,ncol=2)

alpha = 0.05

upr_i = c()
lwr_i = c()

for (i in 1:nrow(a)) {
  upr_i = append(upr_i, x_bar[i] + sqrt((p*(n - 1)) / (n*(n - p)) * qf(1 - alpha, p, n-p)*t(a[i,])%*% S %*% a[i,]))
  lwr_i = append(lwr_i, x_bar[i] - sqrt((p*(n - 1)) / (n*(n - p)) * qf(1 - alpha, p, n-p)*t(a[i,])%*% S %*% a[i,]))
}

upr_i
lwr_i

# Es decir los intervalos fueron: 13.49140 <= mu <= 17.30860 
#                                 33.67843 <= mu <= 52.28157



# D). Obtenga la región de confianza para el vector de medias 
#     y grafique la elipse

S = matrix(c(27.95918, 109.9469,
             109.9469, 664.0608), byrow=FALSE,ncol=2)

sub_S = matrix(c(S[1,1], S[2,1], 
                 S[1,2], S[2,2]), ncol=2, nrow=2)

eigen_decomp = eigen(sub_S)
eigen_decomp

# teta = atan(y/x)
angulo = atan(eigen_decomp$vectors[2,1] / eigen_decomp$vectors[1,1])
angulo

install.packages("plotrix")

library(plotrix)

means = c(15.40, 42.98)
n = 50
p = 2
alpha = 0.05

long1 = sqrt(eigen_decomp$values[1]) * sqrt((p*(n-1)) / (n*(n-p)) * qf(1-alpha, p, n-p))
long2 = sqrt(eigen_decomp$values[2]) * sqrt((p*(n-1)) / (n*(n-p)) * qf(1-alpha, p, n-p))

longitudes = c(long1, long2)

# Graficar

plot(0, pch='', ylab='', xlim=c(10, 25), ylim=c(30, 60))
draw.ellipse(x=means[1], y=means[2], a=longitudes[1], b=longitudes[2], angle=angulo, deg=FALSE)

abline(v=lwr_i[1],lty=3);abline(v=upr_i[1],lty=3)
abline(h=lwr_i[2],lty=3);abline(h=upr_i[2],lty=3)
