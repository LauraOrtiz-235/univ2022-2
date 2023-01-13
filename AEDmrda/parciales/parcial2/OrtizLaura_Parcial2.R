# 1. Se midieron las longitudes y los anchos del pétalo y sépalo de 4 especies de flores
#    de plantas iris. Cargue este conjunto de datos en R (se encuentra por defecto como 
#    iris).

head(iris)
summary(iris)

# a. Realice una prueba ANOVA para verificar si las medias de la longitud de los 
#    sépalos son significativamente diferentes entre especies.

# Usando la funcion de anova de R, queremos agrupar por especies para ver si son 
# significativamente diferentes las longitudes de los sépalos entre especies.

anova_especies <- aov(iris$Sepal.Length ~ iris$Species)
anova_especies

summary(anova_especie_setosa)


# Haciendolo a manualmente

especie = split(iris, f=iris$Species)

setosa = especie$setosa
versicolor = especie$versicolor
virginica = especie$virginica

xbar_tot = mean(rbind(setosa$Sepal.Length, versicolor$Sepal.Length, virginica$Sepal.Length))
xbar_tot

xbar_setosa = mean(setosa$Sepal.Length)
xbar_ver = mean(versicolor$Sepal.Length)
xbar_vir = mean(virginica$Sepal.Length)

ss_tr = 50*(xbar_setosa - xbar_tot)^2 + 50*(xbar_ver - xbar_tot)^2 + 50*(xbar_vir - xbar_tot)^2
ss_tr

var_setosa = var(setosa$Sepal.Length)
var_ver = var(versicolor$Sepal.Length)
var_vir = var(virginica$Sepal.Length)

ss_res = var_setosa*(nrow(setosa) - 1) + var_ver*(nrow(versicolor) - 1) + var_vir*(nrow(virginica) - 1)
ss_res


# b. Realice una prueba MANOVA para verificar si las medias de todas las variables son
#    significativamente diferentes entre especies.
manova_especies <- manova(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)~Species, data = iris)
manova_especies

summary(manova_especies)


# Realice un análisis de componentes principales.
summary(iris)

iris_cuantitative_var = rbind(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width)

iris_pca <- prcomp(iris_cuantitative_var, center = TRUE)
iris_pca


# c. ¿Cuántas componentes principales se necesitan para explicar más del 90 % de los 
#    datos?
summary(iris_pca) 

  
# d. ¿Qué variable tiene más peso en la primera componente? ¿Cuánto es el peso de esa
#    variable en esa primera componente?
S = cov(iris_cuantitative_var)
S

eigen_vec = eigen(S)$vectors
eigen_val = eigen(S)$values

py1x1 = (eigen_vec[1,1]*sqrt(eigen_val[1]))/(sqrt(S[1,1]))
py1x1


# e. ¿Qué porcentaje de variabilidad explica la segunda componente principal? ¿Cuál es 
#    la variable que más peso tiene en esa componente? 
summary(iris_pca) 


# f. ¿Cuánta variabilidad se explica si tomo cuatro componentes principales?  

# Se explica un acumulado de varibilidad de 1.000e+00, si se toman las cuatro
# componentes principales.

