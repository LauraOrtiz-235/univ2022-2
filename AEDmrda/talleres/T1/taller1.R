## PUNTO 1

boxplot(oleic~region,
        data=olive,
        main="Oleic vs region",
        xlab="Region",
        ylab="Oleic",
        col="steelblue",
        border="black"
)


## PUNTO 2
# Determine la media, la mediana, la moda y la desviación estándar de cada una de las variables. 
# Se puede calcular a todas la variables? a cuales no? Justifique su respuesta.

head(mtcars)

mode <- function(x){
  return(as.numeric(names(which.max(table(x)))))
}

# MPG
mean(mtcars$mpg)
median(mtcars$mpg)
mode(mtcars$mpg)
sd(mtcars$mpg)

# CYL
mean(mtcars$cyl)
median(mtcars$cyl)
mode(mtcars$cyl)
sd(mtcars$cyl)

# DISP
mean(mtcars$disp)
median(mtcars$disp)
mode(mtcars$disp)
sd(mtcars$disp)

# HP
mean(mtcars$hp)
median(mtcars$hp)
mode(mtcars$hp)
sd(mtcars$hp)

# DRAT
mean(mtcars$drat)
median(mtcars$drat)
mode(mtcars$drat)
sd(mtcars$drat)

# WT
mean(mtcars$wt)
median(mtcars$wt)
mode(mtcars$wt)
sd(mtcars$wt)

# QSEC
mean(mtcars$qsec)
median(mtcars$qsec)
mode(mtcars$qsec)
sd(mtcars$qsec)

# VS
mean(mtcars$vs)
median(mtcars$vs) # La mediana no tiene sentido pues esta variables toman valores binarios (1 y 0)
mode(mtcars$vs)
sd(mtcars$vs)

# AM
mean(mtcars$am)
median(mtcars$am) # La mediana no tiene sentido pues esta variables toman valores binarios (1 y 0)
mode(mtcars$am)
sd(mtcars$am)

# GEAR
mean(mtcars$gear)
median(mtcars$gear)
mode(mtcars$gear)
sd(mtcars$gear)

# CARB
mean(mtcars$carb)
median(mtcars$carb)
mode(mtcars$carb)
sd(mtcars$carb)


# Determinar qué variable presenta valores atípicos, ¿cómo los ha encontrado?

# Por medio de boxplots determinamos los valores atípicos, y las variables qsec, carb, hp, y wt 
# tienen valores atípicos.

# MPG, QSEC
boxplot(mtcars$mpg, mtcars$qsec)

# CYL, GEAR, CARB
boxplot(mtcars$cyl, mtcars$gear, mtcars$carb)

# DISP, HP
boxplot(mtcars$disp, mtcars$hp)

# DRAT, WT
boxplot(mtcars$drat, mtcars$wt)

# VS, AM
boxplot(mtcars$vs, mtcars$am)


# Hacer el histograma para cada una de las variable usando 5 intervalos. De nuevo, 
# está gráfica es útil para todas las variables? justifique su respuesta.

# MPG
hist(breaks=5, mtcars$mpg)

# CYL
hist(breaks=5, mtcars$cyl)

# DISP
hist(breaks=5, mtcars$disp)

# HP
hist(breaks=5, mtcars$hp)

# DRAT
hist(breaks=5, mtcars$drat)

# WT
hist(breaks=5, mtcars$wt)

# QSEC
hist(breaks=5, mtcars$qsec)

# VS
hist(breaks=5, mtcars$vs)

# AM
hist(breaks=5, mtcars$am)

# GEAR
hist(breaks=5, mtcars$gear)

# CARB
hist(breaks=5, mtcars$carb)


# Realice una gráfica que incluya el diagrama de cajas de todas 
# las variables de tal manera de que se puedan comparar.

boxplot(scale(mtcars))


## PUNTO 3. 
olive_acids <- subset(olive, select = c(-region, -area, -oleic))
plot(olive_acids)


## PUNTO 4.
colors <- c("#BCEE68", "#00EEEE", "#7FFFD4")
colors <- colors[as.numeric(olive$regio)]
scatterplot3d(olive$linolenic, olive$arachidic, olive$eicosenoic, pch = 16, color = colors,
              xlab="Linolenic", ylab="Arachidic", zlab="Eicosenoic")


## PUNTO 5. 
x = data.frame(x1 = c(-6, -3, -2, 1, 2, 5, 6, 8),
               x2 = c(-2, -3, 1, -1, 2, 1, 5, 3))

plot(x$x1,
     x$x2)

var(x$x1)
var(x$x2)

cov(x$x1, x$x2)
