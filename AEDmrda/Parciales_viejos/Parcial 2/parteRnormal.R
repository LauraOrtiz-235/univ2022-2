load("/home/laura/Desktop/univ/univ2022-2/AEDmrda/Parciales_viejos/Parcial 2/plantulas.RData")
load("/home/laura/Desktop/univ/univ2022-2/AEDmrda/Parciales_viejos/Parcial 2/abulon.RData")

# PUNTO 1

# a. y b. no nos sale

# c.
summary(dat)

# ESPECIES

#usando la funcion de anova de R especies son las poblaciones que queremos evaluar,
#es decir queremos agrupar por especies.

anova_verde <- aov(dat$verde ~ dat$especies)
anova_verde

summary(anova_verde)

anova_infrarrojo <- aov(dat$infrarrojo ~ dat$especies)
anova_infrarrojo

summary(anova_infrarrojo)


# TIEMPOS

# usando la funcion de anova de R tiempo son las poblaciones que queremos evaluar,
# es decir queremos agrupar por tiempo.

anova_verde_t <- aov(dat$verde ~ dat$tiempo)
anova_verde_t

summary(anova_verde_t)

anova_infrarrojo_t <- aov(dat$infrarrojo ~ dat$tiempo) 
anova_infrarrojo_t

summary(anova_infrarrojo_t)

# COMO LOS P-VALORES DAN TAN PEQUEÑOS NO HAY POSISBILIDAD DE QUE LA HIPOTESIS 
# SEA CIERTA (SI HAY DIFERENCIA DE MEDIAS)

#############################

# ESPECIES

especies = split(dat, f=dat$especies)
tiempo = split(dat, f=dat$tiempo)

SS = especies$SS
LP = especies$LP
JL = especies$JL

xbar_tot_verde = mean(rbind(especies_ss$verde, especies_jl$verde, especies_lp$verde))
xbar_tot_verde

xbar_SS_verde = mean(SS$verde)
xbar_JL_verde = mean(JL$verde)
xbar_LP_verde = mean(LP$verde)

ss_tr_verde = 12*(xbar_SS_verde - xbar_tot_verde)^2 + 12*(xbar_JL_verde - xbar_tot_verde)^2 + 12*(xbar_LP_verde - xbar_tot_verde)^2
ss_tr

#ss_treatmet_green_especies = nrow(especies_ss)*(mean_ss_verde-mean_total)^2+nrow(especies_lp)*(mean_lp_verde-mean_total)^2+nrow(especies_jl)*(mean_jl_verde-mean_total)^2

var_SS_verde = var(SS$verde)
var_JL_verde = var(JL$verde)
var_LP_verde = var(LP$verde)

ss_res_verde = var_SS_verde*(nrow(SS) - 1) + var_JL_verde*(nrow(JL) - 1) + var_LP_verde*(nrow(LP) - 1)
ss_res_verde





# PUNTO 2

# a.
summary(abulon)
head(abulon)

abulon_std = scale(abulon)
summary(abulon_std)

abulon_pca <- prcomp(abulon_std, center = TRUE, scale = TRUE)
abulon_pca 

############################

S = cov(abulon_std)
S

eigen_data = eigen(S)
eigen_data


# b.
summary(abulon_pca)

# LAS COMPONENTES PRINCIPALES QUE ACUMULAN 98% DE LA VAR TOTAL ES DESDE LA 1 A LA 6.

############################

acum = 0
count = 1
total_var = sum(eigen_data$values)

while(acum/total_var <= 0.98){
  acum = acum + eigen_data$values[count]
  count = count + 1 
}

print(paste("Se necesita selecionar desde la componente 1 hasta la", count))


# c.

# Por medio de las 7 primeras variables se podría explicar el 98% de la var de 
# nuestro dataset.


# d.
eigen_val = eigen(S)$values
eigen_vec = eigen(S)$vectors

# PC 1
py1x1 = (eigen_vec[1,1]*sqrt(eigen_val[1]))/(sqrt(S[1,1]))
py1x2 = (eigen_vec[1,2]*sqrt(eigen_val[1]))/(sqrt(S[2,2]))
py1x3 = (eigen_vec[1,3]*sqrt(eigen_val[1]))/(sqrt(S[3,3]))
py1x4 = (eigen_vec[1,4]*sqrt(eigen_val[1]))/(sqrt(S[4,4]))
py1x5 = (eigen_vec[1,5]*sqrt(eigen_val[1]))/(sqrt(S[5,5]))
py1x6 = (eigen_vec[1,6]*sqrt(eigen_val[1]))/(sqrt(S[6,6]))
py1x7 = (eigen_vec[1,7]*sqrt(eigen_val[1]))/(sqrt(S[7,7]))
py1x8 = (eigen_vec[1,8]*sqrt(eigen_val[1]))/(sqrt(S[8,8]))
py1x9 = (eigen_vec[1,9]*sqrt(eigen_val[1]))/(sqrt(S[9,9]))

correlations = data.frame(py1x1,py1x2,py1x3,py1x4,py1x5,py1x6,py1x7,py1x8,py1x9)
correlations

# PC 2
py2x1 = (eigen_vec[2,1]*sqrt(eigen_val[2]))/(sqrt(S[1,1]))
py2x2 = (eigen_vec[2,2]*sqrt(eigen_val[2]))/(sqrt(S[2,2]))
py2x3 = (eigen_vec[2,3]*sqrt(eigen_val[2]))/(sqrt(S[3,3]))
py2x4 = (eigen_vec[2,4]*sqrt(eigen_val[2]))/(sqrt(S[4,4]))
py2x5 = (eigen_vec[2,5]*sqrt(eigen_val[2]))/(sqrt(S[5,5]))
py2x6 = (eigen_vec[2,6]*sqrt(eigen_val[2]))/(sqrt(S[6,6]))
py2x7 = (eigen_vec[2,7]*sqrt(eigen_val[2]))/(sqrt(S[7,7]))
py2x8 = (eigen_vec[2,8]*sqrt(eigen_val[2]))/(sqrt(S[8,8]))
py2x9 = (eigen_vec[2,9]*sqrt(eigen_val[2]))/(sqrt(S[9,9]))

correlations2 = data.frame(py2x1,py2x2,py2x3,py2x4,py2x5,py2x6,py2x7,py2x8,py2x9)
correlations2
