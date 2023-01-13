                                    # TALLER 5 #

# PUNTO 1

# 1. Determine the population principal components Y1 and Y2 for the covariance matrix.
y1 <- c(5,2)
y2 <- c(2,2)

Sigma = cbind(y1,y2)

eigen_val_Sigma = eigen(Sigma)$values
eigen_vec_Sigma = eigen(Sigma)$vectors

eigen_vec_Sigma
eigen_val_Sigma

# Also, calculate the proportion of the total population variance explained by the 
# first principal component.
prop_pc1_Sigma = eigen_val_Sigma[1]/(eigen_val_Sigma[1] + eigen_val_Sigma[2])
prop_pc1_Sigma



# PUNTO 2

# Convert the covariance matrix in Exercise 1 to a correlation matrix ρ.

# a. Determine the principal components Y1 and Y2 from ρ and compute the 
#    proportion of total population variance explained by Y1.
cor = cov2cor(Sigma)
cor

eigen_val_cor = eigen(cor)$values
eigen_vec_cor = eigen(cor)$vectors

eigen_vec_cor

prop_pc1_cor = eigen_val_cor[1]/(eigen_val_cor[1] + eigen_val_cor[2])
prop_pc1_cor


# b. Compare the components calculated in Part a with those obtained in Exercise 1.
#    Are they the same? Should they be?

# No son los mismos componentes, pues las matrices son diferentes :p.


# c. Compute the correlations ρY1,Z1, ρY1,Z2, and ρY2,Z1
py1z1_cor = (eigen_vec_cor[1,1]*sqrt(eigen_val_cor[1]))/(sqrt(Sigma[1,1]))
py1z2_cor = (eigen_vec_cor[1,2]*sqrt(eigen_val_cor[1]))/(sqrt(Sigma[2,2]))
py2z1_cor = (eigen_vec_cor[2,1]*sqrt(eigen_val_cor[2]))/(sqrt(Sigma[1,1]))

correlations = data.frame(py1z1_cor, py1z2_cor, py2z1_cor)
correlations



# PUNTO 3

# Data on x1 = sales and x2 = profits for the 10 largest companies in the world were
# already listed.

S = cbind(c(7476.45, 303.62), c(303.62, 26.19))

# a. Determine the sample principal components and their variances for these data.
#    (You may need the quadratic formula to solve for the eigenvalues of S.)

eigen_val_S = eigen(S)$values
eigen_vec_S = eigen(S)$vectors

eigen_vec_S

# POR TEOREMA LAS VAR SON:

eigen_val_S


# b. Find the proportion of the total sample variance explained by y1gorrito. 
prop_y1_S = eigen_val_S[1]/(eigen_val_S[1] + eigen_val_S[2])
prop_y1_S


# c. Compute the correlation coefficients r_y1gorrito,xk, k = 1, 2. What interpretation, if
#    any, can you give to the first principal component?
py1x1_S = (eigen_vec_S[1,1]*sqrt(eigen_val_S[1]))/(sqrt(S[1,1]))
py1x2_S = (eigen_vec_S[1,2]*sqrt(eigen_val_S[1]))/(sqrt(S[2,2]))

correlationsS = data.frame(py1x1_S, py1x2_S)
correlationsS

# Como se puede observar, la componente principal explica casi por completo los datos. 
# Además, la correlacion py_1x1 nos indica que por cada valor de x1 que se tome, y1 
# crece proporcionalmente pero en sentido inverso, y con cada valor de x2 que se tome,
# y1 aumenta a razon de 0.687 en el mismo sentido de x2.



# PUNTO 4

# Convert the covariance matrix S in Exercise 3 to a sample correlation matrix R.

# a. Find the sample principal components y1gorrito, y2gorrito and their variances.
R = cov2cor(S)
R

eigen_val_R = eigen(R)$values
eigen_vec_R = eigen(R)$vectors

eigen_vec_R
eigen_val_R


# b. Compute the proportion of the total sample variance explained by y1gorrito.
prop_pc1_R = eigen_val_R[1]/(eigen_val_R[1] + eigen_val_R[2])
prop_pc1_R


# c. Compute the correlation coefficients r_y1gorrito,zk, k = 1, 2. Interpret y1gorrito.
py1x1_R = (eigen_vec_R[1,1]*sqrt(eigen_val_R[1]))/(sqrt(S[1,1]))
py1x2_R = (eigen_vec_R[1,2]*sqrt(eigen_val_R[1]))/(sqrt(S[2,2]))

correlationsR = data.frame(py1x1_R, py1x2_R)
correlationsR

# y1gorrito indica que por cada unidad de x1 que aumente, y1 crece a un nivel de 0.01 
# en el mismo sentido de x1. Por otro lado, con cada unidad de x2 que aumente, y1 
# aumenta en -0.179 en sentido inverso a x1.


# d. Compare the components obtained in Part a with those obtained in Exercise 3(a) 
#    do you feel that it is better to determine principal components from the sample 
#    covariance matrix or sample correlation matrix? Explain.

"""
Los cp de la matriz de covarianza varían ligeramente con respecto a los de la matriz 
de correlación. Al comparar las proporciones que explica la primera componente, se 
puede ver que la de la matriz de covarianza explica de mejor manera la varianza (0.998), 
y que con la matriz de correlación baja (0.843). Por lo anterior al tener estandarizar 
la matriz, se está perdiendo información, y por lo tanto es mejor no estandarizarla 
en este caso.
"""



# PUNTO 5

# The weekly rates of return for five stocks listed on the New York Stock Exchange
# are given in Table 1 (stock data.DAT).
stock_data = read.table("/home/laura/Desktop/univ/univ2022-2/AEDmrda/talleres/T5/stock_data.DAT.txt")
summary(stock_data)

# a. Construct the sample covariance matrix S, and find the sample principal 
#    components. (Note that the sample mean vector x is displayed in Example 8.5.)
covData = cov(stock_data)

eigen_vec_Data = eigen(covData)$vectors
eigen_val_Data = eigen(covData)$values

eigen_vec_Data


# b. Determine the proportion of the total sample variance explained by the first
#    three principal components. Interpret these components.
prop_pc1_Data = eigen_val_Data[1]/sum(eigen_val_Data)
prop_pc2_Data = eigen_val_Data[2]/sum(eigen_val_Data)
prop_pc3_Data = eigen_val_Data[3]/sum(eigen_val_Data)

proportions_Data = data.frame(prop_pc1_Data, prop_pc2_Data, prop_pc3_Data)
sum(proportions_Data)


# c. Given the results in Parts a-b, do you feel that the stock rates-of-return data
#    can be summarized in fewer than five dimensions? Explain.

# Dado que con los tres primeros componentes se explica un 89.88% de la varianza, 
# se considera que los datos podrían ser resumidos en tres dimensiones.



# PUNTO 6

# Consider the census-tract data listed in Table 2 (census tract.DAT). Suppose the
# observations on X_5 = median value home were recorded in ten thousands, rather than 
# hundred thousands, of dollars; that is, multiply all the numbers listed in the sixth
# column of the table by 10.
census_track = read.table("/home/laura/Desktop/univ/univ2022-2/AEDmrda/talleres/T5/census_track.DAT.txt")
summary(census_track)

# a. Construct the sample covariance matrix S for the census-tract data when 
#    X_5 = median value home is recorded in ten thousands of dollars.
census_track$V5 = census_track$V5*10
summary(census_track)

cov_census = cov(census_track)
cov_census


# b. Obtain the eigenvalue-eigenvector pairs and the first two sample principal
#    components for the covariance matrix in Part a.
eigen_vec_census = eigen(cov_census)$vectors
eigen_val_census = eigen(cov_census)$values

eigen_vec_census
eigen_val_census


# c. Compute the proportion of total variance explained by the first two principal
#    components obtained in Part b. Calculate the correlation coefficients, r_yi,xk,
#    and interpret these components if possible.
prop_pc1_census = eigen_val_census[1]/sum(eigen_val_census)
prop_pc2_census = eigen_val_census[2]/sum(eigen_val_census)

proportions_census = data.frame(prop_pc1_census ,prop_pc2_census)
sum(proportions_census)

prcomp(cov_census)

# En este caso, como los dos primeros componentes explican casi un 80% de la varianza 
# total, se puede considerar reducir la dimensionalidad a tamaño 2.

