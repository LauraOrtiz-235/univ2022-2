import numpy as np
import math as m


V = 0
R = 2000
C = 2*(10)**(-7)
L = 1.5

A = np.array([[-R/L, -1/L],
              [1/C, 0]])

#A = [[-R/L, -1/L],[1/C, 0]]

autovalor, autovettor = np.linalg.eig(A)

print(autovalor, "\n")
print(autovettor)


#y1 = autovalor * m.exp(autovalor)


#print(autovalor[0].real)


def solu_compleja(t):

    x = m.cos((autovalor[0]).real*t)
    y = m.sin((autovalor[0]).real*t)
    y = autovettor[0] * m.exp((autovalor[0]).real*t) * complex(x,y)
    return y
    

solu_compleja(5)
