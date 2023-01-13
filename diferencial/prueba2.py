import numpy as np 
import matplotlib.pyplot as plt
from scipy import integrate


V = 0
R = 2000
C = 2*(10)**(-7)
L = 1.5


def f(q,t):
    Vc, i = q
    return [-(R/L)*i - (1/L)*Vc + V/L, 1/C*i]


f0 = [0, 10]

t = np.linspace(0, 2500, 50)

sol = integrate.odeint(f, f0, t)
print(sol)

fig, axes = plt.subplots()

axes.plot(t, sol[:, 1], 'g', label='i(t)')
axes.plot(t, sol[:, 0], 'b', label='Vc(t)')
axes.legend()

plt.grid()
plt.title("Gráfica Vc(t) e i(t)")

plt.xlim([-200, 150])
plt.ylim([-200, 150])

plt.show()
