
%%
clear
syms c1 c2 t
R=2000
C=2*(10^(-7))
L=1.5

x=[-R./L,-1./L,;
     1./C,0]
[vec,val]=eig(x)

dec = C^.2 * R^2 - 4*L*C

if (dec < 0){
    }

%% NOTACION INICIAL

%fundamental set of solutions
x1= [vec(1,1);vec(1,2)].*exp(val(1,1)*t)
x2= [vec(2,1);vec(2,2)].*exp(val(2,2)*t)

%% NOTACION QUE SE USA

x11= [vec(1,1);vec(1,2)].*exp(real(val(1,1))*t).*(cos(imag(val(1,1))*t)+i*sin(imag(val(1,1))*t))

%% PARTE REAL Y PARTE IMAGINARIA

u1=[real(x11(1));real(x11(2))]
v1=[imag(x11(1));imag(x11(2))]

a(t) = c1*u1(1) + c2*v1(1)
b(t) = c1*u1(2) + c2*v1(2)

%% SOLUCIONAR Y ENCONTRAR C1 Y C2 

equ= [a(0)==0,b(0)==10]
S = solve(equ,[c1,c2])

sprintf('C1 = %f.    C2 = %f.',S.c1,S.c2)

