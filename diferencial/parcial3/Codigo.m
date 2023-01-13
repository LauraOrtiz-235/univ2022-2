clear
syms c1 c2 t A B a b

% Declaración de los parámetros
Vt = 0

R = 1.2
%C > 4*R.^2*L
C = 100
L = 4.7

raiz = ((R./L).^2 - (4.*(1./(C.*L))))

% Matriz del sistema
x = [-R./L, -1./L;
    1./C, 0]

[eigenvec,eigenval] = eig(x)


%% CASOS POR RAICES %%

%% Homogéneo

%%% Si las raices son reales %%%
if (raiz > 0) && (Vt == 0)

    % Conjunto fundamental de soluciones
    x1 = [eigenvec(1,1);eigenvec(2,1)].*exp(eigenval(1,1)*t)
    x2 = [eigenvec(1,2);eigenvec(2,2)].*exp(eigenval(2,2)*t)

    % Las soluciones
    iSol(t) = c1*x1(1) + c2*x2(1)
    VcSol(t) = c1*x1(2) + c2*x2(2)

    % Condiciones iniciales
    ic = [iSol(0)==0, VcSol(0)==10]

    % Solución de las variables C1 Y C2
    solu = solve(ic, [c1,c2])

    sprintf('La solución de C1 es: %f', solu.c1)
    sprintf('La solución de C2 es: %f', solu.c2)

    % SOLUCIÓN GENERAL %
    fprintf('La solución al sistema es: %f\n')
    i(t) = solu.c1*x1(1) + solu.c2*x2(1)
    Vc(t) = solu.c1*x1(2) + solu.c2*x2(2)

    fprintf('La solución al sistema es: \n')
    pretty(i(t))
    pretty(Vc(t))

    % GRÁFICA
    fplot(i(t))
    hold on
    fplot(Vc(t))
    grid on
    legend('i(t)', 'Vc(t)')
    title('Gráfica Vc(t) e i(t) - Raices reales diferentes en sistema Homogéneo')
    hold off


%%% Raices repetidas %%%
elseif (raiz == 0) && (Vt == 0)

    % Se define la matriz n, que nos ayuda a encontrar la segunda
    % solución para el segundo vector propio
    n = [0; eigenvec(1,1)]

    % Conjunto fundamental de soluciones
    x1 = [eigenvec(1,1);eigenvec(2,1)].*exp(eigenval(1,1)*t)
    x2 = (([eigenvec(1,2);eigenvec(2,2)].*exp(eigenval(1,1)*t)) + exp(eigenval(2,2)*t).*n)

    % Las soluciones
    iSol(t) = c1*x1(1) + c2*x2(1)
    VcSol(t) = c1*x1(2) + c2*x2(2)

    % Condiciones iniciales
    ic = [iSol(0)==0, VcSol(0)==10]

    % Solución de las variables C1 Y C2
    solu = solve(ic, [c1,c2])

    sprintf('La solución de C1 es: %f', solu.c1)
    sprintf('La solución de C2 es: %f', solu.c2)

    % SOLUCIÓN GENERAL %
    fprintf('La solución al sistema es: %f\n')
    i(t) = solu.c1*x1(1) + solu.c2*x2(1)
    Vc(t)= solu.c1*x1(2) + solu.c2*x2(2)

    fprintf('La solución al sistema es: \n')
    pretty(i(t))
    pretty(Vc(t))

    % GRÁFICA
    fplot(i(t))
    hold on
    fplot(Vc(t))
    grid on
    %legend('i(t)', 'Vc(t)')
    title('Gráfica Vc(t) e i(t) - Raices repetidas en sistema Homogéneo')
    hold off


%%% Si las raices son complejas %%%
elseif (raiz < 0) && (Vt == 0)

    % Conjunto fundamental de soluciones
    x1= [eigenvec(1,1);eigenvec(2,1)].*exp(eigenval(1,1)*t)
    x2= [eigenvec(1,2);eigenvec(2,2)].*exp(eigenval(2,2)*t)

    % Notación
    solx= [eigenvec(1,1);eigenvec(2,1)].*exp(real(eigenval(1,1))*t).*(cos(imag(eigenval(1,1))*t) + i*sin(imag(eigenval(1,1))*t))

    % Parte real
    u1=[real(solx(1));
        real(solx(2))]
    % Parte Imaginaria
    u2=[imag(solx(1));
        imag(solx(2))]

    % Las soluciones
    iSol(t) = c1*u1(1) + c2*u2(1)
    VcSol(t) = c1*u1(2) + c2*u2(2)

    % Condiciones iniciales
    ic = [iSol(0)==0, VcSol(0)==10]

    % Solución de las variables C1 Y C2
    solu = solve(ic, [c1,c2])

    sprintf('La solución de C1 es: %f', solu.c1)
    sprintf('La solución de C2 es: %f', solu.c2)

    % SOLUCIÓN GENERAL %
    i(t) = solu.c1*u1(1) + solu.c2*u2(1)
    Vc(t) = solu.c1*u1(2) + solu.c2*u2(2)

    fprintf('La solución al sistema es: \n')
    pretty(i(t))
    pretty(Vc(t))

    % GRÁFICA
    fplot(i(t))
    hold on
    fplot(Vc(t))
    grid on
    legend('i(t)', 'Vc(t)')
    title('Gráfica Vc(t) e i(t) - Raices complejas en sistema Homogéneo')
    hold off


%% NO Homogéneo

%%% Raices reales %%%
elseif (raiz > 0) && (Vt ~= 0)

    % Matrix g(t) - No homogenea
    g = [Vt./L;
         0]

    % Solución Homogénea
    x1 = [eigenvec(1,1);eigenvec(2,1)].*exp(eigenval(1,1)*t)
    x2 = [eigenvec(1,2);eigenvec(2,2)].*exp(eigenval(2,2)*t)

    % Matriz de coeficientes indeterminados
    p = [A;B]
    a = x*p + g

    % Solución de los coeficientes indeterminados
    Sp = solve(a, p)

    solA = Sp.B
    solB = Sp.A

    % Solución general
    iSol(t) = c1*x1(1) + c2*x2(1) + solA
    VcSol(t) = c1*x1(2) + c2*x2(2) + solB

    % Condiciones iniciales
    ic = [iSol(0)==0, VcSol(0)==10]

    % Solución de las variables C1 Y C2
    solu = solve(ic, [c1,c2])

    sprintf('La solución de C1 es: %f', solu.c1)
    sprintf('La solución de C2 es: %f', solu.c2)

    % SOLUCIÓN GENERAL %
    i(t) = solu.c1*x1(1) + solu.c2*x2(1)
    Vc(t) = solu.c1*x1(2) + solu.c2*x2(2)

    fprintf('La solución al sistema es: \n')
    pretty(i(t))
    pretty(Vc(t))

    % GRÁFICA
    fplot(i(t))
    hold on
    fplot(Vc(t))
    grid on
    legend('i(t)', 'Vc(t)')
    title('Gráfica Vc(t) e i(t) - Raices reales en sistema No Homogéneo')
    hold off


%%% Raices repetidas %%%
elseif (raiz == 0) && (Vt ~= 0)

    % Se define la matriz n, que nos ayuda a encontrar la segunda
    % solución para el segundo vector propio
    n = [0; eigenvec(1,1)]

    % Matrix g(t) - No homogenea
    g = [Vt./L;
         0]

    % Conjunto fundamental de soluciones
    x1 = [eigenvec(1,1);eigenvec(2,1)].*exp(eigenval(1,1)*t)
    x2 = (([eigenvec(1,2);eigenvec(2,2)].*exp(eigenval(1,1)*t)) + exp(eigenval(2,2)*t).*n)

    % Matriz de coeficientes indeterminados
    p = [A;B]
    a = x*p + g

    % Solución de los coeficientes indeterminados
    Sp = solve(a, p)

    solA = Sp.B
    solB = Sp.A

    % Solución general
    iSol(t) = c1*x1(1) + c2*x2(1) + solA
    VcSol(t) = c1*x1(2) + c2*x2(2) + solB

    % Condiciones iniciales
    ic = [iSol(0)==0, VcSol(0)==10]

    % Solución de las variables C1 Y C2
    solu = solve(ic, [c1,c2])

    sprintf('La solución de C1 es: %f', solu.c1)
    sprintf('La solución de C2 es: %f', solu.c2)

    % SOLUCIÓN GENERAL %
    i(t) = solu.c1*x1(1) + solu.c2*x2(1)
    Vc(t)= solu.c1*x1(2) + solu.c2*x2(2)

    fprintf('La solución al sistema es: \n')
    pretty(i(t))
    pretty(Vc(t))

    % GRÁFICA
    fplot(i(t))
    hold on
    fplot(Vc(t))
    grid on
    %legend('i(t)', 'Vc(t)')
    title('Gráfica Vc(t) e i(t) - Raices repetidas en sistema No Homogéneo')
    hold off


%%% Raices complejas %%%
elseif (raiz < 0) && (Vt ~= 0)

    % Matrix g(t) - No homogenea
    g = [Vt./L;
         0]

    % Solución Homogénea
    solx = [eigenvec(1,1);eigenvec(2,1)].*exp(real(eigenval(1,1))*t).*(cos(imag(eigenval(1,1))*t) + i*sin(imag(eigenval(1,1))*t))

    % Parte real
    u1=[real(solx(1));
        real(solx(2))]
    % Parte Imaginaria
    u2=[imag(solx(1));
        imag(solx(2))]

    % Matriz de coeficientes indeterminados
    p = [A;B]
    a = x*p + g

    % Solución de los coeficientes indeterminados
    Sp = solve(a, p)

    solA = Sp.B
    solB = Sp.A

    % Solución general
    iSol(t) = c1*u1(1) + c2*u2(1) + solA
    VcSol(t) = c1*u1(2) + c2*u2(2) + solB

    % Condiciones iniciales
    ic = [iSol(0)==0, VcSol(0)==10]

    % Solución de las variables C1 Y C2
    solu = solve(ic, [c1,c2])

    sprintf('La solución de C1 es: %f', solu.c1)
    sprintf('La solución de C2 es: %f', solu.c2)

    % SOLUCIÓN GENERAL %
    i(t) = solu.c1*u1(1) + solu.c2*u2(1)
    Vc(t) = solu.c1*u1(2) + solu.c2*u2(2)

    fprintf('La solución al sistema es: \n')
    pretty(i(t))
    pretty(Vc(t))

    % GRÁFICA
    fplot(i(t))
    hold on
    fplot(Vc(t))
    grid on
    legend('i(t)', 'Vc(t)')
    title('Gráfica Vc(t) e i(t) - Raices complejas en sistema No Homogéneo')
    hold off

end
