from logica import *
from minesweeper import *
from typing import Tuple, List
from itertools import combinations
from time import sleep
#from IPython.display import clear_outputrom 
from random import randint



def formulas_unicidad(J: MineSweeper) -> List[str]:
    """
    Retorna formulas relacionadas con la unicidad de la casilla, e.i. si ya
    tiene una propiedad, ya no puede tener otra.
    --------------------------------------------------------------------------
    Keyword Arguments:
    J:MineSweeper -- Juego actual.
    --------------------------------------------------------------------------
    """
    return list(
        set(
            [
                f"{J.desc.P([xi,yi,i])}>-{J.desc.P([xi,yi,j])}"
                for xi in range(J.x)
                for yi in range(J.y)
                for i in range(12)
                for j in range(12)
                if j != i
            ]
        )
    )


def formulas_perim_no_minas(J: MineSweeper) -> List[str]:
    """
    Esta función retorna todas las formulas que pueden ayudar a saber donde no
    hay minas.
    --------------------------------------------------------------------------
    Keyword Arguments:
    J:MineSweeper -- Juego actual.
    --------------------------------------------------------------------------
    """
    formulas = []

    for x in range(J.x):
        for y in range(J.y):
            Adyacentes = adyacentes((x, y), (J.x, J.y))
            # Caso en el que la posición no tiene minas adyacentes.
            formulas += [
                f"{J.desc.P([x,y,0])}>-{J.desc.P([ad[0],ad[1],9])}" for ad in Adyacentes
            ]

            # Caso en los que tiene de 1-8 minas adyacentes 💣
            for caso in range(1, 9):
                # Evalua todas las posibles formas en las que las minas podrian
                # estar ubicadas, teniendo en cuenta la cantidad de adyacentes.
                combinaciones = combinations(Adyacentes, caso)

                for posibilidad in combinaciones:

                    # Todos los posibles casos en donde no hay bombas a razón
                    # del caso actualmente a consideración.
                    a = [
                        f"Y-{J.desc.P([p[0],p[1],9])}"
                        for p in Adyacentes
                        if p not in posibilidad
                    ]
                    # Caso actualmente a consideración.
                    p = f"{J.desc.P([x,y,caso])}{''.join(a)}"
                    # Posibles consecuencias.
                    q = [f"{J.desc.P([p[0],p[1],9])}" for p in posibilidad]
                    # for all q_i in q : p -> q
                    formulas += [f"{p}>{qi}" for qi in q]

    return list(set(formulas))


def formulas_perim_minas(J: MineSweeper) -> List[str]:
    """
    Esta función retorna todas las formulas que pueden ayudar a saber donde hay
    minas.
    --------------------------------------------------------------------------
    Keyword Arguments:
    J:MineSweeper -- Juego actual.
    --------------------------------------------------------------------------
    """
    formulas = []

    for x in range(J.x):
        for y in range(J.y):
            Adyacentes = adyacentes((x, y), (J.x, J.y))

            # Caso en los que tiene de 1-8 minas adyacentes 💣
            for caso in range(1, 9):
                # Evalua todas las posibles formas en las que las minas podrian
                # estar ubicadas, teniendo en cuenta la cantidad de adyacentes.
                combinaciones = combinations(Adyacentes, caso)
                for posibilidad in combinaciones:

                    # Todos los posibles estados en los que puede estar de forma
                    # conjunta al caso actualmente a consideración.
                    a = [f"Y{J.desc.P([p[0],p[1],9])}" for p in posibilidad]
                    # Caso actualmente a consideración.
                    p = f"{J.desc.P([x,y,caso])}{''.join(a)}"
                    # Consecuencias descartadas.
                    q = [
                        f"{J.desc.P([p[0],p[1],9])}"
                        for p in Adyacentes
                        if p not in posibilidad
                    ]
                    # for all q_i in q : p -> q
                    formulas += [f"{p}>{qi}" for qi in q]

    return list(set(formulas))


def GameOn(J: MineSweeper, B: LPQuery, P: bool = True) -> MineSweeper:
    """
    Corre el MineSweeper junto con la LPQuery hasta obtener una victoria o una
    derrota.
    --------------------------------------------------------------------------
    Keyword Arguments:
    J:MineSweeper   -- Juego actual.
    B:LPQuery       -- Base de conocimiento.
    P:bool          -- Si es verdadera, imprime todo. De ser falsa, solo retorna
                       resultados.
    --------------------------------------------------------------------------
    """
    # Comienza en esquina pues en otras posiciones retorna tanta información que
    # deja de se útil.
    x, y = (0, 0)
    J.transicion((x, y))
    if P:
        J.pintar_frente()

    B.TELL(J.desc.P([x, y, J.estado((x, y))]))
    while J.jugando:
        cambios = False
        # Inicia busqueda casilla por casilla
        for i in range(J.x):
            for j in range(J.y):

                x, y = (i, j)
                # Analisa las casillas que aún no han sido marcadas.
                if J.estado((x, y)) == 11:
                    if P:
                        print(f"Inicia ({x},{y})")
                    # Pregunta si hay una mina confirmada, para poder marcarla.
                    if ASK(objetivo=J.desc.P([x, y, 9]), valor=True, base=B):
                        J.transicion((x,y),1)
                        cambios = True
                        if P:
                            print( f"Se encontró 💣 en ({x},{y})... tranqui, ya la marcamos!" )
                            J.pintar_frente()

                    # Pregunta si con certesa no hay bombas, para así poder
                    # destapar el cuadro.
                    elif ASK(objetivo=f"-{J.desc.P([x, y, 9])}", valor=True, base=B):
                        J.transicion((x,y))
                        B.TELL(J.desc.P([x, y, J.estado((x, y))]))
                        cambios = True
                        if P:
                            print( f"({x},{y}) Está libre de minas, se puede descubrir!" )
                            J.pintar_frente()

        # ================= Recorio todos las casilla desconocidas.

        if J.objetivo():
            if P:
                print( f"Felicitaciones, ha ganado!!!" )
            return B

        # Buscar una solución aleatoria a falta de oportunidades!!!
        elif not cambios:
            x,y = (randint(0,J.x-1), randint(0,J.y-1) )

            while J.estado((x,y)) != 11:
                x,y = (randint(0,J.x-1), randint(0,J.y-1) )

            J.transicion((x,y))
            B.TELL(J.desc.P([x, y, J.estado((x, y))]))
            if P:
                print( f"A la suerte en ({x},{y})!!!" )
                J.pintar_frente()
    if P:
        J.pintar_frente()
    return B





# Tamaño
x, y = (3, 3)
m = 1

informacion = [
    "tiene 0 minas adyacentes en",
    "tiene 1 mina adyacente en",
    "tiene 2 minas adyacentes en",
    "tiene 3 minas adyacentes en",
    "tiene 4 minas adyacentes en",
    "tiene 5 minas adyacentes en",
    "tiene 6 minas adyacentes en",
    "tiene 7 minas adyacentes en",
    "tiene 8 minas adyacentes en",
    "tiene mina en",
    "ha sido marcada en",
    "es desconocida en",
]
info = len(informacion)
filas = [i for i in range(x)]
columnas = [j for j in range(y)]

D = Descriptor([x, y, info])
J = MineSweeper(x, y, m, D)
B = LPQuery(
    list(
        set(formulas_unicidad(J) + formulas_perim_no_minas(J) + formulas_perim_minas(J))
    )
)

GAME = GameOn(J, B)


# Prueba formulas
# print(formulas_unicidad(J))
# print(formulas_perim_no_minas(J))
# print(formulas_perim_minas(J))


# Estado del juego:
# 0 - 8 : Cantidad de minas adyacentes
#     9 : Mina confirmada
#    10 : Mina ha sido marcada
#    11 : Casilla desconocida
