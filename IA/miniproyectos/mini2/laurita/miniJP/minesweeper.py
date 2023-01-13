import numpy as np
import random as rnd
from typing import Tuple, List
from logica import Descriptor



def truncar(val: int, lim: int):
    """
    Función que delimita las coordenadas a la designada por el jugador.
    --------------------------------------------------------------------------
    Keyword Arguments:
    val:int -- valor que se desea truncar.
    lim:int -- límite con el cual se desea truncar.
    --------------------------------------------------------------------------
    """

    if val < 0:
        return 0
    elif val > lim - 1:
        return lim - 1
    else:
        return val


def adyacentes(coord: Tuple[int, int], dims: Tuple[int, int]) -> List[Tuple[int, int]]:
    """
    Funcion que retorna todos los puntos adyacentes a las coordenadas 'coord',
    en un espacio de dimensión 'dims'.
    --------------------------------------------------------------------------
    Keyword Arguments:
    coord:Tuple[int, int]   -- Tupla con valores 'x' y 'y' de la coordenada
                               deseada.
    dims:Tuple[int, int]    -- Dimensiones del tablero en el que se está
                               jugando.
    --------------------------------------------------------------------------
    Posibles casillas adyacentes para la casilla marcada como 'O':
    XXX
    XOX
    XXX
    """
    x, y = coord
    lx, ly = dims
    ady = {
        (truncar(x - 1, lx), truncar(y - 1, ly)),
        (x, truncar(y - 1, ly)),
        (truncar(x + 1, lx), truncar(y - 1, ly)),
        (truncar(x - 1, lx), y),
        (truncar(x + 1, lx), y),
        (truncar(x - 1, lx), truncar(y + 1, ly)),
        (x, truncar(y + 1, ly)),
        (truncar(x + 1, lx), truncar(y + 1, ly)),
    }
    return sorted(list(ady))


def codeStrMina(val: int) -> str:
    """
    Esta función cumple con la función de traducir los caracteres utilizados
    para caracterizar los estados de las casillas,
    --------------------------------------------------------------------------
    """
    if val >= 0 and val < 9:
        return f"{val}    "
    elif val == 9:
        return "💣"
    elif val == 10:
        return "⚑"
    elif val == 11:
        return "■"


class MineSweeper:
    """Esta clase funciona como interfaz para el desarrollo de experimentos
    relacionados con el juego de buscaminas."""

    def __init__(self, x:int, y:int, m:int, d:Descriptor):

        self.desc = d

        # Para correr las iteraciones y tener un método para parar.
        self.jugando = True

        # 'x' cantidad de columnas y 'y' cantidad de filas.
        self.x = x
        self.y = y

        # Estado del juego:
        # 0 - 8 : Cantidad de minas adyacentes
        #     9 : Mina confirmada
        #    10 : Mina ha sido marcada
        #    11 : Casilla desconocida

        self.display = np.full((x, y), 11)
        self.background = np.zeros((x, y))

        # Minas:
        self.minas_num = m
        self.minas_mar = 0
        self.minas_ubi = []

        # Marcas
        self.marcas = self.minas_num

        # Ubicar minas y num. adyacentes.
        for m in range(self.minas_num):
            # Ubica aleatoriamente cada una de las minas
            mx, my = (rnd.randint(0, x - 1), rnd.randint(0, y - 1))

            # Procura que las minas esten ubicadas en distintas posiciones.
            while (mx, my) in self.minas_ubi:
                mx, my = (rnd.randint(0, x - 1), rnd.randint(0, y - 1))

            # Pone la mina en el mapa vacio
            self.background[mx, my] = 10
            self.minas_ubi.append((mx, my))
            adys = adyacentes((mx, my), (x, y))

            # Segun las cada una de las minas modificar las adyacentes.
            for coord in adys:
                if self.background[coord] != 10:
                    self.background[coord] += 1


    def pintar_frente(self) -> None:
        """
        Pinta bonito el estado actual del juego (self.display).
        --------------------------------------------------------------------------
        """
        s = "$   "
        for i in range(self.x):
            s += f"{i:<5}"
        s += "\n"
        for i in range(self.x):
            s += f"{i:<4}"
            for j in range(self.y):
                s += f"{codeStrMina(self.display[(i,j)]):<4}"
            s += "\n"

        print(s)


    def pintar_atras(self) -> None:
        """
        Pinta bonito como está conformado el juego (self.background).
        --------------------------------------------------------------------------
        """
        s = "$   "
        for i in range(self.x):
            s += f"{i:<5}"
        s += "\n"
        for i in range(self.x):
            s += f"{i:<4}"
            for j in range(self.y):
                s += f"{codeStrMina(self.background[(i,j)]):<4}"
            s += "\n"

        print(s)


    def transicion(self, target: Tuple[int, int], accion: int = 0):
        """
        Según una posición y una acción, cambia el estado del juego
        (self.display).
        --------------------------------------------------------------------------
        Keyword Arguments:
        target:Tuple[int,int]   -- La coordenada que va a ser afectada.
        accion:int (default 0)  -- Depende de la acción suministrada:
                                    0 : Revelar lo que hay en el self.background.
                                    1 : Marcar mina.
        --------------------------------------------------------------------------
        """

        if self.jugando:

            if accion == 0:
                estado = self.background[target]
                self.display[target] = estado

                # Si lo que revela es una mina
                if estado == 9:
                    print("BOOOOOOM 💣")
                    self.jugando = False

            elif accion == 1:
                if self.marcas > 0:
                    self.estado[target] = 10
                else:
                    print("Ups... no hay más banderas 😞")
                    self.jugando = False

        else:
            print("WAT!... Pero si no estas jugando.")

        return self.background


    def objetivo(self) -> bool:
        """
        Función que revisa si todas las minas han sido marcadas, cuando todas lo
        han sido, se gana (termina) el juego.
        --------------------------------------------------------------------------
        """
        for m in self.minas_ubi:
            if self.display[m] != 10:
                return False
        return True

    def estado(self, target: Tuple[int, int]) -> int:
        """
        Retorna el estado en el self.display.
        --------------------------------------------------------------------------
        Keyword Arguments:
        target:Tuple[int,int]   -- La coordenada que va a ser afectada.
        --------------------------------------------------------------------------
        """
        return self.display[target]
