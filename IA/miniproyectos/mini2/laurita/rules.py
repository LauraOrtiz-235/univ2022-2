from logica import *


def unicidad_por_casillas(n: int, descriptor: Descriptor) -> list:
    
    """    
    Input:
        n = tamaño del tablero
        descriptor = clase encargada decodificar los átomos
    Output:
        reglas = lista de reglas
    """

    reglas = []

    for x in range(n):
        for y in range(n):
            for i in range(9):
                for j in range(9):
                    if i != j:
                        reglas += [f'{descriptor(x, y, i)}>-{descriptor(x, y, j)}']
    
    return set(reglas)



