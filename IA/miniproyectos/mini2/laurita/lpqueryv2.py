class LPQuery:

    '''
    Implementación de una base de conocimiento.
    Input:  base_conocimiento_lista, que es una lista de cláusulas definidas
                de la forma p1 Y ... Y pn > q
            cods, un objeto de clase Descriptor
    '''
    
    # por qué es buena idea usar el diccionario:
    # https://towardsdatascience.com/faster-lookups-in-python-1d7503e9cd38
    #

    def __init__(self, base_conocimiento_lista) :
        
        self.datos = {}  #diccionario de strings (codificaciones)
        self.reglas = {} #lista de cláusulas
        self.atomos = {} #lista de strings (codificaciones)
        
        #para todas las fórmulas en la base de conocimiento 
        #hago su respectiva descomposición en reglas, datos y átomos.
        for formula in base_conocimiento_lista:
            self.TELL(formula)

    def __str__(self) :
        cadena = 'Datos:\n'
        for dato in self.datos:
            cadena += dato + '\n'
        cadena += '\nReglas:\n'
        for regla in self.reglas:
            cadena += regla.nombre + '\n'
        return cadena

    def reglas_aplicables(self, head):
        
        #retorno la lista de reglas que me llevan al mismo objetivo
        #(recuerde que esto es un agente guiado por objetivos)
        return [r for r in self.reglas.keys() if r.cabeza == head]

    def test_objetivo(self, literal):
        
        #si cierta condición ya es un hecho (está en los datos)
        #entoneces se cumple el test objetivo para dicho literal.
        return literal in self.datos.keys()



    
    def TELL(self, formula):
        
        '''
            parece que tell añade una fórmula, o los hechos que la componen 
            a una lista de átomos.
        '''
        
        #obtiene el indice a partir del cual comienza el consecuente
        indice_conectivo = formula.find('>')
        
        #si hay precedente entonces creo una clausula para la formula
        #y la añado a las reglas. después para cada una de las proposiciones
        #del precedente las vuelvo un átomo y reviso si ya existe dicho átomo.
        #si el átomo no existe lo añado.
        
        #hago un proceso análogo para el consecuente: lo vuelvo un átomo, 
        #y si no está dentro de mis átomos lo añado.

        if indice_conectivo > 0:
            
            clausula = ClausulaDefinida(formula)
            self.reglas[clausula] = None
            
            for a in clausula.cuerpo:
            	if '-' in a:
            		atomo = a[1:]
            	else:
                	atomo = a
                    
                #añade el átomo si no está
            	if atomo not in self.atomos.keys():
            		self.atomos[atomo] = None
                    
            if '-' in clausula.cabeza:
            	atomo = clausula.cabeza[1:]
            else:
            	atomo = clausula.cabeza
            
            #añade el atomo
            if atomo not in self.atomos.keys():
            	self.atomos[atomo] = None
                
        # si no hay precedente (pido que se haga una acción)
        else:
            for literal in formula.split('Y'):
                if literal not in self.datos:
                    self.datos[literal] = None
                    
                    if '-' in literal:
                    	atomo = literal[1:]
                    else:
                    	atomo = literal
                    
                    #añade el atomo
                    if literal not in self.atomos.keys():
                    	self.atomos[atomo] = None
    
