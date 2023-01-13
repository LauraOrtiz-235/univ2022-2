

# sensor:

# acciones:

# - Medidas. las medidas son imperfectas, tienen errores.
# entonces tengo que hacer mi código un poco aprueba de esos errores.

# ¿cuáles son los errores posibles?

# 1. que lea mal, queda por fuera de un rango dado.
# 1.5 que haya ruido en el sensor.
# 2. que no detecte el sensor.

# - Tomar medidas.

# - Enviar medidas.


class sensor:

    def __init__(self,
                 pin_number: int, 
                 min_acceptable: float,
                 max_acceptable: float,
                 n_samples: int,
                 ):

        self.sensor     =  None
        self.min        = min_acceptable
        self.max        = max_acceptable
        self.n_samples  = n_samples



        pass

    def get_measurement(self):

        return 

    def get_valid_measurement(self):

        """
            returns a valid measurement.

            tries n_samples times to get a valid measurement.
            every time it gets a valid measurement, it adds it 
            to the list of measurements, and when done
            return the average of the measurements.
        """

        measurements = []

        for i in range(self.n_samples):
            measurement = self.get_measurement()

            if self.watch_dog(measurement):
                measurements.append(measurement)

        if len(measurements) == 0:
            return -1

        return sum(measurements) / len(measurements)



    def send_measurement(self):

        return

    def watch_dog(self, value: float):

        """
        if value not between min and max, returns False
        else returns True.
        """

        if value < self.min or value > self.max:
            return False
        else:
            return True





