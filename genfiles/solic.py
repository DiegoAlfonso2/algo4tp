#!/usr/bin/python
import random
import sys

fechas = ['2018-04-22','2018-04-23','2018-04-24']

def generar_solicitudes(cant_solicitudes=500, filename="SOLIC.TXT"):
  random.seed()
  print "Generando archivo secuencial SOLIC con " + str(cant_solicitudes) + " solicitudes"
  with open(filename, "w") as f:  
    for solicitud in xrange(1,cant_solicitudes + 1):
      cod_vendedor = "{0:0>3}".format(random.randint(1, 1000))
      for nro_fecha in xrange(0,len(fechas)):
        cant_repeticiones_por_fecha = random.randint(2, 12)
        for producto in random.sample(range(1,10000), cant_repeticiones_por_fecha):
          cod_solicitud = "{0:0>6}".format(solicitud)
          fecha = fechas[nro_fecha]
          cod_prod = "{0:0>4}".format(producto)
          cantidad = "{0:0>4}".format(random.randint(1, 5001))
          importe = "{0:0>9}".format(random.randint(1, 5001)*100)
          f.write(cod_solicitud + fecha + cod_prod + cantidad + cod_vendedor + importe + '\n')
  
if __name__ == "__main__":
    generar_solicitudes(int(sys.argv[1]) if len(sys.argv) > 1 else 500)