#!/usr/bin/python
import random
import sys

fechas = ['2018-04-22','2018-04-23','2018-04-24']

def generar_prod_vend(cant_productos=500, filename="PROD-VEND.TXT"):
  random.seed()
  print "Generando archivo secuencial PROD-VEND con " + str(cant_productos) + " registros"
  with open(filename, "w") as f:  
    for producto in random.sample(range(1,10000), cant_productos):
      cod_prod = "{0:0>4}".format(producto)
      fecha = fechas[random.randint(0,len(fechas) -1)]
      cantidad = "{0:0>4}".format(random.randint(1, 5001))
      importe = "{0:0>9}".format(random.randint(1, 5001)*100)
      f.write(cod_prod + fecha + cantidad + importe + '\n')
  
if __name__ == "__main__":
    generar_prod_vend(int(sys.argv[1]) if len(sys.argv) > 1 else 500)