#!/usr/bin/python
import random
import sys

def generar_prod(cant_productos=2000, filename="PROD.TXT"):
  random.seed()
  print "Generando archivo PROD con " + str(cant_productos) + " productos"
  codigos_disponibles = map(lambda x: "{0:04}".format(x),range(1, cant_productos + 1))
  fecha_alta = "20180401"
  with open(filename, "w") as f:
    for codigo in random.sample(codigos_disponibles, cant_productos):
      f.write("{0}{1}{2:<15}".format(codigo, fecha_alta, "PRODUCTO N." + codigo))
  
if __name__ == "__main__":
    generar_prod(int(sys.argv[1]) if len(sys.argv) > 1 else 2000)