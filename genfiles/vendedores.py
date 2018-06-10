#!/usr/bin/python
import random
import sys

apellidos = ["Gonzalez","Rodriguez","Gomez","Huertas","Lopez","Diaz",
             "Martinez","Perez","Garcia","Sanchez","Romero","Sosa",
             "alvarez","Torres","Ruiz","Ramirez","Flores","Acosta",
             "Benitez","Medina"]

nombres = ["Sofia","Maria","Lucia","Martina","Catalina","Elena",
           "Emilia","Valentina","Paula","Zoe","Santiago","Mateo",
           "Juan","Matias","Nicolas","Benjamin","Pedro","Tomas",
           "Thiago","Santino"]

categorias = ["A", "B", "C"]

def generar_vendedores(cant_vendedores=900, filename="VENDEDORES.TXT"):
  random.seed()
  print "Generando archivo secuencial VENDEDORES con " + str(cant_vendedores) + " vendedores"
  fecha_alta = "20180401"
  direccion = "PASEO COLON 850"
  with open(filename, "w") as f:  
    for codigo in xrange(1,cant_vendedores + 1):
      telefono = "114" + str(random.randint(1000000,9999999))
      apellido = apellidos[random.randint(0, len(apellidos) - 1)]
      nombre = nombres[random.randint(0, len(nombres) - 1)]
      apeynom = apellido + " " + nombre 
      categoria = categorias[random.randint(0, len(categorias) - 1)]
      f.write("{0:0>3}{1}{2:<20}{3:<25}{4}{5}".format(codigo, fecha_alta, direccion, apeynom, telefono, categoria))
  
if __name__ == "__main__":
    generar_vendedores(int(sys.argv[1]) if len(sys.argv) > 1 else 900)