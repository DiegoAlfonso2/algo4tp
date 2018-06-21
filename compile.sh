#!/bin/bash
cobc -x tp.clp -o target/tp.exe
cobc -c updprodven.clp
cobc -c -x testupdprodven.clp
cobc -c -x tp2.clp
cobc -x -o target/test.exe testupdprodven.o updprodven.o
cobc -x -o target/tp2.exe tp2.o updprodven.o
rm testupdprodven.o
rm updprodven.o
rm tp2.o
