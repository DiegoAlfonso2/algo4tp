#!/bin/bash
cobc -x tp.clp -o target/tp.exe
cobc -c updprodven.clp
cobc -c -x testupdprodven.clp
cobc -x -o target/test.exe testupdprodven.o updprodven.o
rm testupdprodven.o
rm updprodven.o