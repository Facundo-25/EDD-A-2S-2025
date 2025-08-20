#!/bin/bash

# Compilar el código
fpc -Fuinterfaces main.pas

# Ejecutar
./main


# Eliminar los archivos generados para compilar
rm *.o
rm main

# Eliminar los archivos generados en la carpeta interfaces
rm interfaces/*.o;    
rm interfaces/*.ppu;  