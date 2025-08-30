#!/bin/bash

# Compilar el código
fpc -Fuinterfaces -Fustructures -Fudata -Futools main.pas

# Ejecutar
./main


# Eliminar los archivos generados para compilar
rm *.o
rm main

# Eliminar los archivos generados en la carpeta interfaces
rm interfaces/*.o;    
rm interfaces/*.ppu;  

# Eliminar los archivos generados en la carpeta structures
rm structures/*.o;    
rm structures/*.ppu;  

# Eliminar los archivos generados en la carpeta tools
rm tools/*.o;
rm tools/*.ppu;

# Eliminar los archivos generados en la carpeta data
rm data/*.o;
rm data/*.ppu;