#!/bin/bash

# Compila el programa principal
# -Fuinterfaces indica dónde buscar las unidades adicionales
fpc -Fuinterfaces main.pas;

# Ejecuta el programa compilado
./main;

# Elimina archivos intermedios generados por la compilación
rm *.o;           
rm -r main;       

# Limpieza de la carpeta interfaces
rm interfaces/*.o;    
rm interfaces/*.ppu;  