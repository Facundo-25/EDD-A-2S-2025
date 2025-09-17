program ArbolB;

// Incluye SysUtils para manejo de archivos y cadenas
uses
  SysUtils;

const
  ORDEN = 5;          // Orden del árbol B (máximo 4 claves por nodo)
  MAX_CLAVES = ORDEN - 1; // Máximo de claves por nodo (4)
  MIN_CLAVES = ORDEN div 2 - 1; // Mínimo de claves por nodo (2)

type
  // Define un nodo del árbol B
  PNodoB = ^TNodoB;
  TNodoB = record
    n: Integer;                   // Número de claves en el nodo
    claves: array[0..MAX_CLAVES-1] of Integer; // Arreglo de claves (máximo 4)
    valores: array[0..MAX_CLAVES-1] of String; // Arreglo de valores asociados
    hijos: array[0..ORDEN-1] of PNodoB; // Arreglo de punteros a hijos (máximo 5)
    esHoja: Boolean;             // Indica si el nodo es hoja
  end;

var
  raiz: PNodoB = nil; // Raíz del árbol B, inicialmente vacía

// Crea un nuevo nodo B
function CrearNodoB(esHoja: Boolean): PNodoB;
var
  nuevo: PNodoB; // Puntero al nuevo nodo
  i: Integer;    // Índice para inicializar hijos
begin
  New(nuevo);                    // Reserva memoria para el nodo
  nuevo^.n := 0;                 // Inicializa número de claves en 0
  nuevo^.esHoja := esHoja;       // Establece si es hoja
  for i := 0 to ORDEN-1 do       // Recorre arreglo de hijos
    nuevo^.hijos[i] := nil;      // Inicializa todos los hijos como nulos
  CrearNodoB := nuevo;           // Retorna el nuevo nodo
end;

// Busca una clave en el árbol B
function Buscar(nodo: PNodoB; clave: Integer): PNodoB;
var
  i: Integer; // Índice para recorrer claves
begin
  if nodo = nil then             // Si el nodo es nulo
    Buscar := nil                // Retorna nulo
  else
  begin
    i := 0;                      // Inicia índice en 0
    while (i < nodo^.n) and (clave > nodo^.claves[i]) do // Busca posición de la clave
      Inc(i);                    // Incrementa índice
    if (i < nodo^.n) and (clave = nodo^.claves[i]) then // Si encuentra la clave
      Buscar := nodo             // Retorna el nodo actual
    else
      Buscar := Buscar(nodo^.hijos[i], clave); // Busca en el hijo correspondiente
  end;
end;

// Actualiza el valor de una clave existente
procedure Actualizar(var raiz: PNodoB; clave: Integer; nuevoValor: String);
var
  nodo: PNodoB; // Nodo donde se encuentra la clave
  i: Integer;   // Índice para buscar la clave
begin
  nodo := Buscar(raiz, clave);   // Busca el nodo con la clave
  if nodo <> nil then            // Si se encuentra el nodo
  begin
    i := 0;                      // Inicia índice en 0
    while (i < nodo^.n) and (nodo^.claves[i] <> clave) do // Busca la clave exacta
      Inc(i);                    // Incrementa índice
    nodo^.valores[i] := nuevoValor; // Actualiza el valor
    Writeln('Valor actualizado para clave ', clave); // Confirma actualización
  end
  else
    Writeln('Clave ', clave, ' no encontrada'); // Clave no existe
end;

// Divide un nodo lleno durante la inserción
procedure DividirHijo(padre: PNodoB; i: Integer; hijo: PNodoB);
var
  nuevoNodo: PNodoB; // Nuevo nodo creado tras la división
  j: Integer;        // Índice para mover claves y hijos
  claveMedia: Integer; // Clave que sube al padre
  valorMedio: String;  // Valor asociado a la clave media
begin
  nuevoNodo := CrearNodoB(hijo^.esHoja); // Crea nuevo nodo (misma condición de hoja)
  nuevoNodo^.n := MIN_CLAVES;            // Establece número de claves en nuevo nodo
  for j := 0 to MIN_CLAVES-1 do          // Mueve claves al nuevo nodo
  begin
    nuevoNodo^.claves[j] := hijo^.claves[j + MIN_CLAVES + 1]; // Copia claves
    nuevoNodo^.valores[j] := hijo^.valores[j + MIN_CLAVES + 1]; // Copia valores
  end;
  if not hijo^.esHoja then               // Si no es hoja
    for j := 0 to MIN_CLAVES do          // Mueve hijos al nuevo nodo
      nuevoNodo^.hijos[j] := hijo^.hijos[j + MIN_CLAVES + 1]; // Copia hijos
  hijo^.n := MIN_CLAVES;                 // Reduce número de claves en hijo
  for j := padre^.n downto i + 1 do      // Desplaza claves en el padre
    padre^.claves[j] := padre^.claves[j-1]; // Mueve claves
  for j := padre^.n + 1 downto i + 2 do  // Desplaza hijos en el padre
    padre^.hijos[j] := padre^.hijos[j-1]; // Mueve hijos
  claveMedia := hijo^.claves[MIN_CLAVES]; // Obtiene clave media
  valorMedio := hijo^.valores[MIN_CLAVES]; // Obtiene valor medio
  padre^.claves[i] := claveMedia;        // Inserta clave media en padre
  padre^.valores[i] := valorMedio;       // Inserta valor medio en padre
  padre^.hijos[i + 1] := nuevoNodo;      // Conecta nuevo nodo comomeaning
  Inc(padre^.n);                         // Incrementa número de claves en padre
end;

// Inserta una clave y valor en un nodo no lleno
procedure InsertarNoLleno(nodo: PNodoB; clave: Integer; valor: String);
var
  i: Integer; // Índice para recorrer claves
begin
  i := nodo^.n - 1;                      // Inicia en la última clave
  if nodo^.esHoja then                   // Si el nodo es hoja
  begin
    while (i >= 0) and (clave < nodo^.claves[i]) do // Busca posición para insertar
    begin
      nodo^.claves[i + 1] := nodo^.claves[i]; // Desplaza claves
      nodo^.valores[i + 1] := nodo^.valores[i]; // Desplaza valores
      Dec(i);                               // Decrementa índice
    end;
    nodo^.claves[i + 1] := clave;         // Inserta nueva clave
    nodo^.valores[i + 1] := valor;        // Inserta nuevo valor
    Inc(nodo^.n);                         // Incrementa número de claves
  end
  else
  begin
    while (i >= 0) and (clave < nodo^.claves[i]) do // Busca hijo donde insertar
      Dec(i);                               // Decrementa índice
    Inc(i);                                 // Ajusta índice al hijo correcto
    if nodo^.hijos[i]^.n = MAX_CLAVES then // Si el hijo está lleno
    begin
      DividirHijo(nodo, i, nodo^.hijos[i]); // Divide el hijo
      if clave > nodo^.claves[i] then      // Ajusta índice si clave es mayor
        Inc(i);
    end;
    InsertarNoLleno(nodo^.hijos[i], clave, valor); // Inserta en el hijo
  end;
end;

// Inserta una clave y valor en el árbol B
procedure Insertar(var raiz: PNodoB; clave: Integer; valor: String);
var
  nuevaRaiz: PNodoB; // Nueva raíz si se divide la raíz
begin
  if raiz = nil then                     // Si el árbol está vacío
  begin
    raiz := CrearNodoB(True);            // Crea nueva raíz (hoja)
    raiz^.claves[0] := clave;            // Inserta clave
    raiz^.valores[0] := valor;           // Inserta valor
    raiz^.n := 1;                        // Establece número de claves
  end
  else
  begin
    if raiz^.n = MAX_CLAVES then         // Si la raíz está llena
    begin
      nuevaRaiz := CrearNodoB(False);    // Crea nueva raíz (no hoja)
      nuevaRaiz^.hijos[0] := raiz;       // Conecta raíz actual como hijo
      DividirHijo(nuevaRaiz, 0, raiz);   // Divide la raíz antigua
      InsertarNoLleno(nuevaRaiz, clave, valor); // Inserta en nueva raíz
      raiz := nuevaRaiz;                 // Actualiza la raíz
    end
    else
      InsertarNoLleno(raiz, clave, valor); // Inserta en nodo no lleno
  end;
end;

// Encuentra el predecesor de una clave
function EncontrarPredecesor(nodo: PNodoB; idx: Integer): PNodoB;
var
  actual: PNodoB; // Nodo actual
begin
  actual := nodo^.hijos[idx];            // Toma hijo izquierdo
  while not actual^.esHoja do            // Mientras no sea hoja
    actual := actual^.hijos[actual^.n];   // Va al hijo más a la derecha
  EncontrarPredecesor := actual;         // Retorna nodo con predecesor
end;

// Encuentra el sucesor de una clave
function EncontrarSucesor(nodo: PNodoB; idx: Integer): PNodoB;
var
  actual: PNodoB; // Nodo actual
begin
  actual := nodo^.hijos[idx + 1];        // Toma hijo derecho
  while not actual^.esHoja do            // Mientras no sea hoja
    actual := actual^.hijos[0];          // Va al hijo más a la izquierda
  EncontrarSucesor := actual;            // Retorna nodo con sucesor
end;

// Fusiona dos nodos hijos
procedure Fusionar(nodo: PNodoB; idx: Integer);
var
  hijo, hermano: PNodoB; // Nodo hijo y su hermano
  i: Integer;           // Índice para mover claves y hijos
begin
  hijo := nodo^.hijos[idx];              // Hijo en la posición idx
  hermano := nodo^.hijos[idx + 1];       // Hermano derecho
  hijo^.claves[hijo^.n] := nodo^.claves[idx]; // Mueve clave del padre
  hijo^.valores[hijo^.n] := nodo^.valores[idx]; // Mueve valor del padre
  Inc(hijo^.n);                         // Incrementa claves en hijo
  for i := 0 to hermano^.n - 1 do       // Copia claves del hermano
  begin
    hijo^.claves[hijo^.n + i] := hermano^.claves[i]; // Copia clave
    hijo^.valores[hijo^.n + i] := hermano^.valores[i]; // Copia valor
  end;
  if not hijo^.esHoja then              // Si no es hoja
    for i := 0 to hermano^.n do         // Copia hijos del hermano
      hijo^.hijos[hijo^.n + i] := hermano^.hijos[i]; // Copia hijo
  hijo^.n := hijo^.n + hermano^.n;      // Actualiza número de claves
  for i := idx to nodo^.n - 2 do        // Desplaza claves en el padre
    nodo^.claves[i] := nodo^.claves[i + 1]; // Mueve clave
  for i := idx + 1 to nodo^.n - 1 do    // Desplaza hijos en el padre
    nodo^.hijos[i] := nodo^.hijos[i + 1]; // Mueve hijo
  Dec(nodo^.n);                         // Decrementa claves en padre
  Dispose(hermano);                     // Libera memoria del hermano
end;

// Toma una clave del hermano izquierdo
procedure TomarDeIzquierdo(nodo: PNodoB; idx: Integer);
var
  hijo, hermano: PNodoB; // Nodo hijo y su hermano izquierdo
  i: Integer;           // Índice para mover claves y hijos
begin
  hijo := nodo^.hijos[idx];              // Hijo en la posición idx
  hermano := nodo^.hijos[idx - 1];       // Hermano izquierdo
  for i := hijo^.n - 1 downto 0 do      // Desplaza claves en hijo
  begin
    hijo^.claves[i + 1] := hijo^.claves[i]; // Mueve clave
    hijo^.valores[i + 1] := hijo^.valores[i]; // Mueve valor
  end;
  if not hijo^.esHoja then              // Si no es hoja
    for i := hijo^.n downto 0 do        // Desplaza hijos
      hijo^.hijos[i + 1] := hijo^.hijos[i]; // Mueve hijo
  hijo^.claves[0] := nodo^.claves[idx - 1]; // Toma clave del padre
  hijo^.valores[0] := nodo^.valores[idx - 1]; // Toma valor del padre
  if not hijo^.esHoja then              // Si no es hoja
    hijo^.hijos[0] := hermano^.hijos[hermano^.n]; // Toma hijo del hermano
  nodo^.claves[idx - 1] := hermano^.claves[hermano^.n - 1]; // Actualiza clave en padre
  nodo^.valores[idx - 1] := hermano^.valores[hermano^.n - 1]; // Actualiza valor en padre
  Inc(hijo^.n);                         // Incrementa claves en hijo
  Dec(hermano^.n);                      // Decrementa claves en hermano
end;

// Toma una clave del hermano derecho
procedure TomarDeDerecho(nodo: PNodoB; idx: Integer);
var
  hijo, hermano: PNodoB; // Nodo hijo y su hermano derecho
  i: Integer;           // Índice para mover claves y hijos
begin
  hijo := nodo^.hijos[idx];              // Hijo en la posición idx
  hermano := nodo^.hijos[idx + 1];       // Hermano derecho
  hijo^.claves[hijo^.n] := nodo^.claves[idx]; // Toma clave del padre
  hijo^.valores[hijo^.n] := nodo^.valores[idx]; // Toma valor del padre
  if not hijo^.esHoja then              // Si no es hoja
    hijo^.hijos[hijo^.n + 1] := hermano^.hijos[0]; // Toma hijo del hermano
  Inc(hijo^.n);                         // Incrementa claves en hijo
  nodo^.claves[idx] := hermano^.claves[0]; // Actualiza clave en padre
  nodo^.valores[idx] := hermano^.valores[0]; // Actualiza valor en padre
  for i := 0 to hermano^.n - 2 do       // Desplaza claves en hermano
  begin
    hermano^.claves[i] := hermano^.claves[i + 1]; // Mueve clave
    hermano^.valores[i] := hermano^.valores[i + 1]; // Mueve valor
  end;
  if not hermano^.esHoja then           // Si no es hoja
    for i := 0 to hermano^.n - 1 do     // Desplaza hijos en hermano
      hermano^.hijos[i] := hermano^.hijos[i + 1]; // Mueve hijo
  Dec(hermano^.n);                      // Decrementa claves en hermano
end;

// Corrige un nodo con menos de MIN_CLAVES
procedure CorregirNodo(nodo: PNodoB; idx: Integer);
var
  hijo: PNodoB; // Nodo hijo a corregir
begin
  hijo := nodo^.hijos[idx];              // Hijo en la posición idx
  if (idx > 0) and (nodo^.hijos[idx - 1]^.n > MIN_CLAVES) then // Si hermano izquierdo tiene claves suficientes
    TomarDeIzquierdo(nodo, idx)         // Toma clave del hermano izquierdo
  else if (idx < nodo^.n) and (nodo^.hijos[idx + 1]^.n > MIN_CLAVES) then // Si hermano derecho tiene claves suficientes
    TomarDeDerecho(nodo, idx)           // Toma clave del hermano derecho
  else
  begin
    if idx < nodo^.n then               // Si hay hermano derecho
      Fusionar(nodo, idx)               // Fusiona con hermano derecho
    else
      Fusionar(nodo, idx - 1);          // Fusiona con hermano izquierdo
  end;
end;

// Elimina una clave del árbol B
procedure Eliminar(var raiz: PNodoB; clave: Integer);
var
  i: Integer;   // Índice para recorrer claves
  nodo: PNodoB; // Nodo actual
begin
  if raiz = nil then                     // Si el árbol está vacío
  begin
    Writeln('Clave ', clave, ' no encontrada'); // Clave no existe
    Exit;
  end;
  i := 0;                                // Inicia índice en 0
  while (i < raiz^.n) and (clave > raiz^.claves[i]) do // Busca posición de la clave
    Inc(i);                              // Incrementa índice
  if (i < raiz^.n) and (clave = raiz^.claves[i]) then // Si la clave está en el nodo
  begin
    if raiz^.esHoja then                 // Si el nodo es hoja
    begin
      for i := i to raiz^.n - 2 do       // Desplaza claves
      begin
        raiz^.claves[i] := raiz^.claves[i + 1]; // Mueve clave
        raiz^.valores[i] := raiz^.valores[i + 1]; // Mueve valor
      end;
      Dec(raiz^.n);                     // Decrementa claves
    end
    else
    begin
      if raiz^.hijos[i]^.n >= MIN_CLAVES then // Si hijo izquierdo tiene suficientes claves
      begin
        nodo := EncontrarPredecesor(raiz, i); // Encuentra predecesor
        raiz^.claves[i] := nodo^.claves[nodo^.n - 1]; // Copia clave del predecesor
        raiz^.valores[i] := nodo^.valores[nodo^.n - 1]; // Copia valor del predecesor
        Eliminar(raiz^.hijos[i], nodo^.claves[nodo^.n - 1]); // Elimina predecesor
      end
      else if raiz^.hijos[i + 1]^.n >= MIN_CLAVES then // Si hijo derecho tiene suficientes claves
      begin
        nodo := EncontrarSucesor(raiz, i); // Encuentra sucesor
        raiz^.claves[i] := nodo^.claves[0]; // Copia clave del sucesor
        raiz^.valores[i] := nodo^.valores[0]; // Copia valor del sucesor
        Eliminar(raiz^.hijos[i + 1], nodo^.claves[0]); // Elimina sucesor
      end
      else
      begin
        Fusionar(raiz, i);               // Fusiona hijos
        Eliminar(raiz^.hijos[i], clave); // Elimina clave en el hijo fusionado
      end;
    end;
  end
  else
  begin
    if raiz^.esHoja then                 // Si es hoja y no contiene la clave
    begin
      Writeln('Clave ', clave, ' no encontrada'); // Clave no existe
      Exit;
    end;
    if raiz^.hijos[i]^.n < MIN_CLAVES then // Si el hijo tiene pocas claves
      CorregirNodo(raiz, i);             // Corrige el hijo
    Eliminar(raiz^.hijos[i], clave);     // Elimina en el hijo correspondiente
  end;
  if (raiz^.n = 0) and (not raiz^.esHoja) then // Si la raíz queda vacía
  begin
    nodo := raiz;                        // Guarda raíz actual
    raiz := raiz^.hijos[0];              // Nueva raíz es el primer hijo
    Dispose(nodo);                       // Libera memoria de la raíz antigua
  end;
end;

// Recorre el árbol B en orden
procedure InOrden(nodo: PNodoB);
var
  i: Integer; // Índice para recorrer claves
begin
  if nodo <> nil then                    // Si el nodo no es nulo
  begin
    for i := 0 to nodo^.n - 1 do         // Recorre todas las claves
    begin
      if not nodo^.esHoja then           // Si no es hoja
        InOrden(nodo^.hijos[i]);         // Recorre hijo izquierdo
      Writeln('Clave: ', nodo^.claves[i], ' | Valor: ', nodo^.valores[i]); // Imprime clave y valor
    end;
    if not nodo^.esHoja then             // Si no es hoja
      InOrden(nodo^.hijos[nodo^.n]);     // Recorre último hijo
  end;
end;

// Escribe una línea en el archivo .dot
procedure EscribirLinea(var f: Text; const linea: String);
begin
  Writeln(f, linea);                     // Escribe línea en el archivo
end;

// Genera nodos y conexiones para el archivo .dot
procedure GenerarNodosDOT(var f: Text; nodo: PNodoB; idNodo: Integer);
var
  i: Integer;   // Índice para recorrer claves
  idHijo: Integer; // Identificador para hijos
  clavesStr: String; // Cadena para construir la etiqueta del nodo
begin
  if nodo = nil then                     // Si el nodo es nulo
    Exit;                                // Sale de la función
  clavesStr := '';                       // Inicializa cadena de claves
  for i := 0 to nodo^.n - 1 do           // Recorre claves
    clavesStr := clavesStr + Format('|<%d>%d:%s', [i, nodo^.claves[i], nodo^.valores[i]]); // Construye etiqueta
  EscribirLinea(f, Format('  nodo%d [label="%s", shape=record];', [idNodo, clavesStr])); // Escribe nodo
  for i := 0 to nodo^.n do               // Recorre hijos
    if nodo^.hijos[i] <> nil then        // Si el hijo existe
    begin
      idHijo := idNodo * ORDEN + i + 1;  // Genera ID único para hijo
      EscribirLinea(f, Format('  nodo%d -> nodo%d;', [idNodo, idHijo])); // Conecta nodo con hijo
      GenerarNodosDOT(f, nodo^.hijos[i], idHijo); // Genera subárbol
    end;
end;

// Genera archivo .dot para Graphviz
procedure GenerarDOT(raiz: PNodoB; const nombreArchivo: String);
var
  f: Text;      // Archivo de texto
begin
  Assign(f, nombreArchivo);              // Asigna archivo
  Rewrite(f);                            // Crea o sobrescribe archivo
  EscribirLinea(f, 'digraph ArbolB {');  // Inicia grafo
  EscribirLinea(f, '  node [shape=record, style=filled, fillcolor=lightblue];'); // Estilo de nodos
  if raiz <> nil then                    // Si el árbol no está vacío
    GenerarNodosDOT(f, raiz, 1)          // Genera nodos
  else
    EscribirLinea(f, '  vacio [label="(vacio)"];'); // Árbol vacío
  EscribirLinea(f, '}');                 // Cierra grafo
  Close(f);                              // Cierra archivo
  Writeln('Archivo DOT generado: ', nombreArchivo); // Confirma generación
end;

// Programa principal
begin
  // Inserta nodos de ejemplo
  Insertar(raiz, 10, 'Diez');            // Inserta clave 10
  GenerarDOT(raiz, 'arbol_b.dot');       // Genera archivo .dot
  Readln;                                // Pausa

  Insertar(raiz, 5, 'Cinco');            // Inserta clave 5
  GenerarDOT(raiz, 'arbol_b.dot');       // Genera archivo .dot
  Readln;                                // Pausa

  Insertar(raiz, 15, 'Quince');          // Inserta clave 15
  GenerarDOT(raiz, 'arbol_b.dot');       // Genera archivo .dot
  Readln;                                // Pausa

  Insertar(raiz, 3, 'Tres');             // Inserta clave 3
  GenerarDOT(raiz, 'arbol_b.dot');       // Genera archivo .dot
  Readln;                                // Pausa

  Insertar(raiz, 7, 'Siete');            // Inserta clave 7
  GenerarDOT(raiz, 'arbol_b.dot');       // Genera archivo .dot
  Readln;                                // Pausa

  Writeln('Arbol despues de insertar:'); // Muestra mensaje
  InOrden(raiz);                         // Imprime árbol en orden
  Readln;                                // Pausa

  Actualizar(raiz, 5, 'Cinco actualizado'); // Actualiza valor de clave 5
  Writeln('Arbol despues de actualizar:'); // Muestra mensaje
  InOrden(raiz);                         // Imprime árbol en orden
  GenerarDOT(raiz, 'arbol_b.dot');       // Genera archivo .dot
  Readln;                                // Pausa

  Eliminar(raiz, 10);                    // Elimina clave 10
  Writeln('Arbol despues de eliminar la clave 10:'); // Muestra mensaje
  InOrden(raiz);                         // Imprime árbol en orden
  GenerarDOT(raiz, 'arbol_b.dot');       // Genera archivo .dot
  Readln;                                // Pausa
end.
