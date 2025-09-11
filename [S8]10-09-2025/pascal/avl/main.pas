program ArbolAVL;

uses
  SysUtils;

type
  // Definimos un puntero a un nodo del árbol AVL
  PNodo = ^TNodo;
  TNodo = record
    clave: Integer;             // Clave numérica del nodo
    valor: String;              // Valor asociado (texto)
    izquierda, derecha: PNodo;  // Punteros a subárboles izquierdo y derecho
    altura: Integer;            // Altura del subárbol con raíz en este nodo
  end;

var
  raiz: PNodo = nil; // Raíz del árbol, inicialmente vacía

// Obtener la altura de un nodo (0 si nulo)
function GetAltura(nodo: PNodo): Integer;
begin
  if nodo = nil then
    GetAltura := 0
  else
    GetAltura := nodo^.altura;
end;

// Actualizar la altura de un nodo basado en sus hijos
procedure ActualizarAltura(nodo: PNodo);
var
  alturaIzq, alturaDer: Integer; // Alturas de los subárboles
begin
  alturaIzq := GetAltura(nodo^.izquierda); // Obtener altura izquierda
  alturaDer := GetAltura(nodo^.derecha);   // Obtener altura derecha
  if alturaIzq > alturaDer then
    nodo^.altura := 1 + alturaIzq // Usar la mayor altura
  else
    nodo^.altura := 1 + alturaDer; // Usar la mayor altura
end;

// Obtener el factor de balance (altura izquierda - altura derecha)
function GetBalance(nodo: PNodo): Integer;
begin
  if nodo = nil then
    GetBalance := 0
  else
    GetBalance := GetAltura(nodo^.izquierda) - GetAltura(nodo^.derecha);
end;

// Rotación derecha: Corrige desbalance LL (izquierda-izquierda)
function RotarDerecha(y: PNodo): PNodo;
var
  x: PNodo; // Nodo izquierdo de y
begin
  x := y^.izquierda;         // x es el hijo izquierdo de y
  y^.izquierda := x^.derecha; // Mover subárbol derecho de x a izquierdo de y
  x^.derecha := y;            // y se convierte en hijo derecho de x
  ActualizarAltura(y);        // Actualizar altura de y
  ActualizarAltura(x);        // Actualizar altura de x (nueva raíz)
  RotarDerecha := x;          // Retornar nueva raíz
end;

// Rotación izquierda: Corrige desbalance RR (derecha-derecha)
function RotarIzquierda(x: PNodo): PNodo;
var
  y: PNodo; // Nodo derecho de x
begin
  y := x^.derecha;           // y es el hijo derecho de x
  x^.derecha := y^.izquierda; // Mover subárbol izquierdo de y a derecho de x
  y^.izquierda := x;          // x se convierte en hijo izquierdo de y
  ActualizarAltura(x);        // Actualizar altura de x
  ActualizarAltura(y);        // Actualizar altura de y (nueva raíz)
  RotarIzquierda := y;        // Retornar nueva raíz
end;

// Crear un nuevo nodo con clave, valor y altura inicial 1
function CrearNodo(clave: Integer; valor: String): PNodo;
var
  nuevo: PNodo; // Puntero al nuevo nodo
begin
  New(nuevo);                    // Reservar memoria para el nodo
  nuevo^.clave := clave;         // Asignar la clave
  nuevo^.valor := valor;         // Asignar el valor
  nuevo^.izquierda := nil;       // Inicializar hijo izquierdo como nulo
  nuevo^.derecha := nil;         // Inicializar hijo derecho como nulo
  nuevo^.altura := 1;            // Altura inicial de hoja
  CrearNodo := nuevo;            // Retornar el nuevo nodo
end;

// Insertar un nodo en el árbol AVL con balanceo
function Insertar(var raiz: PNodo; clave: Integer; valor: String): PNodo;
var
  balance: Integer; // Factor de balance
begin
  // Caso base: Árbol vacío o posición de inserción
  if raiz = nil then
  begin
    raiz := CrearNodo(clave, valor);
    Insertar := raiz;
    Exit;
  end;

  // Inserción recursiva en subárbol correspondiente
  if clave < raiz^.clave then
    raiz^.izquierda := Insertar(raiz^.izquierda, clave, valor)
  else if clave > raiz^.clave then
    raiz^.derecha := Insertar(raiz^.derecha, clave, valor)
  else
  begin
    raiz^.valor := valor; // Actualizar valor si clave existe
    Insertar := raiz;
    Exit;
  end;

  // Actualizar altura de la raíz actual
  ActualizarAltura(raiz);

  // Obtener factor de balance y corregir si es necesario
  balance := GetBalance(raiz);

  // Caso LL: Desbalance a la izquierda-izquierda
  if (balance > 1) and (clave < raiz^.izquierda^.clave) then
  begin
    Insertar := RotarDerecha(raiz);
    Exit;
  end;

  // Caso RR: Desbalance a la derecha-derecha
  if (balance < -1) and (clave > raiz^.derecha^.clave) then
  begin
    Insertar := RotarIzquierda(raiz);
    Exit;
  end;

  // Caso LR: Desbalance a la izquierda-derecha
  if (balance > 1) and (clave > raiz^.izquierda^.clave) then
  begin
    raiz^.izquierda := RotarIzquierda(raiz^.izquierda);
    Insertar := RotarDerecha(raiz);
    Exit;
  end;

  // Caso RL: Desbalance a la derecha-izquierda
  if (balance < -1) and (clave < raiz^.derecha^.clave) then
  begin
    raiz^.derecha := RotarDerecha(raiz^.derecha);
    Insertar := RotarIzquierda(raiz);
    Exit;
  end;

  Insertar := raiz; // Retornar raíz actualizada
end;

// Buscar un nodo por su clave (igual que en BST)
function Buscar(raiz: PNodo; clave: Integer): PNodo;
begin
  if (raiz = nil) or (raiz^.clave = clave) then
    Buscar := raiz // Retornar nodo si se encuentra o es nulo
  else if clave < raiz^.clave then
    Buscar := Buscar(raiz^.izquierda, clave) // Buscar en subárbol izquierdo
  else
    Buscar := Buscar(raiz^.derecha, clave); // Buscar en subárbol derecho
end;

// Actualizar el valor de un nodo existente (no afecta estructura)
procedure Actualizar(var raiz: PNodo; clave: Integer; nuevoValor: String);
var
  nodo: PNodo; // Puntero al nodo encontrado
begin
  nodo := Buscar(raiz, clave);
  if nodo <> nil then
  begin
    nodo^.valor := nuevoValor; // Actualizar valor
    Writeln('Valor actualizado correctamente.');
  end
  else
    Writeln('Clave no encontrada.');
end;

// Encontrar el nodo con la clave mínima (para eliminación)
function EncontrarMinimo(nodo: PNodo): PNodo;
begin
  while nodo^.izquierda <> nil do
    nodo := nodo^.izquierda; // Ir al nodo más a la izquierda
  EncontrarMinimo := nodo;   // Retornar el nodo mínimo
end;

// Eliminar un nodo del árbol AVL con balanceo
function Eliminar(var raiz: PNodo; clave: Integer): PNodo;
var
  temp: PNodo;      // Nodo temporal para eliminación
  balance: Integer; // Factor de balance
begin
  if raiz = nil then
  begin
    Writeln('Clave no encontrada.');
    Eliminar := nil;
    Exit;
  end;

  // Eliminación recursiva en subárbol correspondiente
  if clave < raiz^.clave then
    raiz^.izquierda := Eliminar(raiz^.izquierda, clave)
  else if clave > raiz^.clave then
    raiz^.derecha := Eliminar(raiz^.derecha, clave)
  else
  begin
    // Caso 1: Nodo sin hijos o un hijo
    if (raiz^.izquierda = nil) or (raiz^.derecha = nil) then
    begin
      if raiz^.izquierda <> nil then
        temp := raiz^.izquierda
      else
        temp := raiz^.derecha;
      if temp = nil then
      begin
        temp := raiz;
        raiz := nil;
      end
      else
        raiz^ := temp^; // Copiar contenido de temp a raiz
      Dispose(temp);    // Liberar memoria
    end
    else
    begin
      // Caso 2: Dos hijos, encontrar sucesor
      temp := EncontrarMinimo(raiz^.derecha);
      raiz^.clave := temp^.clave;
      raiz^.valor := temp^.valor;
      raiz^.derecha := Eliminar(raiz^.derecha, temp^.clave);
    end;
  end;

  if raiz = nil then
  begin
    Eliminar := nil;
    Exit;
  end;

  // Actualizar altura de la raíz actual
  ActualizarAltura(raiz);

  // Obtener factor de balance y corregir si es necesario
  balance := GetBalance(raiz);

  // Caso LL: Desbalance a la izquierda-izquierda
  if (balance > 1) and (GetBalance(raiz^.izquierda) >= 0) then
  begin
    Eliminar := RotarDerecha(raiz);
    Exit;
  end;

  // Caso LR: Desbalance a la izquierda-derecha
  if (balance > 1) and (GetBalance(raiz^.izquierda) < 0) then
  begin
    raiz^.izquierda := RotarIzquierda(raiz^.izquierda);
    Eliminar := RotarDerecha(raiz);
    Exit;
  end;

  // Caso RR: Desbalance a la derecha-derecha
  if (balance < -1) and (GetBalance(raiz^.derecha) <= 0) then
  begin
    Eliminar := RotarIzquierda(raiz);
    Exit;
  end;

  // Caso RL: Desbalance a la derecha-izquierda
  if (balance < -1) and (GetBalance(raiz^.derecha) > 0) then
  begin
    raiz^.derecha := RotarDerecha(raiz^.derecha);
    Eliminar := RotarIzquierda(raiz);
    Exit;
  end;

  Eliminar := raiz; // Retornar raíz actualizada
end;

// Recorrido in-orden del árbol (izquierda, raíz, derecha)
procedure InOrden(nodo: PNodo);
begin
  if nodo <> nil then
  begin
    InOrden(nodo^.izquierda); // Recorrer subárbol izquierdo
    Writeln('Clave: ', nodo^.clave, ' | Valor: ', nodo^.valor, ' | Altura: ', nodo^.altura); // Imprimir nodo con altura
    InOrden(nodo^.derecha);   // Recorrer subárbol derecho
  end;
end;

// Escribe una línea en el archivo .dot
procedure EscribirLinea(var f: Text; const linea: String);
begin
  Writeln(f, linea); // Escribir línea en el archivo
end;

// Generar nodos y conexiones para el archivo .dot
procedure GenerarNodosDOT(var f: Text; nodo: PNodo);
begin
  if nodo = nil then
    Exit; // Salir si el nodo es nulo

  // Escribir nodo actual en formato DOT con altura
  EscribirLinea(f, Format('  "%d" [label="%d: %s (Alt: %d)"];', [nodo^.clave, nodo^.clave, nodo^.valor, nodo^.altura]));

  // Conexión al hijo izquierdo
  if nodo^.izquierda <> nil then
  begin
    EscribirLinea(f, Format('  "%d" -> "%d";', [nodo^.clave, nodo^.izquierda^.clave]));
    GenerarNodosDOT(f, nodo^.izquierda); // Recorrer subárbol izquierdo
  end;

  // Conexión al hijo derecho
  if nodo^.derecha <> nil then
  begin
    EscribirLinea(f, Format('  "%d" -> "%d";', [nodo^.clave, nodo^.derecha^.clave]));
    GenerarNodosDOT(f, nodo^.derecha); // Recorrer subárbol derecho
  end;
end;

// Generar archivo .dot para visualizar con Graphviz
procedure GenerarDOT(raiz: PNodo; const nombreArchivo: String);
var
  f: Text; // Archivo de texto
begin
  Assign(f, nombreArchivo); // Asignar archivo
  Rewrite(f);               // Crear o sobrescribir archivo

  EscribirLinea(f, 'digraph ArbolAVL {'); // Iniciar grafo
  EscribirLinea(f, '  node [shape=circle, style=filled, color=lightblue];'); // Estilo de nodos

  if raiz <> nil then
    GenerarNodosDOT(f, raiz) // Generar nodos si el árbol no está vacío
  else
    EscribirLinea(f, '  vacio [label="(vacio)"];'); // Árbol vacío

  EscribirLinea(f, '}'); // Cerrar grafo
  Close(f);              // Cerrar archivo
  Writeln('Archivo DOT generado: ', nombreArchivo);
end;

// Programa principal
begin
  // Insertar nodos de ejemplo (usar función Insertar que retorna PNodo)
  raiz := Insertar(raiz, 10, 'Diez');
  GenerarDOT(raiz, 'arbol_avl.dot');
  ReadLn;

  raiz := Insertar(raiz, 5, 'Cinco');
  GenerarDOT(raiz, 'arbol_avl.dot');
  ReadLn;

  raiz := Insertar(raiz, 15, 'Quince');
  GenerarDOT(raiz, 'arbol_avl.dot');
  ReadLn;

  raiz := Insertar(raiz, 3, 'Tres');
  GenerarDOT(raiz, 'arbol_avl.dot');
  ReadLn;

  raiz := Insertar(raiz, 7, 'Siete');
  GenerarDOT(raiz, 'arbol_avl.dot');
  ReadLn;

  Writeln('Árbol después de insertar:');
  InOrden(raiz); // Mostrar árbol en orden

  // Actualizar un nodo
  Actualizar(raiz, 5, 'Cinco actualizado');
  Writeln('Árbol después de actualizar:');
  InOrden(raiz);
  GenerarDOT(raiz, 'arbol_avl.dot');
  ReadLn;

  // Eliminar un nodo
  raiz := Eliminar(raiz, 10);
  Writeln('Árbol después de eliminar la clave 10:');
  InOrden(raiz); // Mostrar árbol final
  GenerarDOT(raiz, 'arbol_avl.dot');

end.
