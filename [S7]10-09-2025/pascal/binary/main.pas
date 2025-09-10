program ArbolBinario;

// Incluye la unidad SysUtils para funciones de manejo de archivos y strings
uses
  SysUtils;

type
  // Definimos un puntero a un nodo del árbol binario
  PNodo = ^TNodo;
  TNodo = record
    clave: Integer;             // Clave numérica del nodo
    valor: String;              // Valor asociado (texto)
    izquierda, derecha: PNodo;  // Punteros a subárboles izquierdo y derecho
  end;

var
  raiz: PNodo = nil; // Raíz del árbol, inicialmente vacía

// Crear un nuevo nodo con una clave y valor
function CrearNodo(clave: Integer; valor: String): PNodo;
var
  nuevo: PNodo; // Puntero al nuevo nodo
begin
  New(nuevo);                    // Reservar memoria para el nodo
  nuevo^.clave := clave;         // Asignar la clave
  nuevo^.valor := valor;         // Asignar el valor
  nuevo^.izquierda := nil;       // Inicializar hijo izquierdo como nulo
  nuevo^.derecha := nil;         // Inicializar hijo derecho como nulo
  CrearNodo := nuevo;            // Retornar el nuevo nodo
end;

// Insertar un nodo en el árbol binario
procedure Insertar(var raiz: PNodo; clave: Integer; valor: String);
begin
  if raiz = nil then
    raiz := CrearNodo(clave, valor) // Si la raíz es nula, crear nuevo nodo
  else if clave < raiz^.clave then
    Insertar(raiz^.izquierda, clave, valor) // Insertar en subárbol izquierdo
  else if clave > raiz^.clave then
    Insertar(raiz^.derecha, clave, valor)   // Insertar en subárbol derecho
  else
    raiz^.valor := valor; // Si la clave ya existe, actualizar el valor
end;

// Buscar un nodo por su clave
function Buscar(raiz: PNodo; clave: Integer): PNodo;
begin
  if (raiz = nil) or (raiz^.clave = clave) then
    Buscar := raiz // Retornar nodo si se encuentra o es nulo
  else if clave < raiz^.clave then
    Buscar := Buscar(raiz^.izquierda, clave) // Buscar en subárbol izquierdo
  else
    Buscar := Buscar(raiz^.derecha, clave); // Buscar en subárbol derecho
end;

// Actualizar el valor de un nodo existente
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

// Encontrar el nodo con la clave mínima
function EncontrarMinimo(nodo: PNodo): PNodo;
begin
  while nodo^.izquierda <> nil do
    nodo := nodo^.izquierda; // Ir al nodo más a la izquierda
  EncontrarMinimo := nodo;   // Retornar el nodo mínimo
end;

// Eliminar un nodo del árbol
procedure Eliminar(var raiz: PNodo; clave: Integer);
var
  temp: PNodo; // Nodo temporal para eliminación
begin
  if raiz = nil then
  begin
    Writeln('Clave no encontrada.');
    Exit;
  end;

  if clave < raiz^.clave then
    Eliminar(raiz^.izquierda, clave) // Buscar en subárbol izquierdo
  else if clave > raiz^.clave then
    Eliminar(raiz^.derecha, clave) // Buscar en subárbol derecho
  else
  begin
    // Caso 1: Nodo sin hijos
    if (raiz^.izquierda = nil) and (raiz^.derecha = nil) then
    begin
      Dispose(raiz); // Liberar memoria
      raiz := nil;   // Establecer como nulo
    end
    // Caso 2: Nodo con un hijo (derecho)
    else if raiz^.izquierda = nil then
    begin
      temp := raiz;
      raiz := raiz^.derecha; // Reemplazar con hijo derecho
      Dispose(temp);        // Liberar memoria
    end
    // Caso 2: Nodo con un hijo (izquierdo)
    else if raiz^.derecha = nil then
    begin
      temp := raiz;
      raiz := raiz^.izquierda; // Reemplazar con hijo izquierdo
      Dispose(temp);          // Liberar memoria
    end
    // Caso 3: Nodo con dos hijos
    else
    begin
      temp := EncontrarMinimo(raiz^.derecha); // Encontrar sucesor
      raiz^.clave := temp^.clave;             // Copiar clave
      raiz^.valor := temp^.valor;             // Copiar valor
      Eliminar(raiz^.derecha, temp^.clave);   // Eliminar sucesor
    end;
  end;
end;

// Recorrido in-orden del árbol (izquierda, raíz, derecha)
procedure InOrden(nodo: PNodo);
begin
  if nodo <> nil then
  begin
    InOrden(nodo^.izquierda); // Recorrer subárbol izquierdo
    Writeln('Clave: ', nodo^.clave, ' | Valor: ', nodo^.valor); // Imprimir nodo
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

  // Escribir nodo actual en formato DOT
  EscribirLinea(f, Format('  "%d" [label="%d: %s"];', [nodo^.clave, nodo^.clave, nodo^.valor]));

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

  EscribirLinea(f, 'digraph ArbolBinario {'); // Iniciar grafo
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
  Writeln('--- Árbol Binario en Pascal (FPC/Linux) ---');

  // Insertar nodos de ejemplo
  Insertar(raiz, 10, 'Diez');
  GenerarDOT(raiz, 'arbol.dot');
  Readln;

  Insertar(raiz, 5, 'Cinco');
  GenerarDOT(raiz, 'arbol.dot');
  Readln;

  Insertar(raiz, 15, 'Quince');
  GenerarDOT(raiz, 'arbol.dot');
  Readln;

  Insertar(raiz, 3, 'Tres');
  GenerarDOT(raiz, 'arbol.dot');
  Readln;
  
  Insertar(raiz, 7, 'Siete');
  GenerarDOT(raiz, 'arbol.dot');
  Readln;

  Writeln('Árbol después de insertar:');
  InOrden(raiz); // Mostrar árbol en orden

  // Actualizar un nodo
  Actualizar(raiz, 5, 'Cinco actualizado');
  GenerarDOT(raiz, 'arbol.dot');
  Readln;

  Writeln('Árbol después de actualizar:');
  InOrden(raiz); // Mostrar árbol actualizado


  // Eliminar un nodo
  Eliminar(raiz, 10);
  GenerarDOT(raiz, 'arbol.dot');
  Readln;

end.
