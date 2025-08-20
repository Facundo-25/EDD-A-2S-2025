program SparseMatrix;

uses
  SysUtils;

type
  // Estructura para las cabeceras (filas o columnas)
  PNodoCabecera = ^TNodoCabecera;
  TNodoCabecera = record
    Fila: string;
    Columna: string;
    SiguienteFil: PNodoCabecera;
    SiguienteCol: PNodoCabecera;
    Acceso: Pointer; // Apunta al primer nodo interno
  end;

  // Estructura para los nodos internos
  PNodoInterno = ^TNodoInterno;
  TNodoInterno = record
    Fila: string;
    Columna: string;
    Detalle: Integer;
    Derecha: PNodoInterno;
  end;

var
  Root: PNodoCabecera; // Nodo raíz de la matriz

// Busca o crea una cabecera para una fila específica
function ObtenerOCrearCabeceraFila(Fila: string): PNodoCabecera;
var
  Anterior, Actual, Nueva: PNodoCabecera;
begin
  // Inicializamos los punteros para recorrer la lista de cabeceras de filas
  Anterior := Root;
  Actual := Root^.SiguienteFil;
  
  // Recorremos la lista hasta encontrar la fila o el lugar donde insertarla
  while (Actual <> nil) and (Actual^.Fila < Fila) do
  begin
    Anterior := Actual;
    Actual := Actual^.SiguienteFil;
  end;
  
  // Si encontramos la fila, la retornamos
  if (Actual <> nil) and (Actual^.Fila = Fila) then
    ObtenerOCrearCabeceraFila := Actual
  else
  begin
    // Si no existe, creamos una nueva cabecera de fila
    New(Nueva);
    Nueva^.Fila := Fila;
    Nueva^.Columna := '';
    Nueva^.Acceso := nil;
    Nueva^.SiguienteFil := nil;
    Nueva^.SiguienteCol := nil;
    
    // Enlazamos la nueva cabecera en la lista ordenada
    Nueva^.SiguienteFil := Actual;
    Anterior^.SiguienteFil := Nueva;
    ObtenerOCrearCabeceraFila := Nueva;
  end;
end;

// Busca o crea una cabecera para una columna específica
function ObtenerOCrearCabeceraColumna(Columna: string): PNodoCabecera;
var
  Anterior, Actual, Nueva: PNodoCabecera;
begin
  // Inicializamos los punteros para recorrer la lista de cabeceras de columnas
  Anterior := Root;
  Actual := Root^.SiguienteCol;
  
  // Recorremos la lista hasta encontrar la columna o el lugar donde insertarla
  while (Actual <> nil) and (Actual^.Columna < Columna) do
  begin
    Anterior := Actual;
    Actual := Actual^.SiguienteCol;
  end;
  
  // Si encontramos la columna, la retornamos
  if (Actual <> nil) and (Actual^.Columna = Columna) then
    ObtenerOCrearCabeceraColumna := Actual
  else
  begin
    // Si no existe, creamos una nueva cabecera de columna
    New(Nueva);
    Nueva^.Fila := '';
    Nueva^.Columna := Columna;
    Nueva^.Acceso := nil;
    Nueva^.SiguienteFil := nil;
    Nueva^.SiguienteCol := nil;
    
    // Enlazamos la nueva cabecera en la lista ordenada
    Nueva^.SiguienteCol := Actual;
    Anterior^.SiguienteCol := Nueva;
    ObtenerOCrearCabeceraColumna := Nueva;
  end;
end;

// Inserta un nodo en la submatriz de una fila específica
procedure InsertarEnSubmatriz(CabFila, CabCol: PNodoCabecera; Detalle: Integer);
var
  Actual, Anterior, Nuevo: PNodoInterno;
  ColObjetivo: string;
begin
  // Obtenemos el primer nodo interno de la fila y preparamos variables
  Actual := PNodoInterno(CabFila^.Acceso);
  Anterior := nil;
  ColObjetivo := CabCol^.Columna;
  
  // Recorremos la lista de nodos internos hasta encontrar la columna o el lugar de inserción
  while (Actual <> nil) and (Actual^.Columna < ColObjetivo) do
  begin
    Anterior := Actual;
    Actual := Actual^.Derecha;
  end;
  
  // Si ya existe un nodo en esa posición, actualizamos su valor
  if (Actual <> nil) and (Actual^.Columna = ColObjetivo) then
  begin
    Actual^.Detalle := Detalle;
    Exit;
  end;
  
  // Si no existe, creamos un nuevo nodo interno
  New(Nuevo);
  Nuevo^.Fila := CabFila^.Fila;
  Nuevo^.Columna := CabCol^.Columna;
  Nuevo^.Detalle := Detalle;
  Nuevo^.Derecha := nil;
  
  // Enlazamos el nuevo nodo en la lista ordenada
  Nuevo^.Derecha := Actual;
  if Anterior = nil then
    CabFila^.Acceso := Nuevo
  else
    Anterior^.Derecha := Nuevo;
end;

// Inserta un elemento en la matriz dispersa
procedure Insertar(Fila, Columna: string; Detalle: Integer);
var
  CabFila, CabCol: PNodoCabecera;
begin
  // Obtenemos o creamos las cabeceras correspondientes
  CabFila := ObtenerOCrearCabeceraFila(Fila);
  CabCol := ObtenerOCrearCabeceraColumna(Columna);
  
  // Insertamos el nodo en la submatriz
  InsertarEnSubmatriz(CabFila, CabCol, Detalle);
end;

// Genera la representación en formato DOT
procedure GenerateDot;
var
  Dot: Text;
  Filas, Columnas: array of PNodoCabecera;
  ActualFil, ActualCol: PNodoCabecera;
  ActualNodo, PrevioNodo: PNodoInterno;
  i, FilCount, ColCount: Integer;
  NodeName, PrevName: string;
begin
  Assign(Dot, 'matriz.dot');
  Rewrite(Dot);
  Writeln(Dot, 'digraph Bitacora {');
  Writeln(Dot, '  graph [splines=ortho];');
  Writeln(Dot, '  rankdir=TB;');
  Writeln(Dot, '  node [shape=record, style=filled, fillcolor=white];');
  Writeln(Dot, '  label = "Matriz Dispersa (Bitácora)";');
  FilCount := 0;
  ColCount := 0;
  ActualFil := Root^.SiguienteFil;
  while ActualFil <> nil do
  begin
    Inc(FilCount);
    ActualFil := ActualFil^.SiguienteFil;
  end;
  ActualCol := Root^.SiguienteCol;
  while ActualCol <> nil do
  begin
    Inc(ColCount);
    ActualCol := ActualCol^.SiguienteCol;
  end;
  SetLength(Filas, FilCount);
  SetLength(Columnas, ColCount);
  FilCount := 0;
  ActualFil := Root^.SiguienteFil;
  while ActualFil <> nil do
  begin
    Filas[FilCount] := ActualFil;
    Inc(FilCount);
    ActualFil := ActualFil^.SiguienteFil;
  end;
  ColCount := 0;
  ActualCol := Root^.SiguienteCol;
  while ActualCol <> nil do
  begin
    Columnas[ColCount] := ActualCol;
    Inc(ColCount);
    ActualCol := ActualCol^.SiguienteCol;
  end;
  Writeln(Dot, '  nodeRoot [label="ROOT", shape=plaintext, fillcolor=yellow];');
  Write(Dot, '  { rank=same; nodeRoot;');
  for i := 0 to ColCount - 1 do
    Write(Dot, Format(' colHeader_%s;', [Columnas[i]^.Columna]));
  Writeln(Dot, ' }');
  for i := 0 to ColCount - 1 do
    Writeln(Dot, Format('  colHeader_%s [label="%s", shape=plaintext, fillcolor=lightblue];',
      [Columnas[i]^.Columna, Columnas[i]^.Columna]));
  for i := 0 to FilCount - 1 do
    Writeln(Dot, Format('  rowHeader_%s [label="%s", shape=plaintext, fillcolor=lightgreen];',
      [Filas[i]^.Fila, Filas[i]^.Fila]));
  if ColCount > 0 then
  begin
    Writeln(Dot, Format('  nodeRoot -> colHeader_%s [color=black];', [Columnas[0]^.Columna]));
    Writeln(Dot, Format('  colHeader_%s -> nodeRoot [color=black];', [Columnas[0]^.Columna]));
  end;
  if FilCount > 0 then
  begin
    Writeln(Dot, Format('  nodeRoot -> rowHeader_%s [color=black];', [Filas[0]^.Fila]));
    Writeln(Dot, Format('  rowHeader_%s -> nodeRoot [color=black];', [Filas[0]^.Fila]));
  end;
  for i := 0 to ColCount - 2 do
  begin
    Writeln(Dot, Format('  colHeader_%s -> colHeader_%s [color=black];',
      [Columnas[i]^.Columna, Columnas[i + 1]^.Columna]));
    Writeln(Dot, Format('  colHeader_%s -> colHeader_%s [color=black];',
      [Columnas[i + 1]^.Columna, Columnas[i]^.Columna]));
  end;
  for i := 0 to FilCount - 2 do
  begin
    Writeln(Dot, Format('  rowHeader_%s -> rowHeader_%s [color=black];',
      [Filas[i]^.Fila, Filas[i + 1]^.Fila]));
    Writeln(Dot, Format('  rowHeader_%s -> rowHeader_%s [color=black];',
      [Filas[i + 1]^.Fila, Filas[i]^.Fila]));
  end;
  for i := 0 to FilCount - 1 do
  begin
    Write(Dot, '  { rank=same;');
    Write(Dot, Format(' rowHeader_%s;', [Filas[i]^.Fila]));
    ActualNodo := PNodoInterno(Filas[i]^.Acceso);
    while ActualNodo <> nil do
    begin
      Write(Dot, Format(' f_%s_c_%s;', [Filas[i]^.Fila, ActualNodo^.Columna]));
      ActualNodo := ActualNodo^.Derecha;
    end;
    Writeln(Dot, ' }');
  end;
  for i := 0 to FilCount - 1 do
  begin
    ActualNodo := PNodoInterno(Filas[i]^.Acceso);
    PrevioNodo := nil;
    while ActualNodo <> nil do
    begin
      NodeName := Format('f_%s_c_%s', [Filas[i]^.Fila, ActualNodo^.Columna]);
      Writeln(Dot, Format('  %s [label="%d"];', [NodeName, ActualNodo^.Detalle]));
      if PrevioNodo = nil then
      begin
        Writeln(Dot, Format('  rowHeader_%s -> %s [color=black];', [Filas[i]^.Fila, NodeName]));
        Writeln(Dot, Format('  %s -> rowHeader_%s [color=black];', [NodeName, Filas[i]^.Fila]));
      end
      else
      begin
        PrevName := Format('f_%s_c_%s', [Filas[i]^.Fila, PrevioNodo^.Columna]);
        Writeln(Dot, Format('  %s -> %s [color=black];', [PrevName, NodeName]));
        Writeln(Dot, Format('  %s -> %s [color=black];', [NodeName, PrevName]));
      end;
      Writeln(Dot, Format('  colHeader_%s -> %s [color=black];', [ActualNodo^.Columna, NodeName]));
      Writeln(Dot, Format('  %s -> colHeader_%s [color=black];', [NodeName, ActualNodo^.Columna]));
      PrevioNodo := ActualNodo;
      ActualNodo := ActualNodo^.Derecha;
    end;
  end;
  Writeln(Dot, '}');
  Close(Dot);
end;

// Programa principal para inicializar y probar la matriz
begin
  // Creamos el nodo raíz de la matriz dispersa
  New(Root);
  Root^.Fila := '';
  Root^.Columna := '';
  Root^.SiguienteFil := nil;
  Root^.SiguienteCol := nil;
  Root^.Acceso := nil;

  // Insertamos valores de ejemplo en la matriz
  Insertar('sm1', 'fm1', 1);
  Insertar('jm1', 'om1', 2);
  Insertar('dm1', 'gm1', 3);


  // Generamos el archivo DOT para visualización
  GenerateDot;

  // Nota: No se libera memoria para mantener el ejemplo simple
end.