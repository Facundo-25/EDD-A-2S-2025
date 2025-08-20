program ColaConGrafica;

uses
  sysutils, Process;

// Definimos un tipo puntero a nodo (PNode) y la estructura del nodo
type
  PNode = ^TNode;
  TNode = record
    data: Integer;
    next: PNode;
  end;

// Punteros al frente y final de la cola
var
  front, rear: PNode;

// Procedimiento ENQUEUE: Inserta un elemento al final de la cola
procedure enqueue(value: Integer);
var
  newNode: PNode;
begin
  New(newNode);
  newNode^.data := value;
  newNode^.next := nil;

  if front = nil then
  begin
    front := newNode;
    rear := newNode;
  end
  else
  begin
    rear^.next := newNode;
    rear := newNode;
  end;
end;

// Función DEQUEUE: Extrae el elemento al frente de la cola y lo retorna
function dequeue(): Integer;
var
  temp: PNode;
begin
  if front = nil then
  begin
    Writeln('La cola está vacía.');
    Exit(-1);  // Valor por defecto
  end;

  temp := front;
  dequeue := temp^.data;
  front := front^.next;

  // Si la cola quedó vacía, rear también debe apuntar a nil
  if front = nil then
    rear := nil;

  Dispose(temp);
end;

// Función PEEK: Retorna el valor al frente sin eliminarlo
function peek(): Integer;
begin
  if front = nil then
  begin
    Writeln('La cola está vacía.');
    Exit(-1);
  end;
  peek := front^.data;
end;

// Función isEmpty: Retorna verdadero si la cola está vacía
function isEmpty(): Boolean;
begin
  isEmpty := front = nil;
end;

// Procedimiento para mostrar gráficamente la cola
procedure graphQueue();
var
  f: Text;
  current: PNode;
  fileNameDot, fileNamePng, command: String;
  nodeId: Integer;
  s: AnsiString;
begin
  fileNameDot := 'cola.dot';
  fileNamePng := 'cola.png';

  Assign(f, fileNameDot);
  Rewrite(f);

  Writeln(f, 'digraph G {');
  Writeln(f, '  rankdir=LR;'); // Dirección izquierda a derecha
  Writeln(f, '  subgraph cluster_cola {');
  Writeln(f, '    label = "Cola";');
  Writeln(f, '    labelloc = "t";');
  Writeln(f, '    style = rounded;');
  Writeln(f, '    color = black;');
  Writeln(f, '    node [shape=record];');

  current := front;
  nodeId := 0;

  while current <> nil do
  begin
    Writeln(f, Format('    node%d [label="<data> %d"];', [nodeId, current^.data]));
    if current^.next <> nil then
      Writeln(f, Format('    node%d -> node%d;', [nodeId, nodeId + 1]));
    current := current^.next;
    Inc(nodeId);
  end;

  Writeln(f, '  }');
  Writeln(f, '}');
  Close(f);

  if RunCommand('dot', ['-Tpng', fileNameDot, '-o', fileNamePng], s) then
    Writeln('Imagen generada correctamente: ', fileNamePng)
  else
    Writeln('Error al generar la imagen.');
end;

// Procedimiento para mostrar la cola en consola
procedure mostrarCola();
var
  current: PNode;
begin
  current := front;
  Write('Frente -> ');
  while current <> nil do
  begin
    Write(current^.data, ' -> ');
    current := current^.next;
  end;
  Writeln('nil');
end;

// Programa principal
begin
  front := nil;
  rear := nil;

  // Insertamos elementos
  enqueue(100);
  enqueue(200);
  enqueue(300);

  Writeln('Cola actual:');
  mostrarCola();

  // Generamos imagen
  graphQueue();

  // Quitamos un elemento
  Writeln('Elemento extraído (dequeue): ', dequeue());

  Writeln('Cola después del dequeue:');
  mostrarCola();

  // Generamos imagen actualizada
  graphQueue();
end.
