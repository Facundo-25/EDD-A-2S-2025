program PilaConGrafica;

uses
  sysutils, Process; 

// Definimos un tipo puntero a nodo (PNode) y la estructura del nodo
type
  PNode = ^TNode;
  TNode = record
    data: Integer;   // Dato que almacena el nodo
    next: PNode;     // Puntero al siguiente nodo en la pila
  end;

// Variable global que apunta al tope de la pila
var
  top: PNode = nil;

// Procedimiento PUSH: Inserta un nuevo elemento en la cima de la pila
procedure push(value: Integer);
var
  newNode: PNode;
begin
  New(newNode);             // Reservamos memoria para el nuevo nodo
  newNode^.data := value;   // Asignamos el valor
  newNode^.next := top;     // El nuevo nodo apunta al anterior tope
  top := newNode;           // El nuevo nodo ahora es el tope
end;

// Función POP: Extrae el elemento en la cima de la pila y lo retorna
function pop(): Integer;
var
  temp: PNode;
begin
  if top = nil then
  begin
    Writeln('La pila está vacía.');
    Exit(-1); // Retornamos un valor por defecto
  end;
  temp := top;           // Guardamos el nodo a eliminar
  pop := top^.data;      // Guardamos el valor que vamos a retornar
  top := top^.next;      // Movemos el tope al siguiente nodo
  Dispose(temp);         // Liberamos la memoria del nodo extraído
end;

// Función PEEK: Retorna el valor del tope sin eliminarlo
function peek(): Integer;
begin
  if top = nil then
  begin
    Writeln('La pila está vacía.');
    Exit(-1);
  end;
  peek := top^.data;
end;

// Función isEmpty: Retorna verdadero si la pila está vacía
function isEmpty(): Boolean;
begin
  isEmpty := top = nil;
end;

// Procedimiento graphStack: Genera un archivo .dot y lo convierte en una imagen .png usando Graphviz
procedure graphStack();
var
  f: Text;
  current: PNode;
  fileNameDot, fileNamePng, command: String;
  nodeId: Integer;
  s: AnsiString;
begin
  // Nombres de archivo
  fileNameDot := 'pila.dot';
  fileNamePng := 'pila.png';

  // Abrimos el archivo .dot para escribir
  Assign(f, fileNameDot);
  Rewrite(f);

  // Iniciamos el grafo en formato DOT con subgrafo para el marco y título
  Writeln(f, 'digraph G {');
  Writeln(f, '  rankdir=TB;');         
  Writeln(f, '  subgraph cluster_pila {');
  Writeln(f, '    label = "Pila";');
  Writeln(f, '    labelloc = "t";');    
  Writeln(f, '    style = rounded;');
  Writeln(f, '    color = black;');
  Writeln(f, '    node [shape=record];');

  // Recorremos la pila y escribimos cada nodo dentro del subgrafo
  current := top;
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

  // Ejecutamos Graphviz para generar imagen PNG desde el archivo DOT
  if RunCommand('dot', ['-Tpng', fileNameDot, '-o', fileNamePng], s) then
  begin
    Writeln('Imagen generada correctamente: ', fileNamePng);
  end
  else
    Writeln('Error al generar la imagen.');

end;

// Procedimiento auxiliar para mostrar la pila en consola
procedure mostrarPila();
var
  current: PNode;
begin
  current := top;
  Write('Cima -> ');
  while current <> nil do
  begin
    Write(current^.data, ' -> ');
    current := current^.next;
  end;
  Writeln('nil');
end;

// Programa principal
begin
  // Ejemplo de uso de la pila
  push(10);
  push(20);
  push(30);

  Writeln('Pila actual:');
  mostrarPila();

  // Generamos imagen inicial
  graphStack();

  // Hacemos un POP
  Writeln('Elemento extraído (pop): ', pop());

  Writeln('Pila después de pop:');
  mostrarPila();

  // Volvemos a generar imagen actualizada
  graphStack();
end.
