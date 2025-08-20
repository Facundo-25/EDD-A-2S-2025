unit simpleLinkedList;

interface
    
uses
  SysUtils, process;

type
  PNode = ^TNode;
  TNode = record
    id: string;
    name: string;
    username: string;
    email: string;
    phone: string;
    password: string;
    next: PNode;
  end;

var
  head: PNode = nil;

// Inserta un nuevo nodo al inicio de la lista
procedure InsertNode(aId, aName, aUsername, aEmail, aPhone, aPassword: string);

// Elimina el nodo con el id dado (si existe)
procedure DeleteNode(aId: string);

// Actualiza el nodo con el id dado (si existe)
procedure UpdateNode(aId, newName, newUsername, newEmail, newPhone, newPassword: string);

// Genera un archivo .dot y una imagen .png con Graphviz para visualizar la lista
procedure GraphList();

implementation

// Inserta al inicio
procedure InsertNode(aId, aName, aUsername, aEmail, aPhone, aPassword: string);
var
  newNode: PNode;
begin
  New(newNode);
  newNode^.id := aId;
  newNode^.name := aName;
  newNode^.username := aUsername;
  newNode^.email := aEmail;
  newNode^.phone := aPhone;
  newNode^.password := aPassword;
  newNode^.next := head;
  head := newNode;
end;

// Elimina nodo por id
procedure DeleteNode(aId: string);
var
  current, prev: PNode;
begin
  current := head;
  prev := nil;

  while (current <> nil) and (current^.id <> aId) do
  begin
    prev := current;
    current := current^.next;
  end;

  if current = nil then
  begin
    Writeln('Node with id "', aId, '" not found.');
    Exit;
  end;

  if prev = nil then
  begin
    // Eliminar cabeza
    head := current^.next;
  end
  else
  begin
    prev^.next := current^.next;
  end;

  Dispose(current);
  Writeln('Node with id "', aId, '" deleted.');
end;

// Actualiza nodo por id
procedure UpdateNode(aId, newName, newUsername, newEmail, newPhone, newPassword: string);
var
  current: PNode;
begin
  current := head;
  while (current <> nil) and (current^.id <> aId) do
    current := current^.next;

  if current = nil then
  begin
    Writeln('Node with id "', aId, '" not found.');
    Exit;
  end;

  current^.name := newName;
  current^.username := newUsername;
  current^.email := newEmail;
  current^.phone := newPhone;
  current^.password := newPassword;

  Writeln('Node with id "', aId, '" updated.');
end;

// Graficar la lista simple usando Graphviz
procedure GraphList();
var
  f: Text;
  current: PNode;
  fileNameDot, fileNamePng, command: string;
  nodeId: Integer;
  s: AnsiString;
begin
  fileNameDot := 'simpleLinkedList.dot';
  fileNamePng := 'simpleLinkedList.png';

  Assign(f, fileNameDot);
  Rewrite(f);

  Writeln(f, 'digraph G {');
  Writeln(f, '  rankdir=LR;');  // Dirección de izquierda a derecha
  Writeln(f, '  node [shape=record, style=filled, fillcolor=lightblue];');

  current := head;
  nodeId := 0;

  // Crear nodos con info
  while current <> nil do
  begin
    Writeln(f, Format('  node%d [label="<id> ID: %s | <name> Name: %s | <user> User: %s | <email> Email: %s | <phone> Phone: %s | <pass> Password: %s"];',
      [nodeId, current^.id, current^.name, current^.username, current^.email, current^.phone, current^.password]));

    // Crear flecha al siguiente nodo si existe
    if current^.next <> nil then
      Writeln(f, Format('  node%d -> node%d;', [nodeId, nodeId + 1]));

    current := current^.next;
    Inc(nodeId);
  end;

  Writeln(f, '}');
  Close(f);

  // Ejecutar Graphviz para generar imagen PNG
  if RunCommand('dot', ['-Tpng', fileNameDot, '-o', fileNamePng], s) then
    Writeln('Image generated successfully: ', fileNamePng)
  else
    Writeln('Error generating image.');
end;

end.
