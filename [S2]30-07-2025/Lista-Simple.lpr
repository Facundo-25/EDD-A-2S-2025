program project1;
uses crt;

type
   //Tipo de dato que almacenara la lista
  TDato = Integer;

  // Nodo de la lista
  PNodo = ^TNodo; // -> La variable de este tipo solo podra apuntar a elementos TNodo
  TNodo = record
    dato: TDato;
    siguiente: PNodo; //^TNodo, apunta a elementos TNodo
  end;

  // LIsta enlazada
  TLista = record
    cabeza: PNodo; //^TNodo, apunta a elementos TNodo
    tamano: Integer;
  end;


//Operaciones para lista
//Inicializarla
procedure InicializarLista(var lista: TLista); //lista.cabeza y lista.tamano
begin
   lista.cabeza := nil;
   lista.tamano := 0;
end;

  //Insertar al inicio
  procedure InsertarInicio(var lista: TLista; valor: TDato); // Se puede mandar los demas valores
  var
    nuevoNodo: PNodo; // ^TNodo, apunta a elementos TNodo
  begin
    new(nuevoNodo); // Reserva espacio en memoria para el nuevoNodo
    nuevoNodo^.dato := valor;        // A lo que apuntaba nuevoNodo.dato ahora a valor
    nuevoNodo^.siguiente := lista.cabeza;
    lista.cabeza := nuevoNodo;   // nuevoNodo ahora estara en cabeza
    Inc(lista.tamano); // Aumenta el valor de tamano
  end;

  //Insertar al final
  procedure InsertarAlFinal(var lista: Tlista; valor: TDato);
  var
    nuevoNodo, actual: PNodo; //^TNodo, apunta a elementos TNodo, dato y siguiente
  begin
    new(nuevoNodo);
    nuevoNodo^.dato := valor;
    nuevoNodo^.siguiente := nil;

    if lista.cabeza = nil then
       lista.cabeza := nuevoNodo
    else
    begin
      actual := lista.cabeza;
      while actual^.siguiente <> nil do // !=
            actual := actual^.siguiente;    //para llegar hasta nodo -> null
      actual^.siguiente := nuevoNodo;
    end;
    Inc(Lista.tamano);

  end;


// Eliminar nodo
function EliminarNodo(var lista: TLista; valor: TDato): Boolean; // Retorna un boolan
var
  actual, anterior: PNodo;  // ^TNodo
begin
  actual := lista.cabeza;
  anterior := nil;

  while (actual <> nil) and (actual^.dato <> valor) do
  begin
    anterior := actual;
    actual := actual^.siguiente;
  end;

  if actual = nil then
    Exit(False); // No se encontró el valor

  if anterior = nil then      // El nodo es el primero
    lista.cabeza := actual^.siguiente
  else
    anterior^.siguiente := actual^.siguiente;

  Dispose(actual);  //Libera memoria del nodo a eliminar.
  Dec(lista.tamano);   // Reducir tamaño
  Result := True;
end;

// Buscar elemento
function Buscar(lista: TLista; valor: TDato): Boolean;
var
  actual: PNodo;  //^TNodo
begin
  actual := lista.cabeza;
  while actual <> nil do
  begin
    if actual^.dato = valor then
      Exit(True);  // Para salir de la funcion.
    actual := actual^.siguiente;
  end;
  Result := False;     // -> lo que va retornar
end;


// Mostrar lista
procedure MostrarLista(lista: TLista);
var
  actual: PNodo; //^Tnode
begin
  actual := lista.cabeza;
  Write('Lista: [');
  while actual <> nil do
  begin
    Write(actual^.dato);
    if actual^.siguiente <> nil then
      Write(', ');
    actual := actual^.siguiente;
  end;
  Writeln(']');
  Writeln('Tamaño: ', lista.tamano);
end;

// Liberar memoria
procedure LiberarLista(var lista: TLista);
var
  actual, siguiente: PNodo;
begin
  actual := lista.cabeza;
  while actual <> nil do
  begin
    siguiente := actual^.siguiente;
    Dispose(actual);
    actual := siguiente;
  end;
  lista.cabeza := nil;
  lista.tamano := 0;
end;



var
  miLista: TLista;
begin
  // Inicializar lista
  InicializarLista(miLista);

  // Insertar elementos
  InsertarAlFinal(miLista, 10);
  InsertarAlFinal(miLista, 20);
  InsertarInicio(miLista, 5);
  InsertarAlFinal(miLista, 30);

  // Mostrar lista
  MostrarLista(miLista);
  // Salida esperada: Lista: [5, 10, 20, 30]
  //                  Tamaño: 4

  // Buscar elemento
  if Buscar(miLista, 20) then
    Writeln('20 está en la lista')
  else
    Writeln('20 NO está en la lista');

  // Eliminar elemento
  if EliminarNodo(miLista, 10) then
    Writeln('10 eliminado correctamente')
  else
    Writeln('10 no encontrado');

  // Mostrar lista después de eliminar
  MostrarLista(miLista);
  // Salida esperada: Lista: [5, 20, 30]
  //                  Tamaño: 3

  // Liberar memoria
  LiberarLista(miLista);

  readkey;
end.

