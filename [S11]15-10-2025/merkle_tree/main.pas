program merklefavoritos;

{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}

uses
  Classes, SysUtils, fpjson, jsonparser, sha1, gtk2, glib2;

type
  { -------- Clase Favorito -------- }
  TFavorito = class
  public
    Remitente: String;
    Asunto: String;
    Fecha: String;
    Mensaje: String;

    constructor Create(ARemitente, AAsunto, AFecha, AMensaje: String);
    function GetHash: String;
  end;

  { -------- Nodo del árbol Merkle -------- }
  TMerkleNode = class
  public
    Hash: String;
    Left, Right: TMerkleNode;
    Favorito: TFavorito;

    constructor CreateLeaf(AFavorito: TFavorito); overload;
    constructor CreateInternal(ALeft, ARight: TMerkleNode); overload;
  private
    function CalculateHash(const LeftHash, RightHash: String): String;
  end;

  { -------- Árbol Merkle -------- }
  TMerkleTree = class
  private
    Leaves: TList;
    Root: TMerkleNode;

    procedure BuildTree;
    procedure GenerateDotRecursive(Node: TMerkleNode; var SL: TStringList; var IdCounter: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Insert(ARemitente, AAsunto, AFecha, AMensaje: String);
    
    function GenerateDot: String;
  end;

{ ---------- Implementación Favorito ---------- }
constructor TFavorito.Create(ARemitente, AAsunto, AFecha, AMensaje: String);
begin
  Remitente := ARemitente;
  Asunto := AAsunto;
  Fecha := AFecha;
  Mensaje := AMensaje;
end;

function TFavorito.GetHash: String;
var
  JSON: TJSONObject;
  Data: String;
  Digest: TSHA1Digest;
  i: Integer;
begin
  JSON := TJSONObject.Create;
  try
    JSON.Add('Remitente', Remitente);
    JSON.Add('Asunto', Asunto);
    JSON.Add('Fecha', Fecha);
    JSON.Add('Mensaje', Mensaje);
    Data := JSON.AsJSON;
  finally
    JSON.Free;
  end;

  Digest := SHA1String(Data);
  Result := '';
  for i := 0 to High(Digest) do
    Result := Result + LowerCase(IntToHex(Digest[i], 2));
end;

{ ---------- Implementación MerkleNode ---------- }
constructor TMerkleNode.CreateLeaf(AFavorito: TFavorito);
begin
  Favorito := AFavorito;
  Hash := Favorito.GetHash;
  Left := nil;
  Right := nil;
end;

constructor TMerkleNode.CreateInternal(ALeft, ARight: TMerkleNode);
begin
  Favorito := nil;
  Left := ALeft;
  Right := ARight;
  if Assigned(ARight) then
    Hash := CalculateHash(ALeft.Hash, ARight.Hash)
  else
    Hash := CalculateHash(ALeft.Hash, ALeft.Hash);
end;

function TMerkleNode.CalculateHash(const LeftHash, RightHash: String): String;
var
  Combined: String;
  Digest: TSHA1Digest;
  i: Integer;
begin
  Combined := LeftHash + RightHash;
  Digest := SHA1String(Combined);
  Result := '';
  for i := 0 to High(Digest) do
    Result := Result + LowerCase(IntToHex(Digest[i], 2));
end;

{ ---------- Implementación MerkleTree ---------- }
constructor TMerkleTree.Create;
begin
  Leaves := TList.Create;        // Crear lista vacía para las hojas
  Root := nil;                    // Inicializar la raíz como nula
end;

destructor TMerkleTree.Destroy;
begin
  Leaves.Free;                    // Liberar la memoria de la lista de hojas
  inherited Destroy;              // Llamar al destructor de la clase padre
end;

procedure TMerkleTree.Insert(ARemitente, AAsunto, AFecha, AMensaje: String);
var
  Leaf: TMerkleNode;
  Fav: TFavorito;
begin
  Fav := TFavorito.Create(ARemitente, AAsunto, AFecha, AMensaje); // Crear un nuevo favorito
  Leaf := TMerkleNode.CreateLeaf(Fav);                             // Crear nodo hoja con el favorito
  Leaves.Add(Leaf);                                                // Agregar nodo hoja a la lista
  BuildTree;                                                       // Reconstruir el árbol Merkle
end;

procedure TMerkleTree.BuildTree;
var
  CurrentLevel, NextLevel: TList;
  i: Integer;
  Left, Right, Parent: TMerkleNode;
begin
  if Leaves.Count = 0 then
  begin
    Root := nil;              // Si no hay hojas, la raíz es nula
    Exit;
  end;

  CurrentLevel := TList.Create;
  try
    CurrentLevel.Assign(Leaves);  // Copiar hojas al nivel actual

    while CurrentLevel.Count > 1 do  // Mientras haya más de un nodo en el nivel
    begin
      NextLevel := TList.Create;    // Crear lista para el siguiente nivel
      for i := 0 to CurrentLevel.Count - 1 do
      begin
        if i mod 2 = 0 then         // Tomar nodos de dos en dos
        begin
          Left := TMerkleNode(CurrentLevel[i]);            // Nodo izquierdo
          if i + 1 < CurrentLevel.Count then
            Right := TMerkleNode(CurrentLevel[i+1])       // Nodo derecho
          else
            Right := nil;                                  // Si no hay derecho, se duplica
          Parent := TMerkleNode.CreateInternal(Left, Right); // Crear nodo padre
          NextLevel.Add(Parent);                           // Agregar padre al siguiente nivel
        end;
      end;
      CurrentLevel.Free;          // Liberar nivel anterior
      CurrentLevel := NextLevel;  // Pasar al siguiente nivel
    end;

    Root := TMerkleNode(CurrentLevel[0]);  // La raíz es el único nodo del último nivel
  finally
    CurrentLevel.Free;          // Liberar la última lista
  end;
end;

procedure TMerkleTree.GenerateDotRecursive(Node: TMerkleNode; var SL: TStringList; var IdCounter: Integer);
var
  NodeId, LeftId, RightId: Integer;
  LabelText: String;
begin
  if Node = nil then Exit;       // Si nodo nulo, salir

  NodeId := IdCounter;           // Guardar ID del nodo actual
  Inc(IdCounter);                // Incrementar contador global de nodos

  if Assigned(Node.Favorito) then
    LabelText := Format('"De: %s\nAsunto: %s\nFecha: %s\nHash: %s..."',
      [Node.Favorito.Remitente, Node.Favorito.Asunto, Node.Favorito.Fecha, Copy(Node.Hash, 1, 8)])
  else
    LabelText := Format('"Hash: %s..."', [Copy(Node.Hash, 1, 8)]); // Etiqueta para nodo interno

  SL.Add(Format('  node%d [label=%s];', [NodeId, LabelText]));     // Agregar nodo al .dot

  if Assigned(Node.Left) then
  begin
    LeftId := IdCounter;                                       // Guardar ID del hijo izquierdo
    GenerateDotRecursive(Node.Left, SL, IdCounter);           // Llamada recursiva izquierda
    SL.Add(Format('  node%d -> node%d;', [NodeId, LeftId]));  // Dibujar conexión
  end;

  if Assigned(Node.Right) then
  begin
    RightId := IdCounter;                                      // Guardar ID del hijo derecho
    GenerateDotRecursive(Node.Right, SL, IdCounter);          // Llamada recursiva derecha
    SL.Add(Format('  node%d -> node%d;', [NodeId, RightId])); // Dibujar conexión
  end;
end;

function TMerkleTree.GenerateDot: String;
var
  SL: TStringList;
  Counter: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Add('digraph MerkleTree {');         // Encabezado del .dot
    SL.Add('  node [shape=record];');       // Nodo como record
    SL.Add('  graph [rankdir=TB];');        // De arriba a abajo
    SL.Add('  subgraph cluster_0 {');       // Subgrafo principal
    SL.Add('    label="Favoritos";');

    if Root = nil then
      SL.Add('    empty [label="Árbol vacío"];') // Si no hay nodos
    else
    begin
      Counter := 0;
      GenerateDotRecursive(Root, SL, Counter); // Recorrer árbol y generar nodos .dot
    end;

    SL.Add('  }');
    SL.Add('}');
    Result := SL.Text;                     // Devolver texto completo .dot
  finally
    SL.Free;                               // Liberar lista de strings
  end;
end;


{ ---------- Interfaz GTK para insertar y generar DOT ---------- }
var
  tree: TMerkleTree;
  win: PGtkWidget;
  lblRem, lblAsunto, lblFecha, lblMsg: PGtkWidget;
  entryRem, entryAsunto, entryFecha, entryMsg: PGtkWidget;
  btnInsert, btnDot, btnExit: PGtkWidget;

procedure ShowMessage(parent: PGtkWidget; title, text: String);
var
  dlg: PGtkWidget;
begin
  dlg := gtk_message_dialog_new(GTK_WINDOW(parent),
                               GTK_DIALOG_MODAL,
                               GTK_MESSAGE_INFO,
                               GTK_BUTTONS_OK,
                               PChar(text));
  gtk_window_set_title(GTK_WINDOW(dlg), PChar(title));
  gtk_dialog_run(GTK_DIALOG(dlg));
  gtk_widget_destroy(dlg);
end;

procedure OnInsertClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  tree.Insert(
    gtk_entry_get_text(GTK_ENTRY(entryRem)),
    gtk_entry_get_text(GTK_ENTRY(entryAsunto)),
    gtk_entry_get_text(GTK_ENTRY(entryFecha)),
    gtk_entry_get_text(GTK_ENTRY(entryMsg))
  );
  ShowMessage(win, 'Éxito', 'Favorito insertado correctamente');
end;

procedure OnDotClick(widget: PGtkWidget; data: gpointer); cdecl;
var
  fs: TFileStream;
  dotText: String;
begin
  dotText := tree.GenerateDot;
  fs := TFileStream.Create('merkle.dot', fmCreate);
  try
    fs.WriteBuffer(Pointer(dotText)^, Length(dotText));
  finally
    fs.Free;
  end;

end;

procedure OnExitClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_main_quit;
end;

procedure ShowInterface;
var
  grid: PGtkWidget;
begin
  gtk_init(@argc, @argv);
  tree := TMerkleTree.Create;

  win := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(win), PChar('Árbol Merkle'));
  gtk_container_set_border_width(GTK_CONTAINER(win), 10);
  gtk_window_set_default_size(GTK_WINDOW(win), 400, 350);
  g_signal_connect(win, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

  grid := gtk_table_new(6, 2, False);
  gtk_container_add(GTK_CONTAINER(win), grid);

  lblRem := gtk_label_new(PChar('Remitente:'));
  gtk_table_attach_defaults(GTK_TABLE(grid), lblRem, 0,1,0,1);
  entryRem := gtk_entry_new;
  gtk_table_attach_defaults(GTK_TABLE(grid), entryRem, 1,2,0,1);

  lblAsunto := gtk_label_new(PChar('Asunto:'));
  gtk_table_attach_defaults(GTK_TABLE(grid), lblAsunto, 0,1,1,2);
  entryAsunto := gtk_entry_new;
  gtk_table_attach_defaults(GTK_TABLE(grid), entryAsunto, 1,2,1,2);

  lblFecha := gtk_label_new(PChar('Fecha:'));
  gtk_table_attach_defaults(GTK_TABLE(grid), lblFecha, 0,1,2,3);
  entryFecha := gtk_entry_new;
  gtk_table_attach_defaults(GTK_TABLE(grid), entryFecha, 1,2,2,3);

  lblMsg := gtk_label_new(PChar('Mensaje:'));
  gtk_table_attach_defaults(GTK_TABLE(grid), lblMsg, 0,1,3,4);
  entryMsg := gtk_entry_new;
  gtk_table_attach_defaults(GTK_TABLE(grid), entryMsg, 1,2,3,4);

  btnInsert := gtk_button_new_with_label(PChar('Insertar Favorito'));
  g_signal_connect(btnInsert, 'clicked', G_CALLBACK(@OnInsertClick), nil);
  gtk_table_attach_defaults(GTK_TABLE(grid), btnInsert, 0,2,4,5);

  btnDot := gtk_button_new_with_label(PChar('Generar DOT'));
  g_signal_connect(btnDot, 'clicked', G_CALLBACK(@OnDotClick), nil);
  gtk_table_attach_defaults(GTK_TABLE(grid), btnDot, 0,2,5,6);

  btnExit := gtk_button_new_with_label(PChar('Salir'));
  g_signal_connect(btnExit, 'clicked', G_CALLBACK(@OnExitClick), nil);
  gtk_table_attach_defaults(GTK_TABLE(grid), btnExit, 0,2,6,7);

  gtk_widget_show_all(win);
  gtk_main;
  tree.Free;
end;

begin
  ShowInterface;
end.
