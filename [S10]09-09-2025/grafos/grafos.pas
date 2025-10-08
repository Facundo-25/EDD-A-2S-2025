program GraphWithGraphviz;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

// Representa un vértice del grafo
type
  TVertex = record
    Name: string;  // Nombre del vértice
  end;

// Representa una arista del grafo
type
  TEdge = record
    FromVertex: string;  // Vértice de origen
    ToVertex: string;    // Vértice de destino
    Weight: Integer;     // Peso de la arista
  end;

type
  TGraph = class
  private
    FVertices: TStringList; // Lista de nombres de vértices
    FEdges: array of TEdge; // Lista de aristas
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddVertex(const Name: string); // Agregar un vértice
    procedure AddEdge(const FromName, ToName: string; Weight: Integer); // Agregar una arista
    procedure ExportToDot(const AFilename: string); // Exportar grafo a Graphviz
  end;

// Constructor: inicializa la lista de vértices
constructor TGraph.Create;
begin
  FVertices := TStringList.Create;
  FVertices.Sorted := False;   // No ordenar automáticamente
  FVertices.Duplicates := dupIgnore; // Ignorar duplicados
end;

// Destructor: libera memoria
destructor TGraph.Destroy;
begin
  FVertices.Free;
  inherited Destroy;
end;

// Agregar un vértice al grafo
procedure TGraph.AddVertex(const Name: string);
begin
  FVertices.Add(Name);  // Agrega el nombre del vértice a la lista
end;

// Agregar una arista al grafo
procedure TGraph.AddEdge(const FromName, ToName: string; Weight: Integer);
var
  NewEdge: TEdge;
begin
  NewEdge.FromVertex := FromName; // Vértice de origen
  NewEdge.ToVertex := ToName;     // Vértice de destino
  NewEdge.Weight := Weight;       // Peso de la arista
  SetLength(FEdges, Length(FEdges) + 1); // Incrementa tamaño de la lista de aristas
  FEdges[High(FEdges)] := NewEdge;      // Asigna la nueva arista
end;

// Exportar grafo a archivo .dot para Graphviz
procedure TGraph.ExportToDot(const AFilename: string);
var
  SL: TStringList;
  I: Integer;
  Edge: TEdge;
begin
  SL := TStringList.Create;
  try
    SL.Add('digraph G {');           // Grafo dirigido
    SL.Add('  rankdir=LR;');         // De izquierda a derecha
    SL.Add('  node [shape=circle];'); // Nodos como círculos
    SL.Add('');

    // Definir nodos
    for I := 0 to FVertices.Count - 1 do
      SL.Add(Format('  "%s";', [FVertices[I]]));

    SL.Add('');

    // Definir aristas con pesos
    for I := 0 to High(FEdges) do
    begin
      Edge := FEdges[I];
      SL.Add(Format('  "%s" -> "%s" [label="%d"];', [Edge.FromVertex, Edge.ToVertex, Edge.Weight]));
    end;

    SL.Add('}');
    SL.SaveToFile(AFilename);  // Guardar archivo .dot
  finally
    SL.Free;
  end;
end;

//Programa principal
var
  Graph: TGraph;
begin
  Graph := TGraph.Create;
  try
    // Agregar vértices
    Graph.AddVertex('A');
    Graph.AddVertex('B');
    Graph.AddVertex('C');
    Graph.AddVertex('D');

    // Agregar aristas con pesos
    Graph.AddEdge('A', 'B', 5);
    Graph.AddEdge('B', 'C', 3);
    Graph.AddEdge('A', 'C', 10);
    Graph.AddEdge('C', 'D', 2);

    // Exportar grafo a Graphviz
    Graph.ExportToDot('graph_example.dot');
    WriteLn('Archivo .dot creado: graph_example.dot');
    WriteLn('Puedes convertirlo a PNG con: dot -Tpng graph_example.dot -o graph_example.png');

  finally
    Graph.Free;
  end;

  ReadLn;
end.
