unit graphInterface;
{$MODE OBJFPC}
{$H+}

interface
  procedure ShowGraphInterface;

implementation
  uses
    gtk2, glib2, SysUtils, Classes, Unix;

  // ESTRUCTURA DE DATOS DEL GRAFO
  type
    TGraph = class
    private
      FEdges: TStringList; // Lista de aristas
    public
      constructor Create;
      destructor Destroy; override;
      procedure AddEdge(nodeA, nodeB: string);
      function ToDotFormat: string;
    end;

  var
    graphWindow: PGtkWidget;
    lblNodeA, lblNodeB: PGtkWidget;
    entryNodeA, entryNodeB: PGtkWidget;
    btnAddEdge, btnExportDot, btnExit: PGtkWidget;
    Graph: TGraph;

  // IMPLEMENTACIÓN 
  // Constructor
  constructor TGraph.Create;
  begin
    FEdges := TStringList.Create;  // Crea la lista que almacenará las aristas del grafo
  end;

  // Destructor
  destructor TGraph.Destroy;
  begin
    FEdges.Free;                   // Libera la memoria usada por la lista de aristas
    inherited Destroy;             // Llama al destructor del objeto padre (buena práctica)
  end;

  // Agrega una arista no dirigida (A--B y B--A son equivalentes)
  procedure TGraph.AddEdge(nodeA, nodeB: string);
  var
    edge1, edge2: string;
  begin
    edge1 := nodeA + ' -- ' + nodeB;   // Forma textual de la arista A--B
    edge2 := nodeB + ' -- ' + nodeA;   // Forma inversa de la misma arista (no dirigida)

    // Evita duplicados: si no existe ni A--B ni B--A, la agrega
    if (FEdges.IndexOf(edge1) = -1) and (FEdges.IndexOf(edge2) = -1) then
      FEdges.Add(edge1);               // Guarda la arista en la lista
  end;

  // Convierte el grafo en texto con formato DOT (para Graphviz)
  function TGraph.ToDotFormat: string;
  var
    s: string;
    i: Integer;
  begin
    s := 'graph G {' + LineEnding;            // Encabezado del archivo DOT
    for i := 0 to FEdges.Count - 1 do         // Recorre todas las aristas guardadas
      s += '  ' + FEdges[i] + ';' + LineEnding; // Escribe cada arista en formato A -- B;
    s += '}';                                 // Cierra la estructura del grafo
    Result := s;                              // Devuelve todo el texto generado
  end;

  //INTERFAZ 
  procedure ShowGtkMessage(parent: PGtkWidget; title, text: string);
  var
    dialog: PGtkWidget;
  begin
    dialog := gtk_message_dialog_new(GTK_WINDOW(parent),
                                     GTK_DIALOG_MODAL,
                                     GTK_MESSAGE_INFO,
                                     GTK_BUTTONS_OK,
                                     PChar(text));
    gtk_window_set_title(GTK_WINDOW(dialog), PChar(title));
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
  end;

  // Agregar arista desde la interfaz
  procedure OnAddEdgeClick(widget: PGtkWidget; data: gpointer); cdecl;
  var
    nodeA, nodeB: string;
  begin
    nodeA := gtk_entry_get_text(GTK_ENTRY(entryNodeA));
    nodeB := gtk_entry_get_text(GTK_ENTRY(entryNodeB));

    if (Trim(nodeA) = '') or (Trim(nodeB) = '') then
    begin
      ShowGtkMessage(graphWindow, 'Error', 'Debe ingresar ambos nodos.');
      Exit;
    end;

    Graph.AddEdge(nodeA, nodeB);
    ShowGtkMessage(graphWindow, 'Éxito', 'Arista agregada correctamente.');
  end;

  // Exportar grafo a .DOT
  procedure OnExportDotClick(widget: PGtkWidget; data: gpointer); cdecl;
  var
    fs: TFileStream;
    s: string;
  begin
    s := Graph.ToDotFormat;
    fs := TFileStream.Create('grafo.dot', fmCreate);
    try
      fs.WriteBuffer(Pointer(s)^, Length(s));
    finally
      fs.Free;
    end;

    // Intentar generar imagen con Graphviz
    if fpsystem('dot -Tpng grafo.dot -o grafo.png') = 0 then
      ShowGtkMessage(graphWindow, 'Éxito', 'Archivo grafo.dot y grafo.png generados correctamente.')
    else
      ShowGtkMessage(graphWindow, 'Aviso', 'Archivo .dot generado. Instala graphviz para crear la imagen.');
  end;

  // Salir
  procedure OnExitClick(widget: PGtkWidget; data: gpointer); cdecl;
  begin
    gtk_main_quit;
  end;

  // INTERFAZ PRINCIPAL
  procedure ShowGraphInterface;
  var
    grid: PGtkWidget;
  begin
    gtk_init(@argc, @argv);
    Graph := TGraph.Create;

    graphWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(graphWindow), PChar('Grafo No Dirigido'));
    gtk_container_set_border_width(GTK_CONTAINER(graphWindow), 10);
    gtk_window_set_default_size(GTK_WINDOW(graphWindow), 300, 300);

    g_signal_connect(graphWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

    grid := gtk_table_new(8, 1, False);
    gtk_container_add(GTK_CONTAINER(graphWindow), grid);

    lblNodeA := gtk_label_new(PChar('Nodo A:'));
    gtk_table_attach_defaults(GTK_TABLE(grid), lblNodeA, 0, 1, 0, 1);

    entryNodeA := gtk_entry_new;
    gtk_table_attach_defaults(GTK_TABLE(grid), entryNodeA, 0, 1, 1, 2);

    lblNodeB := gtk_label_new(PChar('Nodo B:'));
    gtk_table_attach_defaults(GTK_TABLE(grid), lblNodeB, 0, 1, 2, 3);

    entryNodeB := gtk_entry_new;
    gtk_table_attach_defaults(GTK_TABLE(grid), entryNodeB, 0, 1, 3, 4);

    btnAddEdge := gtk_button_new_with_label(PChar('Agregar Arista'));
    g_signal_connect(btnAddEdge, 'clicked', G_CALLBACK(@OnAddEdgeClick), nil);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnAddEdge, 0, 1, 4, 5);

    btnExportDot := gtk_button_new_with_label(PChar('Generar .DOT'));
    g_signal_connect(btnExportDot, 'clicked', G_CALLBACK(@OnExportDotClick), nil);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnExportDot, 0, 1, 5, 6);

    btnExit := gtk_button_new_with_label(PChar('Salir'));
    g_signal_connect(btnExit, 'clicked', G_CALLBACK(@OnExitClick), nil);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnExit, 0, 1, 6, 7);

    gtk_widget_show_all(graphWindow);
    gtk_main;

    Graph.Free;
  end;

end.
