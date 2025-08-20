unit insert_product;

interface

procedure ShowInsertProductWindow;

implementation

uses
  gtk2, glib2, gdk2, home;

var
  insertWindow: PGtkWidget;
  entryID, entryName, entryPrice, entryQuantity: PGtkWidget;

procedure OnInsertClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  writeln('INSERTAR PRODUCTO');
  writeln('ID: ', gtk_entry_get_text(GTK_ENTRY(entryID)));
  writeln('Nombre: ', gtk_entry_get_text(GTK_ENTRY(entryName)));
  writeln('Precio: ', gtk_entry_get_text(GTK_ENTRY(entryPrice)));
  writeln('Cantidad: ', gtk_entry_get_text(GTK_ENTRY(entryQuantity)));

  gtk_widget_destroy(insertWindow);
  ShowHomeWindow;
end;

procedure ShowInsertProductWindow;
var
  grid: PGtkWidget;
  lblID, lblName, lblPrice, lblQuantity, btnInsert: PGtkWidget;
begin
  gtk_init(@argc, @argv);

  insertWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(insertWindow), 'Insertar Producto');
  gtk_container_set_border_width(GTK_CONTAINER(insertWindow), 10);

  grid := gtk_table_new(5, 2, False);
  gtk_container_add(GTK_CONTAINER(insertWindow), grid);

  lblID := gtk_label_new('ID:');
  lblName := gtk_label_new('Nombre:');
  lblPrice := gtk_label_new('Precio:');
  lblQuantity := gtk_label_new('Cantidad:');

  entryID := gtk_entry_new;
  entryName := gtk_entry_new;
  entryPrice := gtk_entry_new;
  entryQuantity := gtk_entry_new;

  btnInsert := gtk_button_new_with_label('Insertar');
  g_signal_connect(btnInsert, 'clicked', G_CALLBACK(@OnInsertClick), nil);

  gtk_table_attach_defaults(GTK_TABLE(grid), lblID, 0, 1, 0, 1);
  gtk_table_attach_defaults(GTK_TABLE(grid), entryID, 1, 2, 0, 1);
  gtk_table_attach_defaults(GTK_TABLE(grid), lblName, 0, 1, 1, 2);
  gtk_table_attach_defaults(GTK_TABLE(grid), entryName, 1, 2, 1, 2);
  gtk_table_attach_defaults(GTK_TABLE(grid), lblPrice, 0, 1, 2, 3);
  gtk_table_attach_defaults(GTK_TABLE(grid), entryPrice, 1, 2, 2, 3);
  gtk_table_attach_defaults(GTK_TABLE(grid), lblQuantity, 0, 1, 3, 4);
  gtk_table_attach_defaults(GTK_TABLE(grid), entryQuantity, 1, 2, 3, 4);
  gtk_table_attach_defaults(GTK_TABLE(grid), btnInsert, 0, 2, 4, 5);

  gtk_widget_show_all(insertWindow);
  g_signal_connect(insertWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);
  gtk_main;
end;

end.
