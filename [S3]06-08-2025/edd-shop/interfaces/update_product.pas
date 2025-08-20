unit update_product;

interface

procedure ShowUpdateProductWindow;

implementation

uses
  gtk2, glib2, gdk2, home;

var
  updateWindow: PGtkWidget;
  entryID, entryName, entryPrice, entryQuantity: PGtkWidget;

procedure OnUpdateClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  writeln('ACTUALIZAR PRODUCTO');
  writeln('ID: ', gtk_entry_get_text(GTK_ENTRY(entryID)));
  writeln('Nuevo Nombre: ', gtk_entry_get_text(GTK_ENTRY(entryName)));
  writeln('Nuevo Precio: ', gtk_entry_get_text(GTK_ENTRY(entryPrice)));
  writeln('Nueva Cantidad: ', gtk_entry_get_text(GTK_ENTRY(entryQuantity)));

  gtk_widget_destroy(updateWindow);
  ShowHomeWindow;
end;

procedure ShowUpdateProductWindow;
var
  grid: PGtkWidget;
  lblID, lblName, lblPrice, lblQuantity, btnUpdate: PGtkWidget;
begin
  gtk_init(@argc, @argv);

  updateWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(updateWindow), 'Actualizar Producto');
  gtk_container_set_border_width(GTK_CONTAINER(updateWindow), 10);

  grid := gtk_table_new(5, 2, False);
  gtk_container_add(GTK_CONTAINER(updateWindow), grid);

  lblID := gtk_label_new('ID:');
  lblName := gtk_label_new('Nuevo Nombre:');
  lblPrice := gtk_label_new('Nuevo Precio:');
  lblQuantity := gtk_label_new('Nueva Cantidad:');

  entryID := gtk_entry_new;
  entryName := gtk_entry_new;
  entryPrice := gtk_entry_new;
  entryQuantity := gtk_entry_new;

  btnUpdate := gtk_button_new_with_label('Actualizar');
  g_signal_connect(btnUpdate, 'clicked', G_CALLBACK(@OnUpdateClick), nil);

  gtk_table_attach_defaults(GTK_TABLE(grid), lblID, 0, 1, 0, 1);
  gtk_table_attach_defaults(GTK_TABLE(grid), entryID, 1, 2, 0, 1);
  gtk_table_attach_defaults(GTK_TABLE(grid), lblName, 0, 1, 1, 2);
  gtk_table_attach_defaults(GTK_TABLE(grid), entryName, 1, 2, 1, 2);
  gtk_table_attach_defaults(GTK_TABLE(grid), lblPrice, 0, 1, 2, 3);
  gtk_table_attach_defaults(GTK_TABLE(grid), entryPrice, 1, 2, 2, 3);
  gtk_table_attach_defaults(GTK_TABLE(grid), lblQuantity, 0, 1, 3, 4);
  gtk_table_attach_defaults(GTK_TABLE(grid), entryQuantity, 1, 2, 3, 4);
  gtk_table_attach_defaults(GTK_TABLE(grid), btnUpdate, 0, 2, 4, 5);

  gtk_widget_show_all(updateWindow);
  g_signal_connect(updateWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);
  gtk_main;
end;

end.
