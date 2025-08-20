unit delete_product;

interface

procedure ShowDeleteProductWindow;

implementation

uses
  gtk2, glib2, gdk2, home;

var
  deleteWindow: PGtkWidget;
  entryID: PGtkWidget;

procedure OnDeleteClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  writeln('ELIMINAR PRODUCTO');
  writeln('ID: ', gtk_entry_get_text(GTK_ENTRY(entryID)));

  gtk_widget_destroy(deleteWindow);
  ShowHomeWindow;
end;

procedure ShowDeleteProductWindow;
var
  grid: PGtkWidget;
  lblID, btnDelete: PGtkWidget;
begin
  gtk_init(@argc, @argv);

  deleteWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(deleteWindow), 'Eliminar Producto');
  gtk_container_set_border_width(GTK_CONTAINER(deleteWindow), 10);

  grid := gtk_table_new(2, 2, False);
  gtk_container_add(GTK_CONTAINER(deleteWindow), grid);

  lblID := gtk_label_new('ID:');
  entryID := gtk_entry_new;

  btnDelete := gtk_button_new_with_label('Eliminar');
  g_signal_connect(btnDelete, 'clicked', G_CALLBACK(@OnDeleteClick), nil);

  gtk_table_attach_defaults(GTK_TABLE(grid), lblID, 0, 1, 0, 1);
  gtk_table_attach_defaults(GTK_TABLE(grid), entryID, 1, 2, 0, 1);
  gtk_table_attach_defaults(GTK_TABLE(grid), btnDelete, 0, 2, 1, 2);

  gtk_widget_show_all(deleteWindow);
  g_signal_connect(deleteWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);
  gtk_main;
end;

end.
