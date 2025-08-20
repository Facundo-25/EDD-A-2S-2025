unit home;

interface

procedure ShowHomeWindow;

implementation

uses
  gtk2, glib2,
  insert_product, delete_product, update_product, login;

var
  homeWindow: PGtkWidget;

procedure CloseCurrentWindowAndShowHome(win: PGtkWidget);
begin
  gtk_widget_destroy(win);
  ShowHomeWindow;
end;

procedure OnInsertClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_destroy(homeWindow);
  ShowInsertProductWindow;
end;

procedure OnDeleteClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_destroy(homeWindow);
  ShowDeleteProductWindow;
end;

procedure OnUpdateClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_destroy(homeWindow);
  ShowUpdateProductWindow;
end;

procedure OnReportsClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  Writeln('REPORTES-GENERADOS');
end;

procedure OnLogoutClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
  gtk_widget_destroy(homeWindow);
  ShowLoginWindow;
end;

procedure ShowHomeWindow;
var
  vbox: PGtkWidget;
  btnInsert, btnDelete, btnUpdate, btnReports, btnLogout: PGtkWidget;
begin
  // Inicializa GTK 
  gtk_init(@argc, @argv);

  // Crea la ventana principal
  homeWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(homeWindow), 'Home'); // Título de la ventana
  gtk_container_set_border_width(GTK_CONTAINER(homeWindow), 10); // Bordes
  gtk_window_set_default_size(GTK_WINDOW(homeWindow), 300, 300); // Tamaño por defecto

  // Crea un contenedor vertical (VBox) para los botones
  vbox := gtk_vbox_new(True, 5);
  gtk_container_add(GTK_CONTAINER(homeWindow), vbox); // Añade VBox a la ventana

  // Crea los botones con sus respectivas etiquetas
  btnInsert := gtk_button_new_with_label('Insertar Producto');
  btnDelete := gtk_button_new_with_label('Eliminar Producto');
  btnUpdate := gtk_button_new_with_label('Actualizar Producto');
  btnReports := gtk_button_new_with_label('Reportes');
  btnLogout := gtk_button_new_with_label('Cerrar Sesión');

  // Asocia eventos "click" a cada botón 
  g_signal_connect(btnInsert, 'clicked', G_CALLBACK(@OnInsertClick), nil);
  g_signal_connect(btnDelete, 'clicked', G_CALLBACK(@OnDeleteClick), nil);
  g_signal_connect(btnUpdate, 'clicked', G_CALLBACK(@OnUpdateClick), nil);
  g_signal_connect(btnReports, 'clicked', G_CALLBACK(@OnReportsClick), nil);
  g_signal_connect(btnLogout, 'clicked', G_CALLBACK(@OnLogoutClick), nil);

  // Agrega los botones al contenedor vertical
  gtk_box_pack_start(GTK_BOX(vbox), btnInsert, False, False, 0);
  gtk_box_pack_start(GTK_BOX(vbox), btnDelete, False, False, 0);
  gtk_box_pack_start(GTK_BOX(vbox), btnUpdate, False, False, 0);
  gtk_box_pack_start(GTK_BOX(vbox), btnReports, False, False, 0);
  gtk_box_pack_start(GTK_BOX(vbox), btnLogout, False, False, 0);

  // Muestra todos los elementos en la ventana
  gtk_widget_show_all(homeWindow);

  // Cierra la aplicación cuando se destruye la ventana
  g_signal_connect(homeWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

  // Inicia el bucle principal de eventos de GTK
  gtk_main;
end;

end.
