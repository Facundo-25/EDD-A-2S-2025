unit login;

interface

// Procedimiento se encarga de mostrar la ventana de inicio de sesión
procedure ShowLoginWindow;

implementation

uses
  gtk2, glib2, gdk2, home;

var
  // Campos de entrada para usuario y contraseña
  entryUser, entryPass: PGtkWidget;
  // Referencia a la ventana de login
  loginWindow: PGtkWidget;

// Iniciar sesión
procedure OnLoginButtonClick(widget: PGtkWidget; data: gpointer); cdecl;
var
  userText, passText: PChar;
begin
  // Obtener el texto ingresado en los campos de usuario y contraseña
  userText := PChar(gtk_entry_get_text(GTK_ENTRY(entryUser)));
  passText := PChar(gtk_entry_get_text(GTK_ENTRY(entryPass)));

  // Mostrar en consola los datos ingresados (solo con fines de prueba)
  writeln('Usuario ingresado: ', userText);
  writeln('Contraseña ingresada: ', passText);

  // Validar las credenciales
  if (StrPas(userText) = 'vendedor') and (StrPas(passText) = '123') then
  begin
    writeln('INICIO-DE-SESIÓN-EXITOSO');

    // Si las credenciales son correctas, cerrar la ventana de login
    gtk_widget_destroy(loginWindow);

    // Mostrar la ventana principal (home)
    ShowHomeWindow;
    
  end
  else
    // Mostrar mensaje de error si las credenciales no son correctas
    writeln('INICIO-DE-SESIÓN-FALLIDO');
end;

// Muestra la interfaz de la ventana de login
procedure ShowLoginWindow;
var
  grid: PGtkWidget;
  lblUser, lblPass: PGtkWidget;
  btnLogin: PGtkWidget;
begin
  // Inicializar la librería GTK (solo debe hacerse una vez al inicio)
  gtk_init(@argc, @argv);

  // Crear una nueva ventana principal para el login
  loginWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(loginWindow), 'Login');
  gtk_container_set_border_width(GTK_CONTAINER(loginWindow), 10);
  gtk_window_set_default_size(GTK_WINDOW(loginWindow), 300, 150);

  // Crear una tabla para organizar los widgets (etiquetas, entradas y botón)
  grid := gtk_table_new(3, 2, False);
  gtk_container_add(GTK_CONTAINER(loginWindow), grid);

  // Crear etiquetas para los campos
  lblUser := gtk_label_new('Usuario:');
  lblPass := gtk_label_new('Contraseña:');

  // Crear campos de texto para ingresar usuario y contraseña
  entryUser := gtk_entry_new;
  entryPass := gtk_entry_new;
  gtk_entry_set_visibility(GTK_ENTRY(entryPass), False); // Ocultar caracteres de la contraseña

  // Crear el botón de login y conectar el evento "click"
  btnLogin := gtk_button_new_with_label('Iniciar sesión');
  g_signal_connect(btnLogin, 'clicked', G_CALLBACK(@OnLoginButtonClick), nil);

  // Colocar los elementos en la tabla
  gtk_table_attach_defaults(GTK_TABLE(grid), lblUser, 0, 1, 0, 1);
  gtk_table_attach_defaults(GTK_TABLE(grid), entryUser, 1, 2, 0, 1);
  gtk_table_attach_defaults(GTK_TABLE(grid), lblPass, 0, 1, 1, 2);
  gtk_table_attach_defaults(GTK_TABLE(grid), entryPass, 1, 2, 1, 2);
  gtk_table_attach_defaults(GTK_TABLE(grid), btnLogin, 0, 2, 2, 3);

  // Mostrar todos los elementos en la ventana
  gtk_widget_show_all(loginWindow);

  // Cuando se cierra la ventana, terminar el programa
  g_signal_connect(loginWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

  // Iniciar el bucle principal de eventos de GTK
  gtk_main;
end;

end.
