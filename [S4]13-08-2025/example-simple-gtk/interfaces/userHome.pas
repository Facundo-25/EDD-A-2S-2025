unit userHome;

interface
    procedure ShowUserHomeWindow
;

implementation

    uses
        gtk2, glib2, login, userCreateCar
    ;


    var
        userWindow: PGtkWidget;
        lblWelcome: PGtkWidget;
        btnAddVehicle, btnSelectService, btnReports, btnLogout: PGtkWidget
    ;


    // Redireccion a insertar vehiculo
    procedure OnAddVehicleClick(widget: PGtkWidget; data: gpointer); cdecl;
        begin
        gtk_widget_destroy(userWindow);
        ShowUserCreateCarWindow;
        end
    ;


    // Redireccion a insertar un servicio
    procedure OnSelectServiceClick(widget: PGtkWidget; data: gpointer); cdecl;
        begin
        writeln('Botón "Seleccionar Servicio" presionado.');
        end
    ;


    // Generar reportes
    procedure OnReportsClick(widget: PGtkWidget; data: gpointer); cdecl;
        begin
        writeln('Botón "Reportes" presionado.');
        end
    ;


    // Cerrar sesion
    procedure OnLogoutClick(widget: PGtkWidget; data: gpointer); cdecl;
        begin
        gtk_widget_destroy(userWindow);
        ShowLoginWindow;
        end
    ;


    // Mostrar la ventana principal de usuario
    procedure ShowUserHomeWindow;

        var
            grid: PGtkWidget
        ;

        begin
            gtk_init(@argc, @argv);

            // Crear ventana principal
            userWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
            gtk_window_set_title(GTK_WINDOW(userWindow), 'Inicio Usuario');
            gtk_container_set_border_width(GTK_CONTAINER(userWindow), 10);
            gtk_window_set_default_size(GTK_WINDOW(userWindow), 300, 350);

            // Crear una tabla para organizar widgets (5 filas x 1 columna)
            grid := gtk_table_new(5, 1, False);
            gtk_container_add(GTK_CONTAINER(userWindow), grid);

            // Crear etiqueta de bienvenida
            lblWelcome := gtk_label_new('Bienvenido Usuario');
            gtk_table_attach_defaults(GTK_TABLE(grid), lblWelcome, 0, 1, 0, 1);

            // Crear botones con textos en español
            btnAddVehicle := gtk_button_new_with_label('Ingresar Vehículo');
            btnSelectService := gtk_button_new_with_label('Seleccionar Servicio');
            btnReports := gtk_button_new_with_label('Reportes');
            btnLogout := gtk_button_new_with_label('Cerrar Sesión');

            // Conectar señales de click
            g_signal_connect(btnAddVehicle, 'clicked', G_CALLBACK(@OnAddVehicleClick), nil);
            g_signal_connect(btnSelectService, 'clicked', G_CALLBACK(@OnSelectServiceClick), nil);
            g_signal_connect(btnReports, 'clicked', G_CALLBACK(@OnReportsClick), nil);
            g_signal_connect(btnLogout, 'clicked', G_CALLBACK(@OnLogoutClick), nil);

            // Ubicar botones en la tabla, cada uno en su fila correspondiente, columna 0
            gtk_table_attach_defaults(GTK_TABLE(grid), btnAddVehicle,    0, 1, 1, 2);
            gtk_table_attach_defaults(GTK_TABLE(grid), btnSelectService, 0, 1, 2, 3);
            gtk_table_attach_defaults(GTK_TABLE(grid), btnReports,       0, 1, 3, 4);
            gtk_table_attach_defaults(GTK_TABLE(grid), btnLogout,        0, 1, 4, 5);

            // Mostrar todos los widgets
            gtk_widget_show_all(userWindow);

            // Evento para cerrar ventana
            g_signal_connect(userWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

            // Ejecutar el loop principal de GTK
            gtk_main;
        end
    ;

end.