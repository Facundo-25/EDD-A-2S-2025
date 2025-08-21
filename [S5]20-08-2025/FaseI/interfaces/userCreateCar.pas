unit userCreateCar;

interface
    procedure ShowUserCreateCarWindow
;

implementation

    uses
        gtk2, glib2, 
        userHome,
        variables, interfaceTools, jsonTools,
        doubleLinkedList
    ;


    var
        createCarWindow: PGtkWidget;
        lblVehicleId, lblBrand, lblModel: PGtkWidget;
        entryVehicleId, entryBrand, entryModel: PGtkWidget;
        btnInsert, btnCancel: PGtkWidget
    ;


    // Insertar un vehiculo a la lista
    procedure OnInsertClick(widget: PGtkWidget; data: gpointer); cdecl;
        var
            vehicleId, brand, model: string;
        begin
            vehicleId := gtk_entry_get_text(GTK_ENTRY(entryVehicleId));
            brand := gtk_entry_get_text(GTK_ENTRY(entryBrand));
            model := gtk_entry_get_text(GTK_ENTRY(entryModel));

            doubleLinkedList.LDE_V_Insert(vehicleId, brand, model, current_user_email);

            ShowSuccessMessage(createCarWindow, 'Éxito', 'Vehículo ingresado correctamente');

            AddVehicleToJson(json_file_cars, 1, brand, model, current_user_name,current_user_email);

            gtk_widget_destroy(createCarWindow);
            ShowUserHomeWindow;
        end
    ;


    // Redireccion al home de usuario
    procedure OnCancelClick(widget: PGtkWidget; data: gpointer); cdecl;
        begin
        gtk_widget_destroy(createCarWindow);
        ShowUserHomeWindow;
        end
    ;


    // Mostrar la ventana de ingreso de vehículo
    procedure ShowUserCreateCarWindow;

        var
            grid: PGtkWidget
        ;

        begin
            gtk_init(@argc, @argv);

            // Crear ventana principal
            createCarWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
            gtk_window_set_title(GTK_WINDOW(createCarWindow), 'Ingresar Vehículo');
            gtk_container_set_border_width(GTK_CONTAINER(createCarWindow), 10);
            gtk_window_set_default_size(GTK_WINDOW(createCarWindow), 300, 350);

            // Crear una tabla para organizar widgets (5 filas x 1 columna)
            grid := gtk_table_new(5, 1, False);
            gtk_container_add(GTK_CONTAINER(createCarWindow), grid);

            // Etiqueta y campo para ID Vehículo
            lblVehicleId := gtk_label_new('ID Vehículo');
            gtk_table_attach_defaults(GTK_TABLE(grid), lblVehicleId, 0, 1, 0, 1);

            entryVehicleId := gtk_entry_new;
            gtk_entry_set_text(GTK_ENTRY(entryVehicleId), 'Ingrese ID del vehículo');
            gtk_table_attach_defaults(GTK_TABLE(grid), entryVehicleId, 0, 1, 1, 2);

            // Etiqueta y campo para Marca
            lblBrand := gtk_label_new('Marca');
            gtk_table_attach_defaults(GTK_TABLE(grid), lblBrand, 0, 1, 2, 3);

            entryBrand := gtk_entry_new;
            gtk_entry_set_text(GTK_ENTRY(entryBrand), 'Ingrese marca');
            gtk_table_attach_defaults(GTK_TABLE(grid), entryBrand, 0, 1, 3, 4);

            // Etiqueta y campo para Modelo
            lblModel := gtk_label_new('Modelo');
            gtk_table_attach_defaults(GTK_TABLE(grid), lblModel, 0, 1, 4, 5);

            entryModel := gtk_entry_new;
            gtk_entry_set_text(GTK_ENTRY(entryModel), 'Ingrese modelo');
            gtk_table_attach_defaults(GTK_TABLE(grid), entryModel, 0, 1, 5, 6);

            // Botón Insertar
            btnInsert := gtk_button_new_with_label('Insertar');
            g_signal_connect(btnInsert, 'clicked', G_CALLBACK(@OnInsertClick), nil);
            gtk_table_attach_defaults(GTK_TABLE(grid), btnInsert, 0, 1, 6, 7);

            // Botón Cancelar
            btnCancel := gtk_button_new_with_label('Cancelar');
            g_signal_connect(btnCancel, 'clicked', G_CALLBACK(@OnCancelClick), nil);
            gtk_table_attach_defaults(GTK_TABLE(grid), btnCancel, 0, 1, 7, 8);

            // Mostrar todos los widgets
            gtk_widget_show_all(createCarWindow);

            // Ejecutar el loop principal de GTK
            gtk_main;

        end
    ;

end.