unit userVehicleDetails;

interface
    procedure ShowVehicleDetailsWindow(vehicleId: string);

implementation

    uses
        gtk2, glib2, SysUtils,
        doubleLinkedList, variables;

    var
        detailsWindow: PGtkWidget;

    procedure ShowVehicleDetailsWindow(vehicleId: string);
    var
        vehicle: TVehicleData;
        grid: PGtkWidget;
        lblId, lblMarca, lblModelo, lblPropietario: PGtkWidget;
    begin
        // Obtener los datos del vehículo por su ID
        vehicle := doubleLinkedList.LDE_V_GetVehicleById(vehicleId);

        // Crear ventana
        detailsWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(detailsWindow), 'Detalles del Vehículo');
        gtk_container_set_border_width(GTK_CONTAINER(detailsWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(detailsWindow), 300, 200);

        // Crear tabla para mostrar la información
        grid := gtk_table_new(4, 2, False);
        gtk_container_add(GTK_CONTAINER(detailsWindow), grid);

        // Crear etiquetas con títulos
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('ID:'), 0, 1, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Marca:'), 0, 1, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Modelo:'), 0, 1, 2, 3);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Propietario:'), 0, 1, 3, 4);

        // Crear etiquetas con los datos reales
        lblId := gtk_label_new(PChar(vehicle.id));
        gtk_table_attach_defaults(GTK_TABLE(grid), lblId, 1, 2, 0, 1);

        lblMarca := gtk_label_new(PChar(vehicle.marca));
        gtk_table_attach_defaults(GTK_TABLE(grid), lblMarca, 1, 2, 1, 2);

        lblModelo := gtk_label_new(PChar(vehicle.modelo));
        gtk_table_attach_defaults(GTK_TABLE(grid), lblModelo, 1, 2, 2, 3);

        lblPropietario := gtk_label_new(PChar(vehicle.propietario));
        gtk_table_attach_defaults(GTK_TABLE(grid), lblPropietario, 1, 2, 3, 4);

        // Mostrar todos los widgets
        gtk_widget_show_all(detailsWindow);

        // Evento de cerrar
        g_signal_connect(detailsWindow, 'destroy', G_CALLBACK(@gtk_widget_destroy), detailsWindow);
    end;

end.
