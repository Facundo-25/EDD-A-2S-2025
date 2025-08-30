unit userViewVehicles;

interface
    procedure ShowUserViewVehiclesWindow;

implementation

uses
    gtk2, glib2, SysUtils,
    doubleLinkedList, variables,
    userVehicleDetails;

var
    viewWindow: PGtkWidget;

procedure OnDetailsClick(widget: PGtkWidget; data: gpointer); cdecl;
var
    vehicleId: string;
begin
    vehicleId := PChar(data); 
    writeln('Detalles de vehículo: ', vehicleId);
    userVehicleDetails.ShowVehicleDetailsWindow(vehicleId);
end;

// Procedimiento para ordenar la lista y recargar la ventana
procedure OnSortClick(widget: PGtkWidget; data: gpointer); cdecl;
begin
    doubleLinkedList.LDE_V_QuickSortById();
    gtk_widget_destroy(viewWindow); 
    ShowUserViewVehiclesWindow;     
end;

// Mostrar tabla con los vehículos del usuario
procedure ShowUserViewVehiclesWindow;
var
    grid, scroll, table: PGtkWidget;
    vehicles: doubleLinkedList.TVehicleArray;
    i: Integer;
    lblId, lblMarca, lblModelo: PGtkWidget;
    btnDetails, btnSort: PGtkWidget;
begin
    vehicles := doubleLinkedList.LDE_V_GetVehiclesByOwner(variables.current_user_email);

    // Crear ventana
    viewWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(viewWindow), 'Mis Vehículos');
    gtk_container_set_border_width(GTK_CONTAINER(viewWindow), 10);
    gtk_window_set_default_size(GTK_WINDOW(viewWindow), 500, 400);

    // Crear tabla principal (grid) para botón y scroll
    grid := gtk_table_new(2, 1, False);
    gtk_container_add(GTK_CONTAINER(viewWindow), grid);

    // Botón Ordenar
    btnSort := gtk_button_new_with_label('Ordenar por ID');
    g_signal_connect(btnSort, 'clicked', G_CALLBACK(@OnSortClick), nil);
    gtk_table_attach_defaults(GTK_TABLE(grid), btnSort, 0, 1, 0, 1);

    // Contenedor con scroll para la tabla de vehículos
    scroll := gtk_scrolled_window_new(nil, nil);
    gtk_table_attach_defaults(GTK_TABLE(grid), scroll, 0, 1, 1, 2);

    // Crear tabla de vehículos
    table := gtk_table_new(Length(vehicles) + 1, 4, False);
    gtk_scrolled_window_add_with_viewport(GTK_SCROLLED_WINDOW(scroll), table);

    // Encabezados
    lblId := gtk_label_new('ID');
    gtk_table_attach_defaults(GTK_TABLE(table), lblId, 0, 1, 0, 1);

    lblMarca := gtk_label_new('Marca');
    gtk_table_attach_defaults(GTK_TABLE(table), lblMarca, 1, 2, 0, 1);

    lblModelo := gtk_label_new('Modelo');
    gtk_table_attach_defaults(GTK_TABLE(table), lblModelo, 2, 3, 0, 1);

    lblModelo := gtk_label_new('Detalles');
    gtk_table_attach_defaults(GTK_TABLE(table), lblModelo, 3, 4, 0, 1);

    // Llenar tabla con los vehículos
    for i := 0 to High(vehicles) do
    begin
        lblId := gtk_label_new(PChar(vehicles[i].id));
        gtk_table_attach_defaults(GTK_TABLE(table), lblId, 0, 1, i+1, i+2);

        lblMarca := gtk_label_new(PChar(vehicles[i].marca));
        gtk_table_attach_defaults(GTK_TABLE(table), lblMarca, 1, 2, i+1, i+2);

        lblModelo := gtk_label_new(PChar(vehicles[i].modelo));
        gtk_table_attach_defaults(GTK_TABLE(table), lblModelo, 2, 3, i+1, i+2);

        btnDetails := gtk_button_new_with_label('Ver');
        g_signal_connect(btnDetails, 'clicked', G_CALLBACK(@OnDetailsClick), gpointer(PChar(vehicles[i].id)));
        gtk_table_attach_defaults(GTK_TABLE(table), btnDetails, 3, 4, i+1, i+2);
    end;

    // Mostrar todo
    gtk_widget_show_all(viewWindow);

    // Evento de cerrar
    g_signal_connect(viewWindow, 'destroy', G_CALLBACK(@gtk_widget_destroy), viewWindow);
end;

end.
