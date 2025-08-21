unit rootWorkZones;

interface
    procedure ShowWorkZonesWindow
;

implementation

    uses
        Classes, SysUtils, gtk2, glib2, rootHome
    ;


    var
        workZonesWindow: PGtkWidget;
        frameCreate, frameAddClient, btnCancel: PGtkWidget;
        entryWorkspaceName, entryWorkspaceNameClient, entryClientEmail: PGtkWidget;
        btnSaveWorkspace, btnAddClient: PGtkWidget
    ;


    // Crear un nuevo espacio y lo inserta a la lista
    procedure OnSaveWorkspaceClick(widget: PGtkWidget; data: gpointer); cdecl;
        begin
        g_print(PChar('Espacio de trabajo guardado!' + LineEnding));
        end
    ;


    // Agrega un cliente a un espacio
    procedure OnAddClientClick(widget: PGtkWidget; data: gpointer); cdecl;
        begin
        g_print(PChar('Cliente agregado al espacio!' + LineEnding));
        end
    ;


    // Redirecciona a home root.
    procedure OnCancelClick(widget: PGtkWidget; data: gpointer); cdecl;
        begin
        gtk_widget_destroy(workZonesWindow);
        ShowRootHomeWindow;
        end
    ;


    // Mostrar la ventana de zonas de trabajo
    procedure ShowWorkZonesWindow;

        var
            grid, vboxCreate, vboxAddClient: PGtkWidget
        ;

        begin
            gtk_init(@argc, @argv);

            // Crear ventana principal
            workZonesWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
            gtk_window_set_title(GTK_WINDOW(workZonesWindow), 'Zonas de Trabajo');
            gtk_container_set_border_width(GTK_CONTAINER(workZonesWindow), 10);
            gtk_window_set_default_size(GTK_WINDOW(workZonesWindow), 400, 300);

            // Crear una tabla para organizar widgets 
            grid := gtk_table_new(3, 1, False);
            gtk_container_add(GTK_CONTAINER(workZonesWindow), grid);

            // ----------------- PARTE 1: Crear Espacio de Trabajo -----------------
            frameCreate := gtk_frame_new('Crear Espacio de Trabajo');
            gtk_table_attach_defaults(GTK_TABLE(grid), frameCreate, 0, 1, 0, 1);

            vboxCreate := gtk_vbox_new(False, 5);
            gtk_container_add(GTK_CONTAINER(frameCreate), vboxCreate);

            entryWorkspaceName := gtk_entry_new;
            gtk_entry_set_text(GTK_ENTRY(entryWorkspaceName), 'Nombre del Espacio');
            gtk_box_pack_start(GTK_BOX(vboxCreate), entryWorkspaceName, False, False, 0);

            btnSaveWorkspace := gtk_button_new_with_label('Guardar Espacio');
            g_signal_connect(btnSaveWorkspace, 'clicked', G_CALLBACK(@OnSaveWorkspaceClick), nil);
            gtk_box_pack_start(GTK_BOX(vboxCreate), btnSaveWorkspace, False, False, 0);

            // ----------------- PARTE 2: Agregar Cliente -----------------
            frameAddClient := gtk_frame_new('Agregar Cliente a Espacio');
            gtk_table_attach_defaults(GTK_TABLE(grid), frameAddClient, 0, 1, 1, 2);

            vboxAddClient := gtk_vbox_new(False, 5);
            gtk_container_add(GTK_CONTAINER(frameAddClient), vboxAddClient);

            entryWorkspaceNameClient := gtk_entry_new;
            gtk_entry_set_text(GTK_ENTRY(entryWorkspaceNameClient), 'Nombre del Espacio');
            gtk_box_pack_start(GTK_BOX(vboxAddClient), entryWorkspaceNameClient, False, False, 0);

            entryClientEmail := gtk_entry_new;
            gtk_entry_set_text(GTK_ENTRY(entryClientEmail), 'Correo del Cliente');
            gtk_box_pack_start(GTK_BOX(vboxAddClient), entryClientEmail, False, False, 0);

            btnAddClient := gtk_button_new_with_label('Agregar Cliente');
            g_signal_connect(btnAddClient, 'clicked', G_CALLBACK(@OnAddClientClick), nil);
            gtk_box_pack_start(GTK_BOX(vboxAddClient), btnAddClient, False, False, 0);

            // ----------------- Botón Cancelar -----------------
            btnCancel := gtk_button_new_with_label('Cancelar');
            g_signal_connect(btnCancel, 'clicked', G_CALLBACK(@OnCancelClick), nil);
            gtk_table_attach_defaults(GTK_TABLE(grid), btnCancel, 0, 1, 2, 3);

            // Mostrar todos los widgets
            gtk_widget_show_all(workZonesWindow);

            // Ejecutar el loop principal de GTK
            gtk_main;
        end
    ;

end.