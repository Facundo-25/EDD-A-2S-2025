unit rootCreateUser;

interface
  procedure ShowRootCreateUserWindow
;

implementation

    uses
        SysUtils,gtk2, glib2, gdk2, 
        rootHome,
        interfaceTools, jsonTools,
        simpleLinkedList, variables
    ;    

    var
        createUserWindow: PGtkWidget;
        entryID, entryName, entryUser, entryEmail, entryPhone, entryPass: PGtkWidget
    ;
    
    // Evento para registrar el usuario nuevo.
    procedure OnRegisterButtonClick(widget: PGtkWidget; data: gpointer); cdecl;
        var
            value_entryId, value_entryName, value_entryUser, value_entryEmail, value_entryPhone, value_entryPass : PChar;
            statusUpdloadJson : Boolean;
        begin
            // Obtener los valores
            value_entryId := PChar(gtk_entry_get_text(GTK_ENTRY(entryID)));
            value_entryName := PChar(gtk_entry_get_text(GTK_ENTRY(entryName)));
            value_entryUser := PChar(gtk_entry_get_text(GTK_ENTRY(entryUser)));
            value_entryEmail := PChar(gtk_entry_get_text(GTK_ENTRY(entryEmail)));
            value_entryPhone := PChar(gtk_entry_get_text(GTK_ENTRY(entryPhone)));
            value_entryPass := PChar(gtk_entry_get_text(GTK_ENTRY(entryPass)));
            
            // Insertar el usuario en la lista
            LSL_U_Insert(value_entryId, value_entryName, value_entryEmail, value_entryUser, value_entryPhone, value_entryPass);
            statusUpdloadJson := AddUserToJson(json_file_path, StrToInt(value_entryId), value_entryName, value_entryEmail, value_entryUser, value_entryPhone, value_entryPass);

            // Verificar si la carga fue exitosa
            if not statusUpdloadJson then
                begin
                    ShowErrorMessage(createUserWindow,'Error Carga en Archivo Json','No se pudo registrar el usuario.');
                    exit;
                end
            ;

            // Cerramos la ventana y regresamos al login
            ShowSuccessMessage(createUserWindow,'Registro Exitoso','El usuario ha sido registrado correctamente.');
            gtk_widget_destroy(createUserWindow);
            ShowRootHomeWindow();
        end
    ;

    // Evento para cancelar el evento.
    procedure OnCancelButtonClick(widget: PGtkWidget; data: gpointer); cdecl;
        begin
        gtk_widget_destroy(createUserWindow);  
        ShowRootHomeWindow;
        end
    ;

    procedure ShowRootCreateUserWindow;
        var
        grid: PGtkWidget;
        lblID, lblName, lblUser, lblEmail, lblPhone, lblPass: PGtkWidget;
        btnRegister, btnCancel: PGtkWidget;
        begin
        gtk_init(@argc, @argv);

        // Crear ventana principal
        createUserWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(createUserWindow), 'Create User');
        gtk_container_set_border_width(GTK_CONTAINER(createUserWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(createUserWindow), 400, 300);

        // Crear tabla para organizar widgets
        grid := gtk_table_new(7, 2, False);
        gtk_container_add(GTK_CONTAINER(createUserWindow), grid);

        // Crear etiquetas para cada campo
        lblID := gtk_label_new('ID:');
        lblName := gtk_label_new('Name:');
        lblUser := gtk_label_new('User:');
        lblEmail := gtk_label_new('Email:');
        lblPhone := gtk_label_new('Phone:');
        lblPass := gtk_label_new('Password:');

        // Crear entradas de texto
        entryID := gtk_entry_new;
        entryName := gtk_entry_new;
        entryUser := gtk_entry_new;
        entryEmail := gtk_entry_new;
        entryPhone := gtk_entry_new;
        entryPass := gtk_entry_new;
        gtk_entry_set_visibility(GTK_ENTRY(entryPass), False);  

        // Crear botones
        btnRegister := gtk_button_new_with_label('Register');
        btnCancel := gtk_button_new_with_label('Cancel');

        // Conectar eventos de los botones
        g_signal_connect(btnRegister, 'clicked', G_CALLBACK(@OnRegisterButtonClick), nil);
        g_signal_connect(btnCancel, 'clicked', G_CALLBACK(@OnCancelButtonClick), nil);

        // Colocar widgets en la tabla (fila, columna)
        gtk_table_attach_defaults(GTK_TABLE(grid), lblID, 0, 1, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryID, 1, 2, 0, 1);

        gtk_table_attach_defaults(GTK_TABLE(grid), lblName, 0, 1, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryName, 1, 2, 1, 2);

        gtk_table_attach_defaults(GTK_TABLE(grid), lblUser, 0, 1, 2, 3);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryUser, 1, 2, 2, 3);

        gtk_table_attach_defaults(GTK_TABLE(grid), lblEmail, 0, 1, 3, 4);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryEmail, 1, 2, 3, 4);

        gtk_table_attach_defaults(GTK_TABLE(grid), lblPhone, 0, 1, 4, 5);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryPhone, 1, 2, 4, 5);

        gtk_table_attach_defaults(GTK_TABLE(grid), lblPass, 0, 1, 5, 6);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryPass, 1, 2, 5, 6);

        // Botones en la fila 6
        gtk_table_attach_defaults(GTK_TABLE(grid), btnCancel, 0, 1, 6, 7);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnRegister, 1, 2, 6, 7);

        // Mostrar todos los widgets
        gtk_widget_show_all(createUserWindow);

        // Conectar señal de cerrar ventana
        g_signal_connect(createUserWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

        // Iniciar ciclo principal GTK
        gtk_main;
        end
    ;

end.
