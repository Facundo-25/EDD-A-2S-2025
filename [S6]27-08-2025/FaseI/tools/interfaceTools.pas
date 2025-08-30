unit interfaceTools;


interface

    uses
        gtk2, glib2, gdk2
    ;

    procedure ShowSuccessMessage(parent: PGtkWidget; title, msg: AnsiString);
    procedure ShowErrorMessage(parent: PGtkWidget; title, msg: AnsiString)
;


implementation
    

    // Procedimiento para mostrar mensaje de éxito
    procedure ShowSuccessMessage(parent: PGtkWidget; title, msg: AnsiString);
        var
            dialog: PGtkWidget;
        begin
            dialog := gtk_message_dialog_new(
                GTK_WINDOW(parent),                
                GTK_DIALOG_MODAL,                  
                GTK_MESSAGE_INFO,                  
                GTK_BUTTONS_OK,                    
                PChar(msg)                         
            );
            gtk_window_set_title(GTK_WINDOW(dialog), PChar(title));
            gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER_ON_PARENT);
            gtk_dialog_run(GTK_DIALOG(dialog));
            gtk_widget_destroy(dialog);
        end
    ;


    // Procedimiento para mostrar mensaje de error
    procedure ShowErrorMessage(parent: PGtkWidget; title, msg: AnsiString);
        var
            dialog: PGtkWidget;
        begin
            dialog := gtk_message_dialog_new(
                GTK_WINDOW(parent),                
                GTK_DIALOG_MODAL,                  
                GTK_MESSAGE_ERROR,                 
                GTK_BUTTONS_OK,                    
                PChar(msg)                         
            );
            gtk_window_set_title(GTK_WINDOW(dialog), PChar(title));
            gtk_window_set_position(GTK_WINDOW(dialog), GTK_WIN_POS_CENTER_ON_PARENT);
            gtk_dialog_run(GTK_DIALOG(dialog));
            gtk_widget_destroy(dialog);
        end
    ;

end.
