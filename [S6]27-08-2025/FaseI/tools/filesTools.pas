unit filesTools;

{$mode objfpc}{$H+}

interface

    uses
        SysUtils;

    procedure GenerateReports(const ReportName, FolderName, DotCode: string);

implementation

    uses
        Process, Classes
    ;

    procedure GenerateReports(const ReportName, FolderName, DotCode: string);
        var
            DotFilePath, PngFilePath: string;
            FolderCreated: Boolean;
            DotFile: Text;
            CmdOutput: AnsiString;
            SL: TStringList;
            i: Integer;
        begin
            // Crear carpeta si no existe
            if not DirectoryExists(FolderName) then
            begin
                FolderCreated := CreateDir(FolderName);
                if not FolderCreated then
                begin
                    Writeln('Error: No se pudo crear la carpeta "', FolderName, '"');
                    Exit;
                end;
            end;

            // Definir rutas completas para archivos
            DotFilePath := FolderName + PathDelim + ReportName + '.dot';
            PngFilePath := FolderName + PathDelim + ReportName + '.png';

            // Guardar el código DOT en el archivo línea por línea
            SL := TStringList.Create;
            SL.Text := DotCode;
            Assign(DotFile, DotFilePath);
            Rewrite(DotFile);
            for i := 0 to SL.Count - 1 do
                Writeln(DotFile, SL[i]);
            Close(DotFile);
            SL.Free;

            // Ejecutar Graphviz para generar imagen PNG
            if RunCommand('dot', ['-Tpng', DotFilePath, '-o', PngFilePath], CmdOutput) then
                Writeln('Reporte generado correctamente: ', PngFilePath)
            else
            begin
                Writeln('Error al generar la imagen.');
                Writeln('Detalles: ', CmdOutput);
            end;
        end
    ;


end.
