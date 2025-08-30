{$MODE DELPHI}

unit linkedListOfLists;

interface

uses
    SysUtils, Classes;

type
    // Estructura para los datos de una celda
    TCellData = record
        name: string;
    end;

    // Estructura para los datos de una cabecera
    THeaderData = record
        name: string;
    end;

    // Declaración de funciones públicas
    procedure LL_InsertHeader(name: string);
    procedure LL_InsertElement(headerName, cellName: string);
    function LL_GenerateDot: string;
    procedure LL_Clear;

implementation

type
    // Definición del nodo Cell (celda)
    PCell = ^TCell;
    TCell = record
        name: string;
        next: PCell;
    end;

    // Definición del nodo Header (cabecera)
    PHeader = ^THeader;
    THeader = record
        name: string;
        cells: PCell; // Puntero a la lista de celdas
        next: PHeader;
    end;

var
    // Puntero al inicio de la lista de cabeceras
    HeaderList: PHeader = nil;

    // Función auxiliar para escapar caracteres especiales en Graphviz
    function EscapeDotString(const S: string): string;
    var
        Res: string;
        i: Integer;
    begin
        Res := '';
        for i := 1 to Length(S) do
        begin
            case S[i] of
                '"': Res := Res + '\"';
                '\': Res := Res + '\\';
                '|': Res := Res + '\|';
                '{': Res := Res + '\{';
                '}': Res := Res + '\}';
                #10: Res := Res + '\n';
                #13: Res := Res + '\n';
            else
                Res := Res + S[i];
            end;
        end;
        Result := Res;
    end;

    // Inserta una nueva cabecera en la lista de cabeceras
    procedure LL_InsertHeader(name: string);
    var
        newHeader, current: PHeader;
    begin
        // Crear un nuevo nodo Header
        New(newHeader);
        newHeader^.name := Trim(name);
        newHeader^.cells := nil; // Inicialmente sin celdas
        newHeader^.next := nil;

        // Si la lista de cabeceras está vacía, establecer como cabeza
        if HeaderList = nil then
            HeaderList := newHeader
        else
        begin
            // Verificar si ya existe una cabecera con ese nombre
            current := HeaderList;
            while current <> nil do
            begin
                if SameText(current^.name, newHeader^.name) then
                begin
                    Writeln('Error: La cabecera "', name, '" ya existe.');
                    Dispose(newHeader);
                    Exit;
                end;
                current := current^.next;
            end;

            // Insertar al final de la lista de cabeceras
            current := HeaderList;
            while current^.next <> nil do
                current := current^.next;
            current^.next := newHeader;
        end;
    end;

    // Inserta una celda en la lista de celdas de una cabecera específica
    procedure LL_InsertElement(headerName, cellName: string);
    var
        currentHeader: PHeader;
        newCell, currentCell: PCell;
    begin
        // Buscar la cabecera con el nombre dado
        currentHeader := HeaderList;
        while (currentHeader <> nil) and (currentHeader^.name <> Trim(headerName)) do
            currentHeader := currentHeader^.next;

        // Si no se encuentra la cabecera, salir
        if currentHeader = nil then
        begin
            Writeln('Error: No se encontró la cabecera ', headerName);
            Exit;
        end;

        // Crear un nuevo nodo Cell
        New(newCell);
        newCell^.name := Trim(cellName);
        newCell^.next := nil;

        // Si la lista de celdas está vacía, establecer como cabeza
        if currentHeader^.cells = nil then
            currentHeader^.cells := newCell
        else
        begin
            // Insertar al final de la lista de celdas
            currentCell := currentHeader^.cells;
            while currentCell^.next <> nil do
                currentCell := currentCell^.next;
            currentCell^.next := newCell;
        end;
    end;

    // Genera el código DOT para visualizar la lista de listas
    function LL_GenerateDot: string;
    var
        SL: TStringList;
        currentHeader: PHeader;
        currentCell: PCell;
        headerCounter, cellCounter: Integer;
        headerNode, cellNode, nextNode: string;
        resultText: string;
    begin
        SL := TStringList.Create;

        // Iniciar el grafo DOT
        SL.Add('digraph ListaDeListas {');
        SL.Add('  rankdir=LR;');
        SL.Add('  nodesep=0.5;');
        SL.Add('  edge [arrowhead=normal];');
        SL.Add('  node [shape=record, style=filled, fillcolor=lightyellow];');
        SL.Add('');

        // Si la lista de cabeceras está vacía
        if HeaderList = nil then
            SL.Add('  null [label="VACÍA", shape=plaintext];')
        else
        begin
            // Recorrer las cabeceras
            headerCounter := 0;
            currentHeader := HeaderList;
            while currentHeader <> nil do
            begin
                // Definir el nodo de la cabecera
                headerNode := Format('header%d', [headerCounter]);
                SL.Add(Format('  %s [label="%s", fillcolor=lightblue];',
                    [headerNode, EscapeDotString(currentHeader^.name)]));

                // Conectar cabeceras en secuencia
                if currentHeader^.next <> nil then
                begin
                    nextNode := Format('header%d', [headerCounter + 1]);
                    SL.Add(Format('  %s -> %s;', [headerNode, nextNode]));
                end;

                // Recorrer las celdas de la cabecera
                if currentHeader^.cells <> nil then
                begin
                    cellCounter := 0;
                    currentCell := currentHeader^.cells;
                    cellNode := Format('cell%d_%d', [headerCounter, cellCounter]);

                    // Conectar cabecera con la primera celda
                    SL.Add(Format('  %s -> %s;', [headerNode, cellNode]));

                    while currentCell <> nil do
                    begin
                        cellNode := Format('cell%d_%d', [headerCounter, cellCounter]);
                        SL.Add(Format('  %s [label="%s"];',
                            [cellNode, EscapeDotString(currentCell^.name)]));

                        // Conectar celdas en secuencia
                        if currentCell^.next <> nil then
                        begin
                            nextNode := Format('cell%d_%d', [headerCounter, cellCounter + 1]);
                            SL.Add(Format('  %s -> %s;', [cellNode, nextNode]));
                        end;

                        Inc(cellCounter);
                        currentCell := currentCell^.next;
                    end;

                    // Agrupar cabecera + celdas en la misma fila (como tabla)
                    SL.Add('{ rank=same; ' + headerNode);
                    for cellCounter := 0 to cellCounter - 1 do
                        SL.Add(' ' + Format('cell%d_%d', [headerCounter, cellCounter]));
                    SL.Add(' }');
                end;

                Inc(headerCounter);
                currentHeader := currentHeader^.next;
            end;
        end;

        SL.Add('}');
        resultText := SL.Text;
        SL.Free;

        Result := resultText;
    end;

    // Libera toda la memoria de la lista de listas
    procedure LL_Clear;
    var
        currentHeader, tempHeader: PHeader;
        currentCell, tempCell: PCell;
    begin
        currentHeader := HeaderList;
        while currentHeader <> nil do
        begin
            // Liberar celdas de cada cabecera
            currentCell := currentHeader^.cells;
            while currentCell <> nil do
            begin
                tempCell := currentCell;
                currentCell := currentCell^.next;
                Dispose(tempCell);
            end;

            // Liberar cabecera
            tempHeader := currentHeader;
            currentHeader := currentHeader^.next;
            Dispose(tempHeader);
        end;
        HeaderList := nil;
    end;

end.
