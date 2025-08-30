unit doubleLinkedList;

{$MODE DELPHI}

interface
    type
        TVehicleData = record
            id: string;
            marca: string;
            modelo: string;
            propietario: string;
        end;

        TVehicleArray = array of TVehicleData;

    procedure LDE_V_Insert(id, marca, modelo, propietario: string);
    function  LDE_V_GenerateDot: string;
    procedure LDE_V_Clear;
    function LDE_V_GetVehiclesByOwner(email: string): TVehicleArray;
    function LDE_V_GetVehicleById(id: string): TVehicleData;
    procedure LDE_V_QuickSortById;

implementation

    uses
        SysUtils, Classes;

    type
        PNode = ^TNode;
        TNode = record
            id: string;
            marca: string;
            modelo: string;
            propietario: string;
            Next: PNode;
            Prev: PNode;
        end;

    var
        Head: PNode = nil;
        Tail: PNode = nil;

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

    procedure LDE_V_Insert(id, marca, modelo, propietario: string);
    var
        NewNode: PNode;
    begin
        New(NewNode);
        NewNode^.id := Trim(id);
        NewNode^.marca := Trim(marca);
        NewNode^.modelo := Trim(modelo);
        NewNode^.propietario := Trim(propietario);
        NewNode^.Next := nil;
        NewNode^.Prev := nil;

        if Head = nil then
        begin
            Head := NewNode;
            Tail := NewNode;
        end
        else
        begin
            Tail^.Next := NewNode;
            NewNode^.Prev := Tail;
            Tail := NewNode;
        end;
    end;


    procedure LDE_V_Clear;
    var
        Current, Temp: PNode;
    begin
        Current := Head;
        while Current <> nil do
        begin
            Temp := Current;
            Current := Current^.Next;
            Dispose(Temp);
        end;
        Head := nil;
        Tail := nil;
    end;


    function LDE_V_GenerateDot: string;
    var
        SL: TStringList;
        Current: PNode;
        Counter: Integer;
        NodeName, NextName: string;
        ResultText: string;
    begin
        SL := TStringList.Create;

        SL.Add('digraph ListaDoble {');
        SL.Add('  rankdir=LR;');
        SL.Add('  nodesep=0.5;');
        SL.Add('');
        SL.Add('  subgraph cluster_0 {');
        SL.Add('    label="Lista doblemente enlazada de vehículos";');
        SL.Add('    fontsize=14;');
        SL.Add('    color=black;');
        SL.Add('    style=filled;');
        SL.Add('    fillcolor=white;');
        SL.Add('    node [shape=record, style=filled, fillcolor=lightyellow];');
        SL.Add('');

        if Head = nil then
            SL.Add('    null [label="VACÍA", shape=plaintext];')
        else
        begin
            Counter := 0;
            Current := Head;
            while Current <> nil do
            begin
                NodeName := Format('nodo%d', [Counter]);
                SL.Add(Format('    %s [label="{%s \n %s \n %s \n %s}"];',
                    [NodeName,
                    EscapeDotString(Current^.id),
                    EscapeDotString(Current^.marca),
                    EscapeDotString(Current^.modelo),
                    EscapeDotString(Current^.propietario)]));

                if Current^.Next <> nil then
                begin
                    NextName := Format('nodo%d', [Counter + 1]);
                    SL.Add(Format('    %s -> %s;', [NodeName, NextName]));
                    SL.Add(Format('    %s -> %s;', [NextName, NodeName]));
                end;

                Inc(Counter);
                Current := Current^.Next;
            end;
        end;

        SL.Add('  }');
        SL.Add('}');

        ResultText := SL.Text;
        SL.Free;

        Result := ResultText;
    end;


    function LDE_V_GetVehiclesByOwner(email: string): TVehicleArray;
    var
        Current: PNode;
        Vehicles: TVehicleArray;
        Count: Integer;
    begin
        Count := 0;
        Current := Head;
        SetLength(Vehicles, 0); 

        while Current <> nil do
        begin
            if SameText(Current^.propietario, email) then
            begin
                SetLength(Vehicles, Count + 1);
                Vehicles[Count].id := Current^.id;
                Vehicles[Count].marca := Current^.marca;
                Vehicles[Count].modelo := Current^.modelo;
                Vehicles[Count].propietario := Current^.propietario;
                Inc(Count);
            end;
            Current := Current^.Next;
        end;

        Result := Vehicles;
    end;


    function LDE_V_GetVehicleById(id: string): TVehicleData;
    var
        Current: PNode;
        Vehicle: TVehicleData;
    begin
        Vehicle.id := '';
        Vehicle.marca := '';
        Vehicle.modelo := '';
        Vehicle.propietario := '';

        Current := Head;
        while Current <> nil do
        begin
            if SameText(Current^.id, id) then
            begin
                Vehicle.id := Current^.id;
                Vehicle.marca := Current^.marca;
                Vehicle.modelo := Current^.modelo;
                Vehicle.propietario := Current^.propietario;
                Break;
            end;
            Current := Current^.Next;
        end;

        Result := Vehicle;
    end;


    procedure LDE_V_QuickSortById;
    type
        // Definimos un tipo de arreglo dinámico de punteros a nodos
        PNodeArray = array of PNode;

        // Procedimiento recursivo para ordenar el arreglo usando Quicksort
        procedure QuickSort(var Arr: PNodeArray; Low, High: Integer);
        var
            i, j: Integer;       // índices para recorrer el arreglo
            Pivot, Temp: PNode;  // pivot para comparación y temp para intercambio
        begin
            i := Low;                         // inicializamos i en el límite inferior
            j := High;                        // inicializamos j en el límite superior
            Pivot := Arr[(Low + High) div 2]; // elegimos el pivote como el elemento del medio

            repeat
                // Avanzar i mientras el id del nodo sea menor que el pivote
                while Arr[i]^.id < Pivot^.id do Inc(i);
                // Retroceder j mientras el id del nodo sea mayor que el pivote
                while Arr[j]^.id > Pivot^.id do Dec(j);

                if i <= j then
                begin
                    // Intercambiar Arr[i] y Arr[j]
                    Temp := Arr[i];
                    Arr[i] := Arr[j];
                    Arr[j] := Temp;

                    Inc(i); // mover i a la derecha
                    Dec(j); // mover j a la izquierda
                end;
            until i > j; // repetir hasta que los índices se crucen

            // Llamadas recursivas a las sublistas izquierda y derecha
            if Low < j then QuickSort(Arr, Low, j);
            if i < High then QuickSort(Arr, i, High);
        end;

    var
        Arr: PNodeArray; // arreglo dinámico de punteros a nodos
        Current: PNode;  // puntero auxiliar para recorrer la lista
        i: Integer;      // contador / índice
    begin
        // Contar la cantidad de nodos en la lista
        Current := Head;
        i := 0;
        while Current <> nil do
        begin
            Inc(i);           // aumentar contador
            Current := Current^.Next; // pasar al siguiente nodo
        end;

        if i < 2 then Exit; // si hay 0 o 1 nodo, ya está ordenada

        // Crear arreglo y llenarlo con punteros a los nodos
        SetLength(Arr, i);  // dimensionar el arreglo
        Current := Head;    // volver al inicio de la lista
        for i := 0 to High(Arr) do
        begin
            Arr[i] := Current;       // guardar el puntero al nodo
            Current := Current^.Next; // avanzar al siguiente
        end;

        // Ordenar el arreglo por id usando Quicksort
        QuickSort(Arr, 0, High(Arr));

        // Reconstruir la lista doblemente enlazada con el orden del arreglo
        Head := Arr[0];      // primer nodo ahora es el primero del arreglo
        Head^.Prev := nil;   // no hay nodo anterior al primero
        for i := 0 to High(Arr)-1 do
        begin
            Arr[i]^.Next := Arr[i+1]; // apuntar al siguiente nodo
            Arr[i+1]^.Prev := Arr[i]; // apuntar al anterior nodo
        end;
        Tail := Arr[High(Arr)]; // el último nodo del arreglo es la cola
        Tail^.Next := nil;       // no hay nodo después del último
    end;

end.