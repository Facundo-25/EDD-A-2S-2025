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

    procedure LDE_V_Insert(id, marca, modelo, propietario: string);
    function  LDE_V_GenerateDot: string;
    procedure LDE_V_Clear;

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

end.
