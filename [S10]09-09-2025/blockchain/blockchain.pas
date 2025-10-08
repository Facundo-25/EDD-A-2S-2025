program BlockchainWithMailDot;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords} 

uses
  SysUtils, Classes, DateUtils;

type

  // Contenido del correo
  TMail = record
    Emisor: string;
    Receptor: string;
    Mensaje: string;
    function ToSingleLine: string;  // Convierte los datos del correo en una sola línea de texto
  end;


  // Representacion de un bloque
  TBlock = class
  public
    Index: Integer;           // Número del bloque
    Timestamp: string;        // Fecha y hora en formato texto
    Mail: TMail;              // Datos: emisor, receptor, mensaje
    PrevHash: string;         // Hash del bloque anterior
    Hash: string;             // Hash actual 
    constructor Create(AIndex: Integer; AMail: TMail; APrevHash: string); // Constructor
    function CalculateHash: string;   // Calcula el hash del bloque actual
  end;


  // Representa toda la cadena de bloques
  TBlockchain = class
  private
    FChain: TList;    // Lista que almacena todos los bloques de la cadena
  public
    constructor Create;                         // Inicializa la cadena y crea el bloque génesis
    destructor Destroy; override;               // Libera la memoria usada por los bloques
    procedure AddBlock(AMail: TMail);           // Agrega un nuevo bloque con datos de correo
    function GetBlock(Index: Integer): TBlock;  // Devuelve un bloque según su índice
    function IsValid: Boolean;                  // Verifica la integridad de toda la cadena
    procedure PrintChain;                       // Muestra todos los bloques y sus datos en consola
    procedure ExportToDot(const AFilename: string);  // Crea un archivo .dot para Graphviz
    function Count: Integer;                    // Devuelve el número total de bloques en la cadena
  end;


  // Implementación de TMail
  function TMail.ToSingleLine: string;
  var
    S: string;
  begin
    S := Trim(Emisor) + ' -> ' + Trim(Receptor) + ': ' + Trim(Mensaje);
    S := StringReplace(S, '"', '\"', [rfReplaceAll]);
    S := StringReplace(S, #13#10, ' ', [rfReplaceAll]);
    S := StringReplace(S, #10, ' ', [rfReplaceAll]);
    S := StringReplace(S, #13, ' ', [rfReplaceAll]);
    Result := S;
  end;


  //Implementación de TBlock
  constructor TBlock.Create(AIndex: Integer; AMail: TMail; APrevHash: string);
  begin
    Index := AIndex;            // Asigna el número de bloque pasado como parámetro
    Mail := AMail;              // Guarda los datos del correo en el bloque
    PrevHash := APrevHash;      // Guarda el hash del bloque anterior para enlazar la cadena
    Timestamp := DateTimeToStr(Now); // Guarda la fecha y hora de creación del bloque
    Hash := CalculateHash;      // Calcula y asigna el hash del bloque actual
  end;


  function TBlock.CalculateHash: string;
  var
    Combined: string;  // Variable que guarda todos los datos del bloque concatenados
    I, Sum: LongInt;   // Contadores y acumulador para calcular el hash
  begin
    Combined := IntToStr(Index) + Timestamp + Mail.ToSingleLine + PrevHash; // Une los campos importantes del bloque
    Sum := 0;  // Inicializa la suma en 0
    for I := 1 to Length(Combined) do
      Sum := Sum + Ord(Combined[I]);  // Suma el valor ASCII de cada carácter
    Result := IntToHex(Sum, 8);      // Convierte la suma a hexadecimal (8 dígitos) como "hash educativo"
  end;


  constructor TBlockchain.Create;
  var
    GenesisMail: TMail;  // Variable para crear el correo del bloque génesis
    Genesis: TBlock;     // Variable para crear el bloque génesis
  begin
    FChain := TList.Create;  // Crea la lista que almacenará todos los bloques

    // Creamos el "correo" del bloque génesis
    GenesisMail.Emisor := 'genesis';      // Emisor ficticio
    GenesisMail.Receptor := 'network';    // Receptor ficticio
    GenesisMail.Mensaje := 'Bloque Génesis'; // Mensaje inicial

    // Creamos el bloque génesis con índice 0 y PrevHash = '0'
    Genesis := TBlock.Create(0, GenesisMail, '0');

    FChain.Add(Genesis);  // Agregamos el bloque génesis a la cadena
  end;


  destructor TBlockchain.Destroy;
  var
    I: Integer;  // Contador para recorrer la lista de bloques
  begin
    // Liberamos la memoria de cada bloque en la lista
    for I := 0 to FChain.Count - 1 do
      TObject(FChain[I]).Free;

    FChain.Free;          // Liberamos la memoria de la lista de bloques
    inherited Destroy;    // Llamamos al destructor de la clase base
  end;


  procedure TBlockchain.AddBlock(AMail: TMail);
  var
    PrevBlock, NewBlock: TBlock;  // Variables para el bloque anterior y el nuevo
  begin
    PrevBlock := TBlock(FChain.Last);  // Tomamos el último bloque de la cadena
    NewBlock := TBlock.Create(PrevBlock.Index + 1, AMail, PrevBlock.Hash); // Creamos un nuevo bloque con índice siguiente y PrevHash del bloque anterior
    FChain.Add(NewBlock);  // Agregamos el nuevo bloque a la cadena
  end;


  function TBlockchain.GetBlock(Index: Integer): TBlock;
  begin
    Result := TBlock(FChain[Index]);  // Devuelve el bloque que está en la posición Index
  end;


  function TBlockchain.Count: Integer;
  begin
    Result := FChain.Count;  // Devuelve la cantidad total de bloques en la cadena
  end;


  function TBlockchain.IsValid: Boolean;
  var
    I: Integer;           // Contador para recorrer la cadena
    CurrentBlock, PrevBlock: TBlock;  // Variables para comparar bloques
  begin
    Result := True;  // Inicialmente asumimos que la cadena es válida

    for I := 1 to FChain.Count - 1 do
    begin
      CurrentBlock := TBlock(FChain[I]);        // Bloque actual
      PrevBlock := TBlock(FChain[I - 1]);       // Bloque anterior

      // Verifica que el hash del bloque actual coincida con el hash recalculado
      if CurrentBlock.Hash <> CurrentBlock.CalculateHash then
      begin
        WriteLn('Error: El hash del bloque ', CurrentBlock.Index, ' no coincide con su cálculo.');
        Result := False;
        Exit;
      end;

      // Verifica que el bloque actual esté enlazado correctamente con el anterior
      if CurrentBlock.PrevHash <> PrevBlock.Hash then
      begin
        WriteLn('Error: El bloque ', CurrentBlock.Index, ' no está enlazado correctamente con el anterior.');
        Result := False;
        Exit;
      end;
    end;
  end;


  procedure TBlockchain.PrintChain;
  var
    I: Integer;
    B: TBlock;
  begin
    WriteLn('=== CADENA DE BLOQUES (Mail Blockchain) ===');
    for I := 0 to FChain.Count - 1 do
    begin
      B := TBlock(FChain[I]);
      WriteLn('-----------------------------');
      WriteLn('Bloque #: ', B.Index);
      WriteLn('Fecha: ', B.Timestamp);
      WriteLn('Emisor: ', B.Mail.Emisor);
      WriteLn('Receptor: ', B.Mail.Receptor);
      WriteLn('Mensaje: ', B.Mail.Mensaje);
      WriteLn('Hash previo: ', B.PrevHash);
      WriteLn('Hash actual: ', B.Hash);
    end;
    WriteLn('-----------------------------');
  end;


  procedure TBlockchain.ExportToDot(const AFilename: string);
  var
    SL: TStringList;
    I: Integer;
    B: TBlock;
    NodeLabel, SafeMsg, NodeName: string;
  begin
    SL := TStringList.Create;
    try
      SL.Add('digraph Blockchain {');
      SL.Add('  rankdir=LR;');
      SL.Add('  node [shape=box, fontsize=10];');
      SL.Add('');

      for I := 0 to FChain.Count - 1 do
      begin
        B := TBlock(FChain[I]);

        SafeMsg := StringReplace(B.Mail.Mensaje, '"', '\"', [rfReplaceAll]);
        SafeMsg := StringReplace(SafeMsg, #13#10, ' ', [rfReplaceAll]);
        SafeMsg := StringReplace(SafeMsg, #10, ' ', [rfReplaceAll]);
        SafeMsg := StringReplace(SafeMsg, #13, ' ', [rfReplaceAll]);

        NodeName := Format('nodo%d', [I + 1]);

        NodeLabel := Format('%s [label="#%d\n%s -> %s\n%s\nPrevHash: %s\nHash: %s"];',
          [NodeName, B.Index, B.Mail.Emisor, B.Mail.Receptor, SafeMsg, B.PrevHash, B.Hash]);

        SL.Add('  ' + NodeLabel);
      end;

      SL.Add('');

      for I := 0 to FChain.Count - 2 do
        SL.Add(Format('  nodo%d -> nodo%d;', [I + 1, I + 2]));

      SL.Add('}');
      SL.SaveToFile(AFilename);
    finally
      SL.Free;
    end;
  end;


  // Principa
  var
    Chain: TBlockchain;
    M: TMail;
    DotFile: string;
  begin
    Chain := TBlockchain.Create;
    try
      M.Emisor := 'facundo@edd.com';
      M.Receptor := 'ana@edd.com';
      M.Mensaje := 'Hola Ana, como estas?';
      Chain.AddBlock(M);

      M.Emisor := 'ana@edd.com';
      M.Receptor := 'luis@edd.com';
      M.Mensaje := 'Luis, decile a facundo que me conteste';
      Chain.AddBlock(M);

      M.Emisor := 'luis@edd.com';
      M.Receptor := 'maria@edd.com';
      M.Mensaje := 'María, hiciste la tarea de edd?.';
      Chain.AddBlock(M);

      // Mostrar en consola
      Chain.PrintChain;

      // Validar cadena
      if Chain.IsValid then
        WriteLn('Blockchain válida.')
      else
        WriteLn('Blockchain inválida.');

      // Exportar a .dot para Graphviz
      DotFile := 'mail_blockchain.dot';
      Chain.ExportToDot(DotFile);
      WriteLn('Archivo .dot creado: ', DotFile);
      WriteLn('Puedes convertirlo a PNG con: dot -Tpng mail_blockchain.dot -o mail_blockchain.png');

    finally
      Chain.Free;
    end;

  end.
