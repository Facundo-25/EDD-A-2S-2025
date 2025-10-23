program lzw;

{$mode objfpc}{$H+}

uses
 classes, SysUtils, crt;


type

  // Arreglo dinámico que almacenará las cadenas del diccionario
  TDictionary = array of string;

  // Tipo para representar un código numérico (0..65535)
  TCode = Word;
  // Arreglo de códigos generados por la compresión
  TCodeArray = array of TCode;

  // Clase principal que implementa el algoritmo LZW
  TCompresorLZW = class
    private
      // (Por ahora no se usan atributos privados)
    public
      // Inicializa el diccionario con los caracteres ASCII básicos
      procedure Inicializar(var dict: TDictionary);
      // Busca una cadena dentro del diccionario y devuelve su índice
      function Buscar(const dict: TDictionary; const cadena: string): Integer;
      // Agrega una nueva cadena al diccionario
      procedure Agregar(var dict: TDictionary; const cadena: string);

      // Comprime un texto y devuelve un arreglo de códigos
      function comprimir(const texto: string): TCodeArray;
      // Descomprime un arreglo de códigos y devuelve el texto original
      function descomprimir(const codigos: TCodeArray): string;
end;


// Inicializa el diccionario con los 256 caracteres ASCII
procedure TCompresorLZW.Inicializar(var dict: TDictionary);
var
  i: integer; // Variable para recorrer el diccionario

begin
  // Crea un diccionario con espacio para 256 entradas
  SetLength(dict, 256);

  // Llena el diccionario con los caracteres ASCII (0 a 255)
  for i := 0 to 255 do
    dict[i] := Chr(i); // Convierte el número i en su carácter correspondiente
end;


// Busca una cadena dentro del diccionario y devuelve su posición
function TCompresorLZW.Buscar(const dict: TDictionary; const cadena: string): Integer;
var
  i: Integer;
begin
  // Recorre todo el diccionario
  for i := 0 to High(dict) do
  begin
    // Si encuentra coincidencia exacta, devuelve el índice
    if dict[i] = cadena then
    begin
      Result := i;
      Exit;
    end;
  end;

  // Si no se encontró, devuelve -1
  Result := -1;
end;


// Agrega una nueva cadena al diccionario
procedure TCompresorLZW.Agregar(var dict: TDictionary; const cadena: string);
begin
  // Aumenta el tamaño del diccionario en una posición
  SetLength(dict, Length(dict) + 1);

  // Guarda la nueva cadena en la última posición del diccionario
  dict[High(dict)] := cadena;
end;




// Comprime el texto original y devuelve un arreglo de códigos
function TCompresorLZW.comprimir(const texto: string): TCodeArray;
var
  // Diccionario que guarda las cadenas conocidas
  dict: TDictionary;
  // Cadena temporal que representa la secuencia actual
  entrada: string;
  // Caracter actual leído del texto
  caracter: Char;
  // Código numérico asociado a una cadena del diccionario
  codigo: Integer;
  // Siguiente posición libre en el diccionario (para nuevas cadenas)
  siguienteCodigo: Integer;
  // Índice para recorrer el texto
  i: Integer;
  // Arreglo que almacenará los códigos resultantes de la compresión
  resultado: array of TCode;


begin
  //1. Inicializar diccionario con caracteres ASCII
  Inicializar(dict);
  siguienteCodigo := 256;

  //Colocar size 0 a resulttado
  SetLength(resultado, 0);
  entrada := '';

  //Recorrer caracteres del texto
  for i := 1 to Length(texto) do
  begin
    caracter := texto[i];

    //Verificar si entrada + caracter esta en diccionario
    codigo := Buscar(dict, entrada + caracter);

    if codigo <> -1 then
    begin
      //Si existe, agregar otro caracter
      entrada := entrada + caracter;
    end
    else
    begin
      //Si existe, buscar el codigo para agregarlo a resultado
      codigo := Buscar(dict, entrada);
      if codigo <> -1 then
      begin
        //Agregar codigo al resultado
        SetLength(resultado, Length(resultado) + 1);
        resultado[High(resultado)] := codigo;

        // Agregar nueva entrada al diccionario
        if siguienteCodigo < 65536 then // Límite de 16 bits
        begin
          Agregar(dict, entrada + caracter);
          Inc(siguienteCodigo);
        end;


      end;

      // Reiniciar con el caracter actual
      entrada := caracter;

    end;
  end;

  // Agregar el último código
  if entrada <> '' then
  begin
    codigo := Buscar(dict, entrada);
    SetLength(resultado, Length(resultado) + 1);
    resultado[High(resultado)] := codigo;
  end;

  Result := resultado;

end;

// Descomprime un arreglo de códigos y devuelve el texto original
function TCompresorLZW.descomprimir(const codigos: TCodeArray): string;
var
  // Diccionario usado durante la descompresión
  dict: TDictionary;
  // Cadenas que guardan la palabra anterior y la actual
  anterior, actual: string;
  // Códigos numéricos correspondientes a esas cadenas
  codigoAnterior, codigoActual: TCode;
  // Índice para recorrer el arreglo de códigos
  i: Integer;
  // Próximo código libre del diccionario
  siguienteCodigo: Integer;
begin
  // Inicializa el diccionario con los 256 caracteres ASCII
  Inicializar(dict);
  // El siguiente código disponible empieza en 256
  siguienteCodigo := 256;

  // Si no hay códigos, termina la función
  if Length(codigos) = 0 then
    Exit;

  // Toma el primer código como punto de partida
  codigoAnterior := codigos[0];
  // Obtiene su cadena del diccionario (por ejemplo: 65 = 'A')
  anterior := dict[codigoAnterior];
  // Coloca esa cadena como el inicio del resultado
  Result := anterior;

  // Recorre todos los códigos del arreglo
  for i := 0 to High(codigos) do
  begin
    // Toma el código actual
    codigoActual := codigos[i];

    // Si el código ya está en el diccionario
    if codigoActual < Length(dict) then
      // Recupera su cadena
      actual := dict[codigoActual]
    // Si el código aún no existe, lo crea combinando el anterior
    else if codigoActual = siguienteCodigo then
      actual := anterior + anterior[1]
    // Si el código no es válido, lanza un error
    else
      raise Exception.Create('Error: Código inválido en compresión');

    // Agrega la cadena actual al texto descomprimido
    Result := Result + actual;

    // Si el diccionario no ha llegado a su límite (65536)
    if siguienteCodigo < 65536 then
    begin
      // Agrega una nueva entrada: anterior + primer carácter del actual
      Agregar(dict, anterior + actual[1]);
      // Avanza al siguiente código disponible
      Inc(siguienteCodigo);
    end;

    // Actualiza referencias para la siguiente iteración
    anterior := actual;
    codigoAnterior := codigoActual;
  end;
end;


// Convierte el arreglo de códigos en una cadena separada por comas
function CodigosAString(const codigos: TCodeArray): string;
var
  i: Integer; // Índice para recorrer el arreglo
begin
  // Inicializa el resultado como una cadena vacía
  Result := '';

  // Recorre todos los elementos del arreglo de códigos
  for i := 0 to High(codigos) do
  begin
    // Si no es el primer elemento, agrega una coma separadora
    if i > 0 then
      Result := Result + ',';
    // Convierte el número a texto y lo agrega al resultado
    Result := Result + IntToStr(codigos[i]);
  end;
end;



var
  compresor: TCompresorLZW;
  textoOriginal, textoDescomprimido: string;
  codigosComprimidos: TCodeArray;
  i: Integer;


begin

  // Ejemplo 1: Texto simple
  textoOriginal := 'ABCABCD';
  Writeln('=== EJEMPLO 1 ===');
  Writeln('Texto original: ', textoOriginal);

  codigosComprimidos := compresor.Comprimir(textoOriginal);

  Writeln('Códigos comprimidos: ', CodigosAString(codigosComprimidos));

  textoDescomprimido := compresor.Descomprimir(codigosComprimidos);

  Writeln('Texto descomprimido: ', textoDescomprimido);

  ReadLn;


  // Ejemplo 2: Texto con patrones repetitivos
  textoOriginal := 'TOBEORNOTTOBEORTOBEORNOT';
  Writeln('=== EJEMPLO 2 ===');
  Writeln('Texto original: ', textoOriginal);

  codigosComprimidos := compresor.Comprimir(textoOriginal);

  Writeln('Códigos comprimidos: ', CodigosAString(codigosComprimidos));

  textoDescomprimido := compresor.Descomprimir(codigosComprimidos);
  Writeln('Texto descomprimido: ', textoDescomprimido);

  ReadLn;


  // Ejemplo 3: Texto más largo
  textoOriginal := 'EL ALGORITMO LZW ES UN ALGORITMO DE COMPRESION MUY EFICIENTE';
  Writeln('=== EJEMPLO 3 ===');
  Writeln('Texto original: ', textoOriginal);

  codigosComprimidos := compresor.Comprimir(textoOriginal);

  Writeln('Primeros 10 códigos: ');
  for i := 0 to 30 do
    Write(codigosComprimidos[i], ' ');
  Writeln;

  textoDescomprimido := compresor.Descomprimir(codigosComprimidos);
  Writeln('Texto descomprimido: ', textoDescomprimido);

  ReadLn;

  // Ejemplo 4
  textoOriginal := 'UNUNUN';
  Writeln('=== EJEMPLO 4 ===');
  Writeln('Texto original: ', textoOriginal);

  codigosComprimidos := compresor.Comprimir(textoOriginal);
  Writeln('Códigos comprimidos: ', CodigosAString(codigosComprimidos));

  textoDescomprimido := compresor.Descomprimir(codigosComprimidos);
  Writeln('Texto descomprimido: ', textoDescomprimido);


end.

