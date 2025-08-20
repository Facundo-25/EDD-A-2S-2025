program AdivinaElNumero;
uses crt;
var
  secreto, intento: Integer;
  intentosMax: Integer;
  i: Integer;
  pista: ^String;
begin
  Randomize; // Inicializa generador de números aleatorios
  secreto := Random(10) + 1; // Número entre 1 y 10
  intentosMax := 3;
  New(pista); // Reservamos memoria para una pista

  writeln('Juego: Adivina el número (1-10)');
  for i := 1 to intentosMax do
  begin
    write('Intento ', i, ': ');
    readln(intento);

    if intento = secreto then
    begin
      writeln('¡Correcto! Ganaste.');
      Dispose(pista); // Liberamos memoria
      Exit;
    end
    else if intento < secreto then
      pista^ := 'Demasiado bajo.'
    else
      pista^ := 'Demasiado alto.';

    writeln(pista^);
  end;

  writeln('¡Perdiste! El número era: ', secreto);
  Dispose(pista);
end.
