program UsoDePunteros;
var
  numero: Integer;
  ptr: ^Integer;
begin
  numero := 42;
  ptr := @numero; // ptr apunta a la dirección de 'numero'

  writeln('Valor: ', numero);
  writeln('Desde el puntero: ', ptr^); // accede al valor de 'numero'
end.
