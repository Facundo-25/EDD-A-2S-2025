program PunteroDinamico;
var
  ptr: ^Integer;
begin
  New(ptr);          // Reserva memoria para un entero
  ptr^ := 99;        // Asigna valor
  writeln('Valor almacenado: ', ptr^);
  Dispose(ptr);      // Libera la memoria
end.
