program SeleccionConCase;
var
  opcion: Integer;
begin
  writeln('Ingresa una opción (1-3):');
  readln(opcion);

  case opcion of
    1: writeln('Elegiste la opción 1.');
    2: writeln('Elegiste la opción 2.');
    3: writeln('Elegiste la opción 3.');
  else
    writeln('Opción inválida.');
  end;
end.
