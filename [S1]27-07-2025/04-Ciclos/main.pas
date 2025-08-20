program Ciclos;
var
  i: Integer;
begin
  writeln('Ciclo FOR:');
  for i := 1 to 5 do
    writeln('i = ', i);

  writeln('Ciclo WHILE:');
  i := 1;
  while i <= 5 do
  begin
    writeln('i = ', i);
    i := i + 1;
  end;
end.
