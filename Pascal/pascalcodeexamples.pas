program GoodCodeExample;
var
  i, sum: integer;
begin
  sum := 0;
  for i := 1 to 10 do
    sum := sum + i;
  writeln('Sum of numbers from 1 to 10: ', sum);
end.


program BadCodeExample;
var
  i,sum:integer;
begin
  sum:=0;
  for i:=1 to 10 do
  sum:=sum+i;
  writeln('Sum of numbers from 1 to 10: ',sum);
end.
