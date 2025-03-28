// Good Code Example: Calculating the factorial of a number.
function Factorial(N: Integer): Int64;
begin
  if N < 0 then
    raise Exception.Create('Factorial is not defined for negative numbers');
  if N = 0 then
    Result := 1
  else
    Result := N * Factorial(N - 1);
end;

// Bad Code Example: Calculating the factorial of a number with potential stack overflow.
function BadFactorial(N: Integer): Int64;
begin
  if N = 0 then
    Result := 1
  else
    Result := N * BadFactorial(N-1); //No check for negative numbers, potential stack overflow for large N.

end;

procedure TForm1.Button1Click(Sender: TObject);
var
  num,fact:integer;
begin
   num := StrToInt(Edit1.Text);
   try
     fact := Factorial(num);
     ShowMessage('Factorial is: ' + IntToStr(fact));

   except
     on E: Exception do
       ShowMessage('Error: ' + E.Message);
   end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  num,fact:integer;
begin
   num := StrToInt(Edit1.Text);
   try
     fact := BadFactorial(num);
     ShowMessage('Factorial is: ' + IntToStr(fact));
   except
     on E: Exception do
       ShowMessage('Error: ' + E.Message);
   end;
end;
