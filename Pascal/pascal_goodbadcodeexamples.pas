program GoodCodeExample;

type
  StringArray = array[1..100] of string;
  PersonRecord = record
    Name: string;
    Age: Integer;
    Address: StringArray;
  end;

procedure AddPerson(var People: array of PersonRecord; NewPerson: PersonRecord; var Count: Integer);
var
  i: Integer;
begin
  if Count >= High(People) then
    exit; //Handle array full condition
  Count := Count + 1;
  People[Count] := NewPerson;
end;


procedure PrintPeople(const People: array of PersonRecord; Count: Integer);
var
  i: Integer;
  j: Integer;
begin
  for i := 1 to Count do
  begin
    Writeln('Name: ', People[i].Name);
    Writeln('Age: ', People[i].Age);
    Writeln('Address:');
    for j := 1 to Length(People[i].Address[1]) do // Assuming addresses are same length for simplicity
      Writeln(People[i].Address[j]);
    Writeln;
  end;
end;

var
  People: array[1..10] of PersonRecord;
  PersonCount: Integer = 0;
  NewPerson: PersonRecord;

begin
  NewPerson.Name := 'John Doe';
  NewPerson.Age := 30;
  NewPerson.Address[1] := '123 Main St';
  AddPerson(People,NewPerson, PersonCount);

  NewPerson.Name := 'Jane Doe';
  NewPerson.Age := 25;
  NewPerson.Address[1] := '456 Oak Ave';
  AddPerson(People,NewPerson, PersonCount);

  PrintPeople(People, PersonCount);
  readln;
end.


program BadCodeExample;

var
  name: string;
  age:integer;
  address:string;
begin
  readln(name);
  readln(age);
  readln(address);
  writeln('Name: ',name);
  writeln('Age: ',age);
  writeln('Address: ',address);
  readln;
end.
