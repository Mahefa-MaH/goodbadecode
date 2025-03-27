with Ada.Text_IO; use Ada.Text_IO;

procedure Good_Code is
begin
   Put_Line("Good code example");
   Put_Line("This is a simple, clear, and concise Ada program.");
end Good_Code;


procedure Bad_Code is
   X : Integer := 10;
   Y : Integer;
begin
   Y := X + 5; -- Missing declaration of Y
   Put_Line(Integer'Image(X + Y)); -- Potential overflow, no error handling
exception
   when others => Put_Line("Error occurred"); -- Too general exception handling
end Bad_Code;
