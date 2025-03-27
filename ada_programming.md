**Title:** Ada: Robust vs. Risky String Handling

**Summary:**  Ada's strong typing prevents common string errors like buffer overflows, unlike less-strict languages.  The good example leverages Ada's built-in string manipulation for safety and efficiency; the bad example uses potentially unsafe C-style operations.


**Good Code:**

```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure String_Manipulation is
   My_String : Unbounded_String := To_Unbounded_String("Hello, Ada!");
   New_String : Unbounded_String;
begin
   Put_Line("Original String: " & To_String(My_String));

   -- Concatenation
   New_String := My_String & To_Unbounded_String(" How are you?");
   Put_Line("Concatenated String: " & To_String(New_String));

   -- Substring Extraction (slice)
   New_String := My_String(1..5);  -- Extract "Hello"
   Put_Line("Substring: " & To_String(New_String));

   -- Finding Substring
   if My_String.Find("Ada") /= 0 then
      Put_Line("Found 'Ada'!");
   end if;

   -- Exception Handling (demonstrates robustness)
   begin
      New_String := My_String(1..100); --Trying to access out of bounds index
   exception
      when Constraint_Error =>
         Put_Line("Index out of bounds!");
   end;
end String_Manipulation;
```


**Bad Code:**

```ada
with Ada.Text_IO; use Ada.Text_IO;

procedure String_Manipulation_Bad is
   My_String : String(1..20); -- Fixed size string, prone to buffer overflow
   New_String : String(1..20);
begin
   My_String := "Hello, Ada!"; 

   --Dangerous concatenation.  Buffer overflow possible!
   New_String := My_String & " How are you?"; --Potential buffer overflow if combined length exceeds 20

   Put_Line(New_String); --Might print garbage or crash

end String_Manipulation_Bad;

```

**Key Takeaways:**

* **Type Safety:** Ada's `Unbounded_String` prevents buffer overflows and other memory corruption issues common with fixed-size character arrays. The bad example uses a fixed-size string, introducing the risk of buffer overflow if the resulting string length exceeds its capacity.

* **Memory Management:** Ada automatically manages memory for `Unbounded_String`, reducing the risk of memory leaks and dangling pointers.

* **Built-in String Functions:** Ada provides safer and more efficient string manipulation functions (`&`, `.Find`, slicing) than manual character-by-character operations.

* **Exception Handling:**  The good example showcases Ada's exception handling mechanism, gracefully handling potential errors like index-out-of-bounds access.  The bad example offers no protection against such errors.

* **Readability and Maintainability:** The good example is more readable and easier to maintain due to its use of clear, high-level string operations.  The bad example necessitates manual memory management and error checking, increasing complexity and the chance of errors.
