**Title:** Efficient String Concatenation in Smalltalk

**Summary:**  Smalltalk's `Stream` provides superior performance for string concatenation compared to repeated `+` operations due to its optimized buffer management, avoiding repeated string object creation.  The `+` operator leads to quadratic time complexity in the worst case.


**Good Code (Using a Stream):**

```smalltalk
string := 'a'.
1 to: 10000 do: [:i | string := string , i printString]. "Inefficient"

string2 := WriteStream on: String new.
1 to: 10000 do: [:i | string2 nextPutAll: i printString].
string2 contents. "Efficient"


"Example with more complex string creation"
writeStringStream := WriteStream on: String new.
(1 to: 100) do: [:i | 
  writeStringStream nextPutAll: 'This is string number: ';
  writeStringStream nextPutAll: i printString;
  writeStringStream nextPutAll: ' - ';
  writeStringStream nextPutAll: (i*i) printString;
  writeStringStream cr
].

resultString := writeStringStream contents.
Transcript show: resultString.

```

**Bad Code (Using Repeated `+`):**

```smalltalk
string := ''.
1 to: 10000 do: [:i | string := string , i printString]. "Inefficient"
```

**Key Takeaways:**

* **Efficiency:** The `WriteStream` approach in the good code has linear time complexity (O(n)), while repeatedly using the `+` operator leads to quadratic time complexity (O(n^2)) because each concatenation creates a new string object.  For large numbers of concatenations, this difference is dramatic.
* **Memory Management:** Repeated use of `+` leads to significantly higher memory consumption due to the creation and garbage collection of many intermediate string objects. `WriteStream` buffers the output, minimizing memory allocations.
* **Readability:** While seemingly simpler at first glance, the `WriteStream` method is arguably more readable for larger concatenation tasks, clearly separating the process of string building from the final string retrieval.
* **Avoidance of String Copies:** Each `+` operation creates a new string, copying the contents of the existing string.  `WriteStream` appends to an internal buffer, reducing copying overhead substantially.


**Note:** The `printString` method is used here to convert the numbers to strings before concatenation.  In some Smalltalk dialects more concise methods might be available.  The fundamental principle remains the same:  avoid repeated string concatenation with the `+` operator for large numbers of strings.
