**Title:** Efficient String Concatenation in Smalltalk

**Summary:**  Smalltalk's `String` concatenation using `+` can be inefficient for many concatenations.  Using `StringBuffer` (or its modern equivalent, `StringBuilder` in some dialects) offers significant performance improvements for building strings iteratively.

**Good Code (Pharo Smalltalk):**

```smalltalk
stringBuilder := WriteStream on: String new.
1 to: 10000 do: [:i | stringBuilder nextPutAll: i printString; nextPut: $.].
result := stringBuilder contents. 
"result now holds a string containing '1.2.3...10000.'"

```

**Bad Code (Pharo Smalltalk):**

```smalltalk
result := ''.
1 to: 10000 do: [:i | result := result , i printString , '.'].
```


**Key Takeaways:**

* **Efficiency:** The `WriteStream` approach in the good code appends to a single mutable buffer. The bad code creates numerous intermediate strings, requiring repeated copying and garbage collection – resulting in O(n^2) time complexity.
* **Memory Management:**  The good code reduces memory usage by avoiding the creation of many temporary string objects.  The bad code leads to excessive allocation and deallocation of string objects, impacting performance and memory consumption.
* **Readability:** While concise, the bad code's repeated concatenation obscures the intent. The good code uses a `WriteStream`, improving the clarity and making the purpose more explicit.
* **Scalability:** The good code's linear time complexity scales much better with large numbers of concatenations compared to the bad code's quadratic time complexity.



**Note:**  While the examples are in Pharo Smalltalk, the concepts translate to other Smalltalk dialects.  The specific class names for mutable string buffers might vary slightly (e.g., `StringBuffer`, `StringBuilder`).  The core idea of using a stream-like object for efficient string building remains consistent.
