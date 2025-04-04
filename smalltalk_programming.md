## Title: Efficient String Concatenation in Smalltalk

## Summary:

Smalltalk offers multiple ways to concatenate strings.  Direct concatenation using `,'` is concise but inefficient for many concatenations; using a `StringStream` offers significantly better performance for building strings iteratively.


## Good Code:

```smalltalk
stringStream := WriteStream on: String new.
1 to: 10000 do: [:i | stringStream nextPutAll: i printString; nextPut: $ ].
result := stringStream contents.  "Efficient concatenation"
```

## Bad Code:

```smalltalk
result := ''.
1 to: 10000 do: [:i | result := result , i printString]. "Inefficient concatenation"
```

## Key Takeaways:

* **Efficiency:**  The `WriteStream` approach avoids repeatedly creating new string objects with each concatenation.  The bad code creates 10000 intermediate strings, consuming significant memory and time.  `WriteStream` appends to a single, growing buffer.

* **Readability:** While the `WriteStream` method is slightly more verbose, it's more expressive of the intent – building a string iteratively. The bad code's repeated assignment obscures the overall purpose.

* **Scalability:** The `WriteStream` method scales much better to larger numbers of concatenations. The bad code's performance degrades quadratically with the number of concatenations.

* **Memory Management:** The bad code creates many short-lived strings that the garbage collector must reclaim, leading to increased garbage collection overhead.  The `WriteStream` minimizes this overhead.
