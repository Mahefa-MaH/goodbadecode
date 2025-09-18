**Title:**  Smalltalk: Efficient vs. Inefficient Collection Iteration

**Summary:**  Efficient Smalltalk iteration leverages the collection's built-in `do:` method for direct element access, avoiding unnecessary intermediate objects. Inefficient methods often involve manual indexing or creation of temporary arrays.

**Good Code:**

```smalltalk
myCollection := #(1 2 3 4 5).  "Example collection"

myCollection do: [:each | 
  Transcript show: each; cr. "Process each element directly"
].
```

**Bad Code:**

```smalltalk
myCollection := #(1 2 3 4 5).

i := 1.
[ i <= myCollection size ] whileTrue: [
  Transcript show: (myCollection at: i); cr. "Inefficient indexing"
  i := i + 1.
].

"Or even worse, creating a temporary array"
tempArray := Array new: myCollection size.
myCollection withIndexDo: [:element :index |
    tempArray at: index put: element.
].
tempArray do: [:each | Transcript show: each; cr].

```


**Key Takeaways:**

* **Performance:** The `do:` method is highly optimized for collection traversal.  Manual indexing (`at:`) adds overhead.  Creating a temporary array incurs significant memory allocation and processing costs.
* **Readability:** `do:` provides a clear and concise way to express iteration. Manual indexing is less readable and prone to off-by-one errors.
* **Memory Management:**  The good code avoids unnecessary object creation, leading to reduced memory consumption and garbage collection overhead.
* **Correctness:** Manual indexing is more error-prone (e.g., off-by-one errors, handling empty collections).  The `do:` method handles these edge cases implicitly.
* **Smalltalk Idiom:** Utilizing `do:` is idiomatic Smalltalk, aligning with the language's design principles for efficient and elegant code.

