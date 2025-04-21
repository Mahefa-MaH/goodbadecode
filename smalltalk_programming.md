**Title:**  Smalltalk: Efficient vs. Inefficient Collection Iteration

**Summary:**  Efficient Smalltalk collection iteration leverages optimized methods like `do:` for direct access, avoiding unnecessary intermediate collections. Inefficient code often creates new collections during iteration, leading to increased memory consumption and slower execution.


**Good Code:**

```smalltalk
Transcript show: 'Efficient Iteration:'.

#(1 2 3 4 5) do: [:each | 
  Transcript show: each printString; cr.
].

Transcript show: 'Sum of efficient iteration: '; show: ((#(1 2 3 4 5) inject: 0 into: [:sum :each | sum + each]) printString); cr.


```

**Bad Code:**

```smalltalk
Transcript show: 'Inefficient Iteration:'.

evenNumbers := #(1 2 3 4 5) select: [:each | each even].  "Creates a new collection"
evenNumbers do: [:each | 
  Transcript show: each printString; cr.
].

Transcript show: 'Sum of inefficient iteration: '; show: ((#(1 2 3 4 5) select: [:each | each even]) inject: 0 into: [:sum :each | sum + each]) printString; cr.

```

**Key Takeaways:**

* **Memory Efficiency:** The good code directly iterates over the original collection without creating intermediate collections like `evenNumbers` in the bad code. This saves memory, especially with large collections.

* **Speed:**  Creating and traversing a new collection adds overhead. The `do:` method in the good code is highly optimized for in-place iteration, resulting in faster execution.

* **Readability:** The good code is more concise and easier to understand. The intent is clearer as the iteration happens directly on the original collection.  The bad code introduces an unnecessary intermediate variable (`evenNumbers`).

* **Avoidance of Unnecessary Object Creation:**  Smalltalk's garbage collection is efficient, but creating unnecessary objects still consumes resources and adds to the garbage collector's workload.  Minimizing object creation is always a best practice.

* **Functional Style:** While the bad example uses `select:` (a functional approach), it's less efficient in this specific case due to the creation of the new collection.  The good example demonstrates that a functional style doesn't automatically equate to efficient code. The choice of method should always align with performance requirements.
