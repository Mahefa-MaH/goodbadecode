**Title:** Efficient Tail-Recursive vs. Inefficient Recursive Factorial in Scheme

**Summary:**  This example highlights the crucial difference between tail-recursive and non-tail-recursive functions in Scheme, demonstrating how tail recursion avoids stack overflow errors for large inputs.  A tail-recursive function allows the Scheme interpreter to optimize the function call, whereas a non-tail-recursive function will consume stack space with each recursive call.


**Good Code (Tail-Recursive):**

```scheme
(define (factorial-tail n acc)
  (if (= n 0)
      acc
      (factorial-tail (- n 1) (* n acc))))

(define (factorial n)
  (factorial-tail n 1))

(display (factorial 1000))  (newline) ; Works without stack overflow
```

**Bad Code (Non-Tail-Recursive):**

```scheme
(define (factorial-bad n)
  (if (= n 0)
      1
      (* n (factorial-bad (- n 1)))))

(display (factorial-bad 1000)) ; Likely stack overflow
```


**Key Takeaways:**

* **Tail Recursion Optimization:** Scheme interpreters (and many other functional language interpreters/compilers) can optimize tail-recursive functions.  They transform the recursive call into a loop, avoiding the need to push a new stack frame for each call. This prevents stack overflow errors, even with large inputs.

* **Stack Overflow Prevention:** The non-tail-recursive `factorial-bad` function creates a new stack frame for each recursive call.  For large `n`, this consumes excessive memory, leading to a stack overflow.

* **Efficiency:** While both functions compute the factorial, the tail-recursive version is significantly more efficient in terms of memory usage for large inputs.

* **Readability & Maintainability:** While the non-tail-recursive version might appear simpler at first glance, the tail-recursive approach using an accumulator (`acc`) promotes clearer understanding of the recursive process and better code structure, especially for more complex recursive functions.  This improves maintainability.

* **Functional Programming Paradigm:**  Tail recursion is a key concept in functional programming, showcasing how to write recursive functions that are both elegant and efficient.
