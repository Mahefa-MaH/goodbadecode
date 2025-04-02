**Title:** F# Functional vs. Imperative: A Concise Comparison

**Summary:** This example showcases the core differences between functional and imperative programming styles in F#.  The functional approach prioritizes immutability and pure functions for better readability, maintainability, and concurrency, while the imperative style uses mutable state and side effects, which can be less predictable and harder to debug.


**Good Code (Functional):**

```fsharp
let rec factorial n =
    match n with
    | 0 -> 1
    | _ when n < 0 -> failwith "Factorial is not defined for negative numbers"
    | _ -> n * factorial (n - 1)

let result = factorial 5
printfn "Factorial of 5: %d" result
```

**Bad Code (Imperative):**

```fsharp
let mutable factorial = 1
let n = 5
for i in 1 .. n do
    factorial <- factorial * i

printfn "Factorial of 5: %d" factorial
```


**Key Takeaways:**

* **Immutability:** The good code uses recursion and pattern matching, avoiding mutable state entirely. This makes the code easier to reason about, understand, and test because there are no unexpected side effects from modifying variables.  The bad code uses a mutable `factorial` variable, making it susceptible to unintended changes and harder to debug.

* **Pure Functions:** The `factorial` function in the good code is a pure function – its output depends solely on its input, without any side effects.  This allows for easier testing, parallelization, and refactoring. The imperative version has a side effect (modifying `factorial`).

* **Readability & Maintainability:** The functional approach (recursion and pattern matching) often leads to more concise and readable code. The imperative loop can be harder to understand, especially for more complex computations.

* **Error Handling:** The good code explicitly handles the case of negative input using `failwith`, preventing unexpected behavior. The bad code silently accepts negative input, leading to incorrect results.

* **Concurrency Safety:** Functional code with immutable data is inherently thread-safe; multiple threads can access and use it without risk of data corruption.  The imperative approach, with mutable state, requires explicit synchronization mechanisms to ensure thread safety, adding complexity.
