**Title:** Haskell: Pure vs. Impure Function Implementation

**Summary:**  The key difference lies in whether a function relies on external state (impure) or solely on its input arguments (pure). Pure functions are predictable, testable, and enable powerful optimization techniques, whereas impure functions can have side effects making them harder to reason about.


**Good Code (Pure):**

```haskell
import Data.List (sort)

-- Pure function: calculates the sorted lengths of a list of strings.
sortedStringLengths :: [String] -> [Int]
sortedStringLengths strs = sort $ map length strs

main :: IO ()
main = do
  let strings = ["hello", "world", "haskell"]
  let sortedLengths = sortedStringLengths strings
  print sortedLengths -- Output: [5,5,6]
```

**Bad Code (Impure):**

```haskell
import Data.List (sort)

-- Impure function: uses global variable, making it harder to test and reason about.
globalStrings :: [String]
globalStrings = ["hello", "world", "haskell"]

sortedStringLengthsImpure :: IO [Int]
sortedStringLengthsImpure = do
  let lengths = map length globalStrings
  return $ sort lengths

main :: IO ()
main = do
  result <- sortedStringLengthsImpure
  print result -- Output: [5,5,6]

```

**Key Takeaways:**

* **Predictability and Testability:** Pure functions are easier to test because their output depends only on their input.  No external factors influence the result. The `sortedStringLengths` function can be tested independently.
* **Referential Transparency:**  A pure function's result can be replaced with its value without affecting program behavior.  This is a cornerstone of functional programming and greatly simplifies reasoning about code.  The impure version lacks referential transparency because `globalStrings` could change.
* **Parallelization and Optimization:** Pure functions can be easily parallelized and optimized by the compiler, as there are no hidden dependencies or side effects to worry about.
* **Maintainability:** Pure functions lead to more modular and maintainable code because they are self-contained and less prone to unexpected interactions with other parts of the program. The impure version makes it harder to modify `globalStrings` without risking unexpected changes in `sortedStringLengthsImpure`.
* **Readability:** Pure functions improve code clarity by explicitly declaring all dependencies through their parameters.  The `sortedStringLengths` function makes it crystal clear what its inputs and outputs are.



