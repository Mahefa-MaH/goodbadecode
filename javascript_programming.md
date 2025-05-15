**Title:** Efficient JavaScript Array Filtering:  Good vs. Bad Practices

**Summary:**  The key difference lies in leveraging native JavaScript array methods for concise and performant filtering versus using less efficient `for` loops and manual condition checks.  This impacts both code readability and execution speed.


**Good Code:**

```javascript
function filterEvenNumbers(numbers) {
  return numbers.filter(number => number % 2 === 0);
}

const numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
const evenNumbers = filterEvenNumbers(numbers);
console.log(evenNumbers); // Output: [2, 4, 6, 8, 10]
```


**Bad Code:**

```javascript
function filterEvenNumbersBad(numbers) {
  const evenNumbers = [];
  for (let i = 0; i < numbers.length; i++) {
    if (numbers[i] % 2 === 0) {
      evenNumbers.push(numbers[i]);
    }
  }
  return evenNumbers;
}

const numbersBad = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
const evenNumbersBad = filterEvenNumbersBad(numbersBad);
console.log(evenNumbersBad); // Output: [2, 4, 6, 8, 10]

```

**Key Takeaways:**

* **Readability:** The `filter()` method is significantly more concise and easier to understand, improving code maintainability.  The intent is immediately clear.
* **Efficiency:**  The `filter()` method is often optimized by JavaScript engines and generally performs faster than manual looping, especially for large arrays.
* **Maintainability:** The good code is shorter, easier to debug, and less prone to errors (e.g., off-by-one errors in loop indices).
* **Best Practices:** Using built-in array methods is considered a best practice in JavaScript, promoting cleaner and more idiomatic code.
* **Declarative vs. Imperative:** The good code uses a declarative approach (describing *what* to do), while the bad code uses an imperative approach (describing *how* to do it).  Declarative code is often preferred for its clarity and reduced complexity.

