**Title:** Efficient JavaScript Array Iteration: `forEach` vs. `for...of`

**Summary:** While both `forEach` and `for...of` iterate over arrays, `for...of` offers superior control flow (e.g., `break`, `continue`) and direct access to array values, making it more versatile and potentially more performant in certain scenarios.  `forEach` is simpler for basic iteration but lacks these capabilities.


**Good Code:**

```javascript
const numbers = [1, 2, 3, 4, 5, 6];
let sum = 0;

for (const number of numbers) {
  if (number % 2 === 0) { //Example of conditional logic and continue
    sum += number;
  } else {
    continue; //Skip odd numbers
  }
}

console.log("Sum of even numbers:", sum);


//Alternative with early exit
const numbers2 = [1,2,3,4,5,6];
let sum2 = 0;
for (const number of numbers2){
    sum2 += number;
    if (sum2 > 10){
        break; //Exit loop early if sum exceeds 10
    }
}
console.log("Sum (with early exit):", sum2);

```

**Bad Code:**

```javascript
const numbers = [1, 2, 3, 4, 5, 6];
let sum = 0;

numbers.forEach(function(number, index) { //unnecessary index and anonymous function
  if (number % 2 === 0) {
    sum += number;
  }
});

console.log("Sum of even numbers:", sum);

//Another bad example with potential error (mutating array during iteration)
const arr = [1,2,3,4,5];
arr.forEach((num, i) => {
    if (num % 2 === 0) {
        arr.splice(i,1); //Dangerous! Modifies the array while iterating.
    }
})
console.log(arr);
```


**Key Takeaways:**

* **Control Flow:** `for...of` allows the use of `break` and `continue` statements for more flexible iteration control, which is not possible with `forEach`.  This is crucial for scenarios requiring early loop termination or skipping elements based on conditions.
* **Readability and Maintainability:**  `for...of` often leads to cleaner and more concise code, particularly when dealing with complex iteration logic.  Avoid unnecessary anonymous functions.
* **Performance:** While the performance difference might be negligible in many cases, `for...of` can be slightly faster in some scenarios, especially when dealing with large arrays, because it avoids the overhead of calling a callback function for each element (as in `forEach`).
* **Direct Value Access:** `for...of` directly provides the value of each element, making the code simpler and easier to read compared to `forEach` which includes an unnecessary index.
* **Avoid Mutation:** Never modify an array while iterating over it using `forEach` or similar methods. This can lead to unpredictable and hard-to-debug behavior.  Use `for...of` and explicit index manipulation if array modification is required during iteration.


