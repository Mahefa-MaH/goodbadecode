// Good Code Example:  Calculates the factorial of a number iteratively.
function factorialGood(n) {
  if (n < 0) {
    throw new Error("Factorial is not defined for negative numbers.");
  }
  if (n === 0) {
    return 1;
  }
  let result = 1;
  for (let i = 2; i <= n; i++) {
    result *= i;
  }
  return result;
}


// Bad Code Example: Calculates the factorial of a number recursively without handling errors.
function factorialBad(n) {
  if (n === 0) {
    return 1;
  }
  return n * factorialBad(n - 1);
}

//Example of using good and bad code
console.log(factorialGood(5)); // Output: 120
//console.log(factorialBad(-1)); // Output: Stack overflow error.
console.log(factorialBad(5)); //Output: 120

