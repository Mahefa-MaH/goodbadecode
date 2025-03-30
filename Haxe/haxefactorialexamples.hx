// Good Code Example: Calculating factorial using recursion
function factorial(n:Int):Int {
  if (n == 0) return 1;
  return n * factorial(n - 1);
}

// Bad Code Example: Calculating factorial with potential stack overflow
function badFactorial(n:Int):Int {
  return n == 0 ? 1 : n * badFactorial(n - 1); //No explicit return type, potential stack overflow for large n
}


class Main {
    static function main() {
        trace(factorial(5)); // Output: 120
        trace(badFactorial(5)); //Output:120, but prone to stack overflow for large inputs.

    }
}
