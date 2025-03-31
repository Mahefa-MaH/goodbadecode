// Good Code:  Calculates the factorial of a non-negative integer using recursion.
operation GoodFactorial(n : Int) : Int {
    if n == 0 {
        return 1;
    }
    else {
        return n * GoodFactorial(n - 1);
    }
}

// Bad Code: Attempts to calculate the factorial but handles negative inputs poorly.
operation BadFactorial(n : Int) : Int {
    return n * BadFactorial(n - 1);
}
