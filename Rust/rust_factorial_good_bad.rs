// Good Code Example: Calculating the factorial of a number using iteration.

fn factorial_iterative(n: u64) -> u64 {
    if n == 0 {
        1
    } else {
        (1..=n).product()
    }
}

// Bad Code Example: Calculating the factorial of a number using recursion without a base case.  This will cause a stack overflow.

fn factorial_recursive_bad(n: u64) -> u64 {
    n * factorial_recursive_bad(n - 1)
}


fn main() {
    let good_result = factorial_iterative(5);
    println!("Good Code Result: {}", good_result);
    //let bad_result = factorial_recursive_bad(5); // Uncommenting this line will cause a stack overflow.
    //println!("Bad Code Result: {}", bad_result);

}
