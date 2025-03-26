// Good Code Example: Calculating the factorial of a number
fn factorial(n: u64) -> u64 {
    match n {
        0 => 1,
        _ => n * factorial(n - 1),
    }
}

fn main() {
    println!("Factorial of 5: {}", factorial(5));
}


// Bad Code Example: Calculating the factorial of a number with potential stack overflow
fn bad_factorial(n: u64) -> u64 {
    if n == 0 {
        1
    } else {
        n * bad_factorial(n - 1) // Potential stack overflow for large n
    }
}

fn main2() {
    println!("Factorial of 5: {}", bad_factorial(5));
}

