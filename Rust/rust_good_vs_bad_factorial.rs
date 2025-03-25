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


// Bad Code Example: Calculating the factorial of a number (inefficient and error-prone)
fn bad_factorial(n: u64) -> u64 {
    let mut result = 1;
    for i in 1..=n {
        result *= i;
        if result == 0 { //This is unnecessary and potentially hides issues.  A better approach would be to use Result<u64, Error> to handle potential overflow.
            break;
        }
    }
    result
}

fn main2() {
    println!("Bad Factorial of 5: {}", bad_factorial(5));
}
