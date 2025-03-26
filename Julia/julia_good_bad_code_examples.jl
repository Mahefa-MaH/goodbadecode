# Good Code Example: Calculating the factorial of a number iteratively.
function factorial_iterative(n)
    fact = 1
    for i in 1:n
        fact *= i
    end
    return fact
end

# Bad Code Example: Calculating the factorial of a number recursively without a base case.  This will cause a stack overflow for n > 0.
function factorial_recursive_bad(n)
    return n * factorial_recursive_bad(n-1)
end


# Good Code Example: Calculating the factorial of a number recursively with a base case.
function factorial_recursive_good(n)
    if n == 0
        return 1
    else
        return n * factorial_recursive_good(n-1)
    end
end

# Bad Code Example:  Unnecessarily complicated way to add two numbers.
function add_complicated(a,b)
    return sum([a,b])
end

# Good code example: Simple and efficient way to add two numbers.
function add_simple(a,b)
    return a+b
end
