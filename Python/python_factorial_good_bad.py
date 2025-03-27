# Good Code Example:  Calculates the factorial of a number using recursion.  Handles invalid input gracefully.

def factorial(n):
    if not isinstance(n, int) or n < 0:
        raise ValueError("Factorial is only defined for non-negative integers.")
    elif n == 0:
        return 1
    else:
        return n * factorial(n - 1)

#Bad Code Example: Calculates the factorial of a number using recursion but lacks error handling and is inefficient for large numbers.

def factorial_bad(n):
    if n==0:
        return 1
    else:
        return n*factorial_bad(n-1)

