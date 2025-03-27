# Good Code Example: Calculating the factorial of a number

def factorial(n):
    if n == 0:
        return 1
    else:
        result = 1
        for i in range(1, n + 1):
            result *= i
        return result

number = 5
result = factorial(number)
print(f"The factorial of {number} is {result}")


# Bad Code Example: Calculating the factorial of a number (inefficient and less readable)

def bad_factorial(n):
  if n==0: return 1
  else:
    r=1;i=1
    while i<=n:r*=i;i+=1
    return r

num=5
res=bad_factorial(num)
print(f"The factorial of {num} is {res}")

