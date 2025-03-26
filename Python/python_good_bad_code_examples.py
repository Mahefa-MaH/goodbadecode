# Good Code Example: Calculating the factorial of a number
def factorial(n):
    if n == 0:
        return 1
    else:
        result = 1
        for i in range(1, n + 1):
            result *= i
        return result

# Bad Code Example: Calculating the factorial of a number (less efficient and harder to read)
def factorial_bad(n):
  if n==0: return 1
  else:
    r=1
    for i in range(1,n+1):r*=i
    return r

#Good Code Example: Finding the largest number in a list.
def find_largest(numbers):
    if not numbers:
        return None  # Handle empty list case
    largest = numbers[0]
    for number in numbers:
        if number > largest:
            largest = number
    return largest

#Bad Code Example: Finding the largest number in a list (less readable)
def find_largest_bad(numbers):
  if len(numbers)==0:return None
  l=numbers[0]
  for n in numbers:
    if n>l:l=n
  return l

