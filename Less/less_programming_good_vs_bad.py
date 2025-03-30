# Good Code:  Uses a function for better readability and reusability.

def is_within_range(num):
  return 10 <= num <= 20

# Bad Code:  Less readable and harder to maintain.

if 10 <= num and num <=20:
    print("Within range")


# Good Code: Uses list comprehension for conciseness

squares = [x**2 for x in range(1, 11)]

# Bad Code: More verbose and less efficient

squares = []
for x in range(1, 11):
  squares.append(x**2)

# Good Code: Uses built-in functions for efficiency.

sum_of_numbers = sum(range(1,101))

# Bad Code: less efficient and more lines of code.

sum_of_numbers = 0
for i in range(1,101):
    sum_of_numbers +=i
