# Good Code Example: Calculating the factorial of a number

def factorial(n)
  return 1 if n == 0
  (1..n).inject(:*)
end

puts factorial(5) # Output: 120


# Bad Code Example: Calculating the factorial of a number (inefficient and unclear)

def bad_factorial(n)
  if n == 0 then f = 1 else f = n * bad_factorial(n-1) end; f
end

puts bad_factorial(5) #Output: 120

