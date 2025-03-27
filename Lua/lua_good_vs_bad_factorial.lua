-- Good Code Example: Calculating the factorial of a number
function factorial(n)
  if n == 0 then
    return 1
  else
    return n * factorial(n - 1)
  end
end

print(factorial(5)) -- Output: 120


-- Bad Code Example: Calculating the factorial of a number with redundant checks and unclear variable names
function fact(x)
  local a = x
  if x == 0 then
    return 1
  elseif x > 0 then
    local i = 1
    while i < x do
      a = a * (i + 1)
      i = i + 1
    end
    return a
  else
    return nil --error handling but not very informative
  end
end

print(fact(5)) -- Output: 120

