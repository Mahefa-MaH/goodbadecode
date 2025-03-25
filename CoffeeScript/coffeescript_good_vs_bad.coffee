# Good CoffeeScript: Concise, readable, and idiomatic.

square = (x) -> x * x

numbers = [1, 2, 3, 4, 5]

squaredNumbers = numbers.map square

console.log squaredNumbers


# Bad CoffeeScript: Unnecessary complexity, poor readability, and non-idiomatic.

numbers2 = [1,2,3,4,5]

squaredNumbers2 = []

i = 0

while i < numbers2.length
  squaredNumbers2.push numbers2[i] * numbers2[i]
  i++

console.log squaredNumbers2
