#!/bin/bash

# Good Code Example:  Calculates the sum of numbers from 1 to 10.
sum=0
for i in $(seq 1 10); do
  sum=$((sum + i))
done
echo "Sum: $sum"


# Bad Code Example:  Calculates the sum of numbers from 1 to 10, but uses inefficient and unclear methods.
sum=0
a=`seq 1 10`
for i in $a; do
  sum=`expr $sum + $i`
done
echo "Sum: $sum"

#Good Code Example 2: Checks if a file exists.
if [ -f /etc/passwd ]; then
  echo "File exists"
else
  echo "File does not exist"
fi

#Bad Code Example 2: Checks if a file exists with inefficient syntax.
if test -f /etc/passwd; then
  echo "File exists"
else
  echo "File does not exist"
fi

