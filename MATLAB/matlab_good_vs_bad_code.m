% Good Code Example: Calculating the factorial of a number using a for loop.
function factorial = goodFactorial(n)
  if n < 0
    error('Factorial is not defined for negative numbers.');
  elseif n == 0
    factorial = 1;
  else
    factorial = 1;
    for i = 1:n
      factorial = factorial * i;
    end
  end
end

% Bad Code Example: Calculating the factorial of a number using recursion without a base case.  Will cause a stack overflow.
function factorial = badFactorial(n)
  factorial = n * badFactorial(n-1);
end

% Good Code Example:  Finding the roots of a quadratic equation using the quadratic formula. Includes error handling.
function roots = goodQuadraticRoots(a, b, c)
  if a == 0
    error('Not a quadratic equation.');
  end
  discriminant = b^2 - 4*a*c;
  if discriminant < 0
      roots = complex((-b + sqrt(discriminant))/(2*a), (-b - sqrt(discriminant))/(2*a));
  else
      roots = [(-b + sqrt(discriminant))/(2*a), (-b - sqrt(discriminant))/(2*a)];
  end
end

% Bad Code Example: Finding the roots of a quadratic equation with no error handling. May produce errors.
function roots = badQuadraticRoots(a, b, c)
  discriminant = b^2 - 4*a*c;
  roots = [(-b + sqrt(discriminant))/(2*a), (-b - sqrt(discriminant))/(2*a)];
end

% Example usage
goodFactorial(5)
%badFactorial(5) % uncomment to see the error
goodQuadraticRoots(1, -3, 2)
%badQuadraticRoots(0,1,1) % uncomment to see the error

