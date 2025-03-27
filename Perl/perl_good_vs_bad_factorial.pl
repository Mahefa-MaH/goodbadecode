# Good Code Example: Calculating factorial
sub factorial {
  my $n = shift;
  return 1 if $n <= 1;
  return $n * factorial($n - 1);
}

print factorial(5); # Output: 120


# Bad Code Example: Calculating factorial (inefficient and unclear)
sub bad_factorial {
  my $n = shift;
  my $result = 1;
  while ($n > 1) {
    $result *= $n;
    $n--;
  }
  return $result;
}

print bad_factorial(5); # Output: 120

