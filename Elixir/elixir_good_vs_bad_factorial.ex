# Good Code
defmodule GoodCode do
  def factorial(0), do: 1
  def factorial(n) when n > 0, do: n * factorial(n - 1)
end

# Bad Code
defmodule BadCode do
  def factorial(n) do
    if n == 0 do
      1
    else
      n * factorial(n - 1)
    end
  end
end

