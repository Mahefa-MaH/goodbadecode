# Good Code
  def is_prime(n : Int64) : Bool
    return false if n <= 1
    (2..n.sqrt).each do |i|
      return false if n % i == 0
    end
    true
  end

# Bad Code
  def is_prime_bad(n : Int64) : Bool
    if n <=1 then return false
    else
      for i in 2..n
        if n % i == 0 then return false end
      end
      return true
    end
  end

