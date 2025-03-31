; Good Code:  Calculating the factorial of a number using a named let

(define (factorial n)
  (let loop ((n n) (acc 1))
    (if (= n 0)
        acc
        (loop (- n 1) (* n acc)))))


; Bad Code: Calculating the factorial of a number using excessive recursion and no tail-call optimization.  This is inefficient and prone to stack overflow for larger inputs.

(define (bad-factorial n)
  (if (= n 0)
      1
      (* n (bad-factorial (- n 1)))))
