; Good Code:  Calculating factorial iteratively.
(define (factorial n)
  (let loop ((i 1) (acc 1))
    (if (> i n)
        acc
        (loop (+ i 1) (* acc i)))))

; Bad Code: Calculating factorial recursively without base case handling.  Will cause stack overflow for larger n.
(define (bad-factorial n)
  (if (= n 0)
      1
      (* n (bad-factorial (- n 1)))))


;Good Code:  Defining a function to check if a list is empty
(define (is-empty? lst)
  (null? lst))

;Bad Code: Defining a function to check if a list is empty with unnecessary complexity.
(define (bad-is-empty? lst)
  (cond ((null? lst) #t)
        (else #f)))

