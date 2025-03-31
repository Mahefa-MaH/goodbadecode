#lang racket

; Good Code:  Uses named `let` for clarity and avoids unnecessary nesting.
(define (good-example x y)
  (let ([a (+ x 1)]
        [b (* y 2)])
    (+ a b)))


; Bad Code: Uses nested `let` and less descriptive variable names.  Harder to read and maintain.
(define (bad-example x y)
  (let ([a x])
    (let ([b (+ a 1)]
          [c (* y 2)])
      (+ b c))))

;Example usage
(good-example 2 3)
(bad-example 2 3)

