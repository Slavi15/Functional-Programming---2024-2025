#lang racket

(define (ordered? xs)
  (define (iter lst condition result)
    (if (< (length lst) 2)
        result
        (iter (cdr lst) condition (and (condition (car lst) (cadr lst)) result))
        )
    )
  (lambda (pred?) (iter xs pred? #t))
  )

(equal? ((ordered? '(1 2 3 5)) (λ (x y) (< x y))) #t)
(equal? ((ordered? '(1 8 29 92)) (λ (x y) (= y (+ (* x 3) 5)))) #t)
(equal? ((ordered? '(1 8 3 14)) (λ (x y) (= y (+ (* x 3) 5)))) #f)