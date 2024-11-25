#lang racket

(define // quotient)
(define % modulo)

(define (even? n) (zero? (% n 2)))
(define (odd? n) (not (even? n)))

(define (fast-power n k)
  (cond [(zero? k) 1]
        [(even? k) (fast-power (* n n) (// k 2))]
        [else (* n (fast-power n (- k 1)))])
)

(= (fast-power 2 5) 32)
(= (fast-power 15 3) 3375)

(define (pow n k)
  (define (iter product i)
    (if (= i 1)
        product
        (iter (* product n) (- i 1)))
  )
  (iter n k)
)