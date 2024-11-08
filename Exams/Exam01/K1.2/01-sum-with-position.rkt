#lang racket

(define // quotient)
(define % modulo)

(define 1+ (lambda (x) (+ x 1)))
(define 1- (lambda (x) (- x 1)))

(define (count-digits n)
  (define (iter acc num)
    (if (< -10 num 10)
        acc
        (iter (+ 1 acc) (// num 10))
        )
    )
  (iter 1 n)
  )

(define (concat n m)
  (let ([count (count-digits m)])
    (+ (* n (expt 10 count)) m)
    )
  )

(define (rev n)
  (define (iter acc num)
    (if (zero? num)
        acc
        (let ([last-digit (% num 10)])
          (iter (concat acc last-digit) (// num 10))
          )
        )
    )
  (iter 0 n)
  )

(define (sum-with-position n)
  (define (iter i acc num)
    (if (zero? i)
        acc
        (let* ([last-digit (% num 10)]
               [digit (if (and (> i 0) (zero? last-digit)) 0 last-digit)]
               [to-concat (+ digit i)])
          (iter (1- i) (concat acc to-concat) (// num 10))
          )
        )
    )
  (iter (count-digits n) 0 (rev n))
  )

(sum-with-position 123)
(sum-with-position 507)
(sum-with-position 987)
(sum-with-position 1249)
(sum-with-position 9000)