#lang racket

(define // quotient)
(define % modulo)

(define (count-digits n)
  (define (iter acc n)
    (if (zero? n)
        acc
        (iter (+ 1 acc) (// n 10)))
    )
  (iter 0 n)
  )

(define (concat n digit)
  (+ (* n 10) digit)
  )

(define (num-to-xs n)
  (if (< -10 n 10)
      (list n)
      (let* ([k (- (count-digits n) 1)]
            [exp (expt 10 k)])
        (cons (// n exp) (num-to-xs (% n exp)))
        )
      )
  )

(define (xs-to-num l) (foldl (lambda (x xs) (concat xs x)) 0 l))

(num-to-xs 123) ; => `(1 2 3)
(num-to-xs 123456789) ; => `(1 2 3 4 5 6 7 8 9)

(xs-to-num '(1 2 3)) ; => 123
(xs-to-num '(1 2 3 4 5 6 7 8 9)) ; => 123456789