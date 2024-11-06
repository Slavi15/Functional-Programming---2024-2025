#lang racket

(define // quotient)
(define % modulo)

(define (sq x) (* x x))

(define (count-digits n)
  (define (iter sum num)
    (if (zero? num)
        sum
        (iter (+ 1 sum) (// num 10))
        )
    )
  (iter 0 n)
  )

(define (get-last-digits n k)
  (% n (expt 10 k))
  )

(define (automorphic? n)
  (let ([sq-num (sq n)]
        [k (count-digits n)])
    (= n (get-last-digits sq-num k))
    )
  )

(automorphic? 5)
(automorphic? 25)
(automorphic? 7)
