#lang racket

(define // quotient)
(define % modulo)

(define (count-divisors n)
  (define (iter cnt i)
    (if (= n i)
        cnt
        (iter (+ cnt (if (zero? (% n i)) 1 0)) (+ i 1)))
  )
  (iter 1 1)
)

(define (strip-first-digit m)
  (% m (expt 10 (- (count-digits m) 1)))
)

(define (ends-with? n k)
  (define (iter m)
    (cond [(and (= k 0) (not (= (% m 10) 0))) #f]
          [(< m k) #f]
          [(= m k) #t]
          [(> m k) (iter (strip-first-digit m))])
  )
  (iter n)
)