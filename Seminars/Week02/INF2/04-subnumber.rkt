#lang racket

(define // quotient)
(define % modulo)

(define (last-digits n digits)
  (% n (expt 10 digits))
)

(define (count-digits n)
  (define (iter acc i)
    (if (< -10 i 10)
        acc
        (iter (+ acc 1) (// i 10)))
  )
  (iter 1 n)
)

(define (subnumber? n k)
  (let ([len (count-digits n)])
    (define (iter k)
      (cond [(> n k) #f]
            [(= n (last-digits k len)) #t]
            [else (iter (// k 10))])
      )
    (iter k)
    )
)

(equal? (subnumber? 123 5123783) #t)
(equal? (subnumber? 0 0) #t)
(equal? (subnumber? 10 101) #t)
(equal? (subnumber? 101 101) #t)
(equal? (subnumber? 10 0) #f)
(equal? (subnumber? 1253 5123783) #f)
(equal? (subnumber? 12 0) #f)