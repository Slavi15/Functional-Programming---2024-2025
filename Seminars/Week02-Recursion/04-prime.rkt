#lang racket

(define // quotient)
(define % modulo)

(define (prime? n)
  (define (iter bound)
    (if (< n 2)
        #f
        (let loop ([i 2])
          (cond [(> i bound) #t]
                [(zero? (% n i)) #f]
                [else (loop (+ i 1))]))
        )
  )
  (iter (sqrt n))
)