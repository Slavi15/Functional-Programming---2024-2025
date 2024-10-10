#lang racket

(define // quotient)
(define % modulo)

(define (prime? n)
  (if (< n 2)
      #f
      (let loop ([i 2])
        (cond [(> i (sqrt n)) #t]
              [(zero? (% n i)) #f]
              [else (loop (+ i 1))])
      )
  )
)