#lang racket

(define // quotient)
(define % modulo)

(define (increasing-digits? n)
  (define (increasing-last-2-digits m)
    (< (% (// m 10) 10) (% m 10))
  )

  (define (iter m)
    (if (< -10 m 10)
        #t
        (and (increasing-last-2-digits m) (increasing-last-2-digits (// m 10))))
  )
  (iter n)
)