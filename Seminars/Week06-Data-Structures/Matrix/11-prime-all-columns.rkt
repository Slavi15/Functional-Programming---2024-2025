#lang racket

(define // quotient)
(define % modulo)

(define (divides n k) (zero? (% n k)))

(define 1+ (lambda (x) (+ 1 x)))

(define (prime? n)
  (define (iter bound)
    (if (< n 2)
        #f
        (let loop ([i 2])
          (cond
            [(> i bound) #t]
            [(divides n i) #f]
            [else (loop (1+ i))])
          )
        )
    )
  (iter (sqrt n))
  )

(define (any? p? l) (foldr (lambda (x xs) (or (p? x) xs)) #f l))

(define (prime-in-each-column? m)
  (if (null? (car m))
      #t
      (and (any? prime? (map car m))
           (prime-in-each-column? (map cdr m)))
      )
  )

(prime-in-each-column? '((2 2 4) (4 5 6))) ;; => #f
(prime-in-each-column? '((17 2 16) (4 5 3))) ;; => #t