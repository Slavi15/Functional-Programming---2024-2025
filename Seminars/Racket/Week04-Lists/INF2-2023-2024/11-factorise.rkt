#lang racket

(define // quotient)
(define % modulo)

(define (divides n k) (zero? (% n k)))

(define (prime? n)
  (define (iter bound)
    (if (< n 2)
        #f
        (let loop ([i 2])
          (cond [(> i bound) #t]
                [(divides n i) #f]
                [else (loop (+ 1 i))])
          )
        )
    )
  
  (iter (sqrt n))
  )

(define (factorise n)
  (define (iter i num lst)
    (cond
      [(< num 2) (reverse lst)]
      [(and (divides num i) (prime? i)) (iter i (// num i) (cons i lst))]
      [else (iter (+ i 1) num lst)])
    )
  (iter 2 n `())
  )

(factorise 2)
(equal? (factorise 2) '(2))

(factorise 6)
(equal? (factorise 6) '(2 3))

(factorise 13)
(equal? (factorise 13) '(13))

(factorise 123)
(equal? (factorise 123) '(3 41))

(factorise 152)
(equal? (factorise 152) '(2 2 2 19))

(factorise 12356498)
(equal? (factorise 12356498) '(2 7 11 19 41 103))