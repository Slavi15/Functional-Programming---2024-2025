#lang racket

(define // quotient)
(define % modulo)

(define (take n l)
  (if (zero? n)
      `()
      (cons (car l) (take (- n 1) (cdr l)))
      )
  )

(define (drop n l)
  (if (zero? n)
      l
      (drop (- n 1) (cdr l))
      )
  )

(define (shuffle l)
  (let* ([mid (// (length l) 2)]
         [xs (take mid l)]
         [ys (drop mid l)])
    (define (iter xs ys)
      (if (null? xs)
          `()
          (cons (car xs) (cons (car ys) (iter (cdr xs) (cdr ys))))
          )
      )
    (iter xs ys)
    )
  )

(shuffle '(2 5 1 3 4 7))
(equal? (shuffle '(2 5 1 3 4 7)) '(2 3 5 4 1 7))

(shuffle '(1 2 3 4 4 3 2 1))
(equal? (shuffle '(1 2 3 4 4 3 2 1)) '(1 4 2 3 3 2 4 1))

(shuffle '(1 1 2 2))
(equal? (shuffle '(1 1 2 2)) '(1 2 1 2))