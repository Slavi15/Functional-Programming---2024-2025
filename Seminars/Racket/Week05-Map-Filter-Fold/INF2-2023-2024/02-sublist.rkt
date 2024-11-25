#lang racket

(define 1+ (lambda (x) (+ 1 x)))

(define (get-sublist start-idx end-idx xs)
  (define (iter i a b ls lst)
    (cond
      [(> i b) (reverse ls)]
      [(<= a i b) (iter (1+ i) a b (cons (car lst) ls) (cdr lst))]
      [else (iter (1+ i) a b ls (cdr lst))]
      )
    )
  (iter 0 start-idx end-idx `() xs)
  )

(get-sublist 2 6 '(1 2 2 3 1 5 6 7 7))
(equal? (get-sublist 2 6 '(1 2 2 3 1 5 6 7 7)) '(2 3 1 5 6))