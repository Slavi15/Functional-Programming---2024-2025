#lang racket

(define (take n l)
  (if (or (zero? n) (null? l))
      `()
      (cons (car l) (take (- n 1) (cdr l)))
      )
  )

(define (count-occurences l1 l2)
  (define (iter cnt lst)
    (cond
      [(null? lst) cnt]
      [(equal? l1 (take (length l1) lst)) (iter (+ 1 cnt) (cdr lst))]
      [else (iter cnt (cdr lst))])
    )
  (iter 0 l2)
  )

(= (count-occurences '(1 5) '(1 5 2 3 1 5 6 7 7 1 5)) 3)
(= (count-occurences '(5 5) '(5 5 5 3 1 5 6 7 5 5 5)) 4)
(= (count-occurences '(6 6) '(2 2)) 0)