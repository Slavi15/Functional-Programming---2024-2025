#lang racket

(define (zip l1 l2)
  (if (or (null? l1) (null? l2))
      `()
      (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))
      )
  )

(zip '(1 2 3) '("a" "b" "c")) ;; => '((1 . "a") (2 . "b") (3 . "c"))
(zip '(1 2) '(3 4 5 6 7)) ;; => '((1 . 3) (2 . 4))