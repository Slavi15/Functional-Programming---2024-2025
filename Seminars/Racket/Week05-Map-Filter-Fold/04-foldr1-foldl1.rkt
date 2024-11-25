#lang racket

(define (foldr1 op l)
  (if (null? (cdr l))
      (car l)
      (op (car l) (foldr1 op (cdr l)))
      )
  )

(define (foldl1 op l)
  (if (null? (cdr l))
      (car l)
      (op (foldl1 op (cdr l)) (car l))
      )
  )

(foldr1 + '(1 2 3)) ;; => 6 (1 + (2 + 3))
(foldl1 + '(1 2 3)) ;; => 6 ((1 + 2) + 3)