#lang racket

(define (count-occurences el l) (foldr (lambda (x acc) (if (= x el) (+ 1 acc) acc)) 0 l))

(define (dedup l)
  (cond
    [(null? l) `()]
    [(= (count-occurences (car l) l) 1) (cons (car l) (dedup (cdr l)))]
    [else (dedup (cdr l))])
  )

(define (union l s) (dedup (append l s)))

(define (intersection l s)
  (cond
    [(null? l) `()]
    [(member (car l) s) (cons (car l) (intersection (cdr l) s))]
    [else (intersection (cdr l) s)])
  )

(define (product l s)
  (foldr append `()
         (map
          (lambda (x)
            (map
             (lambda (y) (cons x y))
             s)
            )
          l)
         )
  )

(union '(1 2 3 4) '(1 3 5 7))         ;; => '(1 2 3 4 5 7)
(intersection '(1 2 3 4) '(1 3 5 7))  ;; => '(1 3)
(product '(1 2) '(3 5 1))             ;; => '((1 . 3) (1 . 5) (1 . 1) (2 . 3) (2 . 5) (2 . 1))