#lang racket

(define (remove-first x l)
  (cond
    [(null? l) `()]
    [(eqv? x (car l)) (cdr l)]
    [else (cons (car l) (remove-first x (cdr l)))]
    )
  )

(define (remove-all x l) (foldr (lambda (y ys) (if (not (eqv? x y)) (cons y ys) ys)) `() l))

(remove-first 1 '(1 1 1 2)) ; => `(1 1 2)
(remove-first 1 '(2 5 6)) ; => `(2 5 6)
(remove-first 1 '(1)) ; => `()
(remove-first 1 '(2 1)) ; => `(2)
(remove-first "RNN" '("CNN" "RNN" "GAN" "RNN")) ; => `("CNN" "GAN" "RNN")

(remove-all 1 '(1 1 1 2)) ; => `(2)
(remove-all 1 '(2 5 6)) ; => `(2 5 6)
(remove-all 1 '(1)) ; => `()
(remove-all 1 '(1 2 1 1)) ; => `(2)
(remove-all "RNN" '("CNN" "RNN" "GAN" "RNN")) ; => `("CNN" "GAN")