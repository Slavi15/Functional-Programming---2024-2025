#lang racket

(define (transpose xss)
  (if (null? (car xss))
      `()
      (cons (map car xss) (transpose (map cdr xss)))
      )
  )

(define (zero-rows xss)
  (map
   (lambda (xs)
     (if (list? (member 0 xs))
         (map (lambda (x) 0) xs) xs))
   xss)
  )

(define (zero-cols xss)
  (transpose (zero-rows (transpose xss)))
  )

(zero-cols '((1 2 0)
                     (3 4 1)
                     (0 5 7)
                     (4 2 4)))

(equal? (zero-cols '((1 2 0)
                     (3 4 1)
                     (0 5 7)
                     (4 2 4))) '((0 2 0)
                                 (0 4 0)
                                 (0 5 0)
                                 (0 2 0)))