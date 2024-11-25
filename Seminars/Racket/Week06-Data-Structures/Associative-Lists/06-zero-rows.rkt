#lang racket

;; --------------------- Easy Solution ---------------------

(define (zero-rows xss)
  (map
   (lambda (xs) (if (list? (member 0 xs)) (map (lambda (x) 0) xs) xs))
   xss)
  )

;; --------------------- Easy Solution ---------------------

;; --------------------- Hard Solution ---------------------

#|
(define (any-zero? l) (foldr (lambda (x acc) (or (zero? x) acc)) #f l))

(define (zero-row i)
  (if (zero? i)
      `()
      (cons 0 (zero-row (- i 1)))
      )
  )

(define (zero-rows mtx)
  (define (iter lst matrix)
    (if (null? matrix)
        (reverse lst)
        (let* ([row (car matrix)]
              [col-length (length row)])
          (if (any-zero? row)
              (iter (cons (zero-row col-length) lst) (cdr matrix))
              (iter (cons row lst) (cdr matrix))
              )
          )
        )
    )
  (iter `() mtx)
  )
|#

;; --------------------- Hard Solution ---------------------

(zero-rows '((1 2 0) 
                     (3 4 1) 
                     (0 5 7) 
                     (4 2 4)))

(equal? (zero-rows '((1 2 0) 
                     (3 4 1) 
                     (0 5 7) 
                     (4 2 4))) '((0 0 0) 
                                 (3 4 1) 
                                 (0 0 0) 
                                 (4 2 4)))