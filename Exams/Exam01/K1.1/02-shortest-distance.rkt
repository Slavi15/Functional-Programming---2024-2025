#lang racket

(define (sq x) (* x x))

(define (dist p1 p2)
  (sqrt (+
         (sq (-  (car p1) (car p2)))
         (sq (-  (cdr p1) (cdr p2)))
         ))
  )

(define (shortest-distance points)
  (define (iter min-dist curr pts)
    (if (null? pts)
        min-dist
        (iter
         (foldr (lambda (x y) (if (< x y) x y)) min-dist (map (lambda (z) (dist z curr)) pts))
         (car pts)
         (cdr pts)
         )
        )
    )
  (iter (dist (car points) (cadr points)) (car points) (cdr points))
  )

(shortest-distance `((1 . 1) (2 . 2) (4 . 4)))

;; ------------------------------------------------------------------------------

#|
(define (get-distances curr-point ps)
  (if (null? ps)
      `()
      (let ([p-list (dist curr-point (car ps))])
        (cons p-list (get-distances curr-point (cdr ps)))
        )
      )
  )

(define (min l) (foldr (lambda (x xs) (if (< x xs) x xs)) (car l) (cdr l)))

(define (shortest-distance points)
  (define (iter min-dist curr-point ps)
    (if (null? ps)
        min-dist
        (let ([new-dist (min (get-distances curr-point ps))])
          (if (< min-dist new-dist)
              (iter min-dist (car ps) (cdr ps))
              (iter new-dist (car ps) (cdr ps))
              )
          )
        )
    )
  (iter (dist (car points) (cadr points)) (car points) (cdr points))
  )
|#

;; ------------------------------------------------------------------------------
