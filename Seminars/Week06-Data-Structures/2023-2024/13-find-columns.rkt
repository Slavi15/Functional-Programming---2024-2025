#lang racket

(define (find-columns m)
  (define (contains? num l) (member num l))

  (define (column m idx) (foldr (lambda (x xs) (cons (list-ref x idx) xs)) `() m))

  (define (all-in-row? row col)
    (foldr (lambda (x acc) (and (contains? x row) acc)) #t col)
    )

  (define (check-column col)
    (foldr (lambda (row acc) (or (all-in-row? row col) acc)) #f m)
    )

  (define (iter i count)
    (if (>= i (length (car m)))
        count
        (let ([col (column m i)])
          (if (check-column col)
              (iter (+ i 1) (+ count 1))
              (iter (+ i 1) count)
              )
          )
        )
    )

  (iter 0 0)
  )

(find-columns '((1 4 3) (4 5 6) (7 4 9))) ;; => 1