#lang racket

(define (transpose m)
  (if (null? (car m))
      `()
      (cons (map car m) (transpose (map cdr m)))
      )
  )

(define (transpose2 m) (apply map list m))

(define (multiply m1 m2)
  (let ([rows1 (length m1)]
        [cols1 (length (car m1))]
        [rows2 (length m2)]
        [cols2 (length (car m2))])
    (if (not (= rows1 cols2))
        `()
        (map (lambda (row)
               (map (lambda (col)
                      (apply + (map * row col)))
                    (transpose2 m2)))
             m1)
        )
    )
  )

(multiply `((1 2 3) (4 5 6) (7 8 9)) `((1 2 3) (4 5 6) (7 8 9)))
(multiply `((2 2 2) (2 2 2) (2 2 2)) `((2 2 2) (2 2 2) (2 2 2)))