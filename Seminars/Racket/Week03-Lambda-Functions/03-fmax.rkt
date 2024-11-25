#lang racket

(define (fmax f g)
  (lambda (x)
    (let ([fx (f x)]
         [gx (g x)])
      (if (> fx gx) fx gx)
      )
    )
  )