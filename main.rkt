#lang racket

(require
  (only-in "image.rkt" simplify-image)
  (only-in "sample.rkt" *image*))

(define (start)
  (simplify-image *image* 8))

(start)