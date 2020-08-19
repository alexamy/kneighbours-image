#lang racket

(require
  2htdp/image
  (only-in "image.rkt" simplify-image))

(define file-name (vector-ref (current-command-line-arguments) 0))
(define image (bitmap/file file-name))

(define-values (image-colors image-simplified) (simplify-image image 8))
(save-image image-simplified "image-simplified.png")
