#lang racket

(require
  2htdp/image
  (only-in "image.rkt" pretty-simplify))

#;(current-command-line-arguments #("snapshot.png"))

(define (start)
  (define file-name (vector-ref (current-command-line-arguments) 0))
  (define image (bitmap/file file-name))
  (define image-simplified (pretty-simplify image 8))
  (save-image image-simplified "image-simplified.png"))

(start)