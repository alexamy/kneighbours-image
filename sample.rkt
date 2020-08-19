#lang racket

(require 2htdp/image)

(provide (all-defined-out))

(define *image* (bitmap "snapshot.png"))

(define *gradient*
  (apply beside
         (for/list ([step (range 1 16)])
           (rectangle 1 1 "solid" (color (quotient 255 step) 0 0 255)))))