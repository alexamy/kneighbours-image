#lang racket

(require 2htdp/image)

(provide (all-defined-out))

;; color components
(define color-getters
  (list color-red color-green color-blue color-alpha))

;; color components as list of numbers
(define (color->list color)
  (for/list ([get color-getters])
    (get color)))

;; random color
(define (random/color)
  (color (random 256) (random 256) (random 256) 255))

;; average of colors
(define (avg . colors-start)
  (define (divide-length n) (quotient n (length colors-start)))
  (let loop ([result '(0 0 0 0)] [colors colors-start])
    (if (empty? colors)
        (apply color (map divide-length result))
        (loop
         (map + result (color->list (car colors)))
         (cdr colors)))))

;; distance between colors
(define (distance c1 c2)
  (sqrt
   (for/sum ([v1 (color->list c1)]
             [v2 (color->list c2)])
     (sqr (- v1 v2)))))