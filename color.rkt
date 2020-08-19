#lang racket

(require 2htdp/image)

(provide (all-defined-out))

;; color components
(define color-getters
  (list color-red color-green color-blue color-alpha))

;; color components as list of numbers
(define (rgba-list color)
  (for/list ([get color-getters])
    (get color)))

;; random color
(define (random/color)
  (color (random 256) (random 256) (random 256) 255))

;; average of colors
(define (avg . colors)
  (define sum-divide (curryr quotient (length colors)))
  (apply color
         (map sum-divide
              (for/fold ([result '(0 0 0 0)])
                        ([color-curr colors])
                (map + result (rgba-list color-curr))))))

;; distance between colors
(define (distance c1 c2)
  (sqrt
   (for/sum ([get color-getters])
     (sqr (- (get c1) (get c2))))))