#lang racket

(require 2htdp/image)

(define image (bitmap "snapshot.png"))

; algorithm
; 1. get all pixels with empty category
; 2. make random categories
; 3. for each pixel find nearest category
; 4. make new categories as avg of pixels
; 5. stop if distance from old to new categories is less than precision

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

(define gradient
  (apply beside
         (for/list ([step (range 1 16)])
           (rectangle 1 1 "solid" (color (quotient 255 step) 0 0 255)))))

;; main procedure
(define (simplify-image2 image color-count #:precision [precision 4])
  (void))

(define (simplify-image image color-count #:precision [precision 4])
  (define color-list (image->color-list image))
  (define colors-start (build-list color-count (λ (x) (list-ref color-list (random (length color-list))))))
  (define pixels-start (map (λ (color) (cons #f color)) color-list))
  (let rec ([pixels pixels-start] [colors colors-start])
    (define pixels-new
      (for/list ([pixel pixels])
        (cons
         (argmin (λ (color) (distance color (cdr pixel))) colors)
         (cdr pixel))))
    (define colors-new
      (map (λ (lst) (apply avg (map cdr lst))) (group-by car pixels-new)))
    (define min-distance
      (apply min (map distance colors colors-new)))
    (cond
      [(<= min-distance precision)
       (values colors
               (color-list->bitmap (map car pixels) (image-width image) (image-height image)))]
      [else (rec pixels-new colors-new)])))
  