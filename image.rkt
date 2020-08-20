#lang racket

(require
  2htdp/image
  (only-in "color.rkt" distance avg))

(provide simplify-image)

; algorithm 
; 1. get all pixels with empty category
; 2. make random categories
; 3. for each pixel find nearest category
; 4. make new categories as avg of pixels
; 5. stop if distance from old to new categories is less than precision

;; create image from pixels with same size as provided image
(define (image-new pixels source)
  (let ([width (image-width source)]
        [height (image-height source)])
  (color-list->bitmap pixels width height)))

;; assign new category for each pixel
(define (assign-categories pixels colors)
  (for/list ([pixel pixels])
    (let ([pixel-color (cdr pixel)])
      (cons
       (argmin (λ (color) (distance color pixel-color)) colors)
       pixel-color))))

;; assign new average colors from pixels
(define (get-colors pixels)
  (map
   (λ (lst) (apply avg (map cdr lst)))
   (group-by car pixels)))

;; main procedure
(define (simplify-image image color-count #:precision [precision 4])
  (define color-list (image->color-list image))
  (define colors-start (build-list color-count (λ (x) (list-ref color-list (random (length color-list))))))
  (define pixels-start (map (λ (color) (cons #f color)) color-list))
  
  (let loop ([pixels pixels-start] [colors colors-start])
    (define pixels-new (assign-categories pixels colors))
    (define colors-new (get-colors pixels-new))
    (define min-distance
      (apply min (map distance colors colors-new)))
    
    (cond
      [(> min-distance precision) (loop pixels-new colors-new)]
      [else (values colors (image-new (map car pixels) image))])))