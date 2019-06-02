#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)
(require "vectors.rkt")

(define e 1)
(define g (list 0 0 0))
(define tick (/ 1 150))

(provide circle%
         rectangle%
         e
         g
         tick)

;(define e 1)
;(define tick (/ 1 60))

(define circle%
  (class object%
    (init-field [pos (list 200 500 0)])
    (init-field [u (list 0 0 0)])
    (init-field [mass 1000]);kilo
    (init-field [radius 100]);metres
    (init-field [color "red"])
    (super-new)

    (field [image (circle radius "solid" color)])
    (field [name "circle"])
    (field [MI (* .5 mass (expt radius 2))])))

(define rectangle%
  (class object%
    (init-field [pos (list 0 0 0)])
    (init-field [dim1 100]);parallel to vec1
    (init-field [dim2 100]);parallel to vec2
    (init-field [u (list 0 0 0)]);m/s                                            ;modifiable
    (init-field [w (list 0 0 0)]);+ is anti-clockwise ;coming out of the plane   ;modifiable
    (init-field [vec1 (list 1 0 0)])                                             ;modifiable
    (init-field [mass 1000])
    (init-field [name "rectangle"])

    (super-new)

    (field [image (rectangle dim1 dim2 "solid" "brown")])
    
    (field [MI (* (/ 1 12) mass (+ (expt dim1 2) (expt dim2 2)))])
    (field [diag (sqrt (+ (expt dim1 2) (expt dim2 2)))])))



;    (field [corner-vec1 (vectorsum (scale unit-vec1 dim1/2) (scale unit-vec1 dim2/2))])
;    (field [corner-vec2 (vectorsum (scale unit-vec1 dim1/2) (neg (scale unit-vec1 dim2/2)))])
;    (field [corner-vec3 (vectorsum (neg (scale unit-vec1 dim1/2)) (scale unit-vec1 dim2/2))])
;    (field [corner-vec4 (vectorsum (neg (scale unit-vec1 dim1/2)) (neg (scale unit-vec1 dim2/2)))])
;(field [diag/2 (mod corner-vec1)])
;    (field [vec2 (perp vec1)])
;    (field [unit-vec1 (unitvector vec1)])
;    (field [unit-vec2 (unitvector vec2)])
;    (field [dim1/2 (/ dim1 2)])
;    (field [dim2/2 (/ dim2 2)])
