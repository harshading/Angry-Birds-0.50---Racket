#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)
(require "classes.rkt")
(require "updater.rkt")
(require "vectors.rkt")


(define WIDTH 1200)
(define HEIGHT 600)
;(define rectq1 (new rectangle%
;                   [pos (list 1 300 0)]
;                   [mass +nan.0]
;                   [dim1 10000]
;                   [dim2 2]))
;(define rectq2 (new rectangle%
;                   [pos (list 1199 300 0)]
;                   [mass +nan.0]
;                   [dim1 60000]
;                   [dim2 2]))
;(define rectv3 (new rectangle%
;                   [pos (list 600 1 0)]
;                   [mass +nan.0]
;                   [dim1 2]
;                   [dim2 120000]))
;
;(define rectv4 (new rectangle%
;                   [pos (list 600 599 0)]
;                   [mass +nan.0]
;                   [dim1 2]
;                   [dim2 120000]))
;
(define circle1 (new circle%
                     [pos (list 100 300 0)]
                     [u (list 100 -50 0)]))

(define circle2 (new circle%
                     [pos (list 200 200 0)]
                     [radius 20]
                     [u (list 300 0 0)]))

(define rect1 (new rectangle%
                   [pos (list 250 400 0)]
                   ;[dim1 400]
                   [mass 2000]
                   [u (list 0 -100 0)]
                   [w (list 0 0 0)]
                   

                    ))
;
;
(define rect2 (new rectangle%
                   [pos (list 300 150 0)]
                   [u (list 100 -50 0)]
                   ;[dim2 400]
                   [vec1 (list 1 2 0)]
                   ))
;
(define rect3 (new rectangle%
                   [pos (list 250 200 0)]
                   [w (list 0 0 0)]
                   [dim1 500]
                   [u (list 0 0 0)]))
                   ;[vec1 (list 2 1 0)]))


;(define rect4 (new rectangle%
;                   [pos (list 400 400 0)]
;                   [u (list 0 -50 0)]
;                   ;[vec1 (list 3 4 0)]
;                   ))
;rect2 pos (400 130 0)
(define all-game-objects (list rect1 rect3 )) ;rectq1 rectq2 rectv3 rectv4))

(big-bang all-game-objects
  (on-tick (lambda (x) (begin (updater x) all-game-objects))
           (/ 1 60))
  (to-draw (lambda (game-objects)
             (foldl (lambda (object scene)
                      (cond [(equal? (get-field name object) "rectangle") (place-image
                                                                           (rotate (if (> 0 (car (get-field vec1 object)))
                                                                                       (+ 0 (* (/ 180 pi) (+ pi (if (= 0 (car (get-field vec1 object))) (/ pi 2)
                                                                                                               (atan (/ 
                                                                                                                    (+ 0 (cadr (get-field vec1 object)))
                                                                                                                    (car (get-field vec1 object))))))))
                                                                                       (+ 0 (* (/ 180 pi) (if (= 0 (car (get-field vec1 object))) (/ pi 2)
                                                                                                               (atan (/ 
                                                                                                                    (+ 0 (cadr (get-field vec1 object)))
                                                                                                                    (car (get-field vec1 object))))))))
                                                                                              (get-field image object))
                                                                          (car (get-field pos object))
                                                                          (- 600 (cadr (get-field pos object)))
                                                                          scene)]
                            [else (place-image (get-field image object)
                                               (car (get-field pos object))
                                               (- 600 (cadr (get-field pos object)))
                                                scene)]))
                    (rectangle WIDTH HEIGHT "solid" "blue")
                    game-objects))))
                                                                    