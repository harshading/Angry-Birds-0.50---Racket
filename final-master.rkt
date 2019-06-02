#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require "classes.rkt")
(require "updater.rkt")

(define WIDTH 1200)
(define HEIGHT 700)
(define K 10)

(define ext (cons 0 0))

(define catapult (scale 2 (rotate 180 (polygon (list (make-posn 0 0)
                                (make-posn 0 30)
                                (make-posn -20 60)
                                (make-posn -19 61)
                                (make-posn -18 60)
                                (make-posn 2.5 33)
                                (make-posn 22.5 60)
                                (make-posn 24.5 60)
                                (make-posn 5 30)
                                (make-posn 5 0))
                          "solid"
                          "chocolate"))))

(define (give-bird-vel w x y me)
  (cond [(and (mouse=? me "drag")
              (> (* 2 (get-field radius (car w))) (abs (- x (car (get-field pos (car w))))))
              (> (* 2 (get-field radius (car w))) (abs (- y (- 700 (cadr (get-field pos (car w))))))))
         (begin (let ([xi (car (get-field pos (car w)))]
                      [yi (- 700 (cadr (get-field pos (car w))))])
                  (set-field! pos (car w) (list x (- 700 y) 0))
                  (set! ext (cons (- xi1 (car (get-field pos (car w))))
                                  (- yi1 (cadr (get-field pos (car w))))))
                  w))]
        [(mouse=? me "button-down") w]
        [(mouse=? me "button-up") (begin 
                                         (set-field! u (car w) (list (* K (car ext))
                                                                     (* K (cdr ext))
                                                                     0))
                                         w)]
        [else w]))

(define bg-img (bitmap "/home/harshad/Downloads/home1.png"))

;;;;;;;;;;Game objects;;;;;;;;;;;;

;;;BIRD;;;
(define circle1 (new circle%
                   [pos (list 80 100 0)]
                   [u (list 0 0 0)]
                   [radius 20]
                   ))

(define xi1 80)
(define yi1 100)
;;;;;;;;;;

(define ground (new rectangle%
                   [pos (list 600 0 0)]
                   [name "ground"]
                   [u (list 0 0 0)]
                   [dim1 2400]
                   [dim2 40]
                   [mass 9999999999999999]
                   [vec1 (list 1 0 0)]))
(define ceiling (new rectangle%
                   [pos (list 600 700 0)]
                   [name "ground"]
                   [u (list 0 0 0)]
                   [dim1 2400]
                   [dim2 40]
                   [mass 9999999999999999]
                   [vec1 (list 1 0 0)]))
(define rightwall (new rectangle%
                   [pos (list 1200 350 0)]
                   [name "ground"]
                   [u (list 0 0 0)]
                   [dim1 40]
                   [dim2 2400]
                   [mass 9999999999999999]
                   [vec1 (list 1 0 0)]))
(define rect1 (new rectangle%
                   [pos (list (* 2 (/ WIDTH 3)) (/ HEIGHT 3) 0)]
                   [u (list 0 0 0)]
                   [dim1 5]
                   [dim2 150]
                   [vec1 (list 1 2 0)]))

(define rect2 (new rectangle%
                   [pos (list (/ WIDTH 3) (/ HEIGHT 3) 0)]
                   [u (list 0 0 0)]
                   [dim1 5]
                   [dim2 150]
                   [vec1 (list 3 2 0)]))
(define rect3 (new rectangle%
                   [pos (list 1000 300 0)]
                   [u (list 0 0 0)]
                   [dim1 20]
                   [dim2 150]
                   [vec1 (list 1 0 0)]))
(define rect4 (new rectangle%
                   [pos (list 1100 300 0)]
                   [u (list 0 0 0)]
                   [dim1 20]
                   [dim2 150]
                   [vec1 (list 1 0 0)]))
(define rect5 (new rectangle%
                   [pos (list 1050 390 0)]
                   [u (list 0 0 0)]
                   [dim1 20]
                   [dim2 150]
                   [vec1 (list 0 1 0)]))
(define rect6 (new rectangle%
                   [pos (list 1050 210 0)]
                   [u (list 0 0 0)]
                   [dim1 20]
                   [dim2 150]
                   [vec1 (list 0 1 0)]))

(define circle2 (new circle%
                   [pos (list 1050 300 0)]
                   [u (list 0 0 0)]
                   [radius 10]
                   [color "yellowgreen"]
                   ))

(define circle3 (new circle%
                   [pos (list 900 300 0)]
                   [u (list 0 0 0)]
                   [radius 20]
                   [color "brown"]
                   ))
(define circle4 (new circle%
                   [pos (list 900 350 0)]
                   [u (list 0 0 0)]
                   [radius 20]
                   [color "brown"]
                   ))
(define circle5 (new circle%
                   [pos (list 900 250 0)]
                   [u (list 0 0 0)]
                   [radius 20]
                   [color "brown"]
                   ))

(define all-game-objects (list circle1 ground ceiling rightwall circle3 circle4 circle5 rect3 rect4 rect5 rect6 circle2))


(define (start-game w x y me)
  (cond [(and (mouse=? me "button-down")
              (> 50 (abs (- x (/ WIDTH 2))))
              (> 50 (abs (- y (* HEIGHT 0.59)))))
         (big-bang all-game-objects
           (on-tick (lambda (game-objects) (begin ;r(set-field! pos ground (list 600 700 0))
                                                  (updater game-objects)
                                                  all-game-objects)) tick)
           (to-draw (lambda (game-objects)
             (foldl (lambda (object scene)
                      (cond 
                            [(equal? (get-field name object) "rectangle") (place-image
                                                        (scale 1 (rotate (if (> 0 (car (get-field vec1 object)))
                                                                             (* (/ 180 pi) (+ pi (if (= 0 (car (get-field vec1 object))) (/ pi 2)
                                                                                                     (atan (/ (cadr (get-field vec1 object))
                                                                                                              (car (get-field vec1 object))
                                                                                                              )))))
                                                                             (* (/ 180 pi) (if (= 0 (car (get-field vec1 object))) (/ pi 2)
                                                                                               (atan (/ (cadr (get-field vec1 object))
                                                                                                        (car (get-field vec1 object))
                                                                                                        )))))
                                                                         (get-field image object)))
                                                        (car (get-field pos object))
                                                        (- 700 (cadr (get-field pos object)))
                                                        scene)]
                            [else (place-image (get-field image object)
                                                                 (car (get-field pos object))
                                                                 (- 700 (cadr (get-field pos object)))
                                                                 scene)]))
                    (place-image catapult
                                 80
                                 640
                                 (rectangle WIDTH HEIGHT "solid" "skyblue"))
                    game-objects)))
           (on-mouse give-bird-vel))]
        [else w]))
           

(big-bang 0
  (on-tick (lambda (ww) ww))
  (to-draw (lambda (ww) (place-image (scale 0.85 bg-img)
                                     (/ WIDTH 2)
                                     (/ HEIGHT 2)
                                     (frame (rectangle WIDTH HEIGHT "solid" "White")))))
  (on-mouse start-game))
