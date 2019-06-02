#lang racket
(require "vectors.rkt"
         "classes.rkt")
(define state 1)
(provide check-for-collision)
(define rect1 (new rectangle%
                   [pos (list 270 150 0)]
                   [u (list 100 -50 0)]
                   [vec1 (list 2 1 0)]))


(define rect2 (new rectangle%
                   [pos (list 400 160 0)]
                   [u (list -100 -50 0)]
                   [vec1 (list 3 4 0)]
                   ))


(define circle1 (new circle%
                     [pos (list 378 185 0)]))
(define rect5 (new rectangle%
                   [pos (list 160 100 0)]
                   [dim1 100]
                   [dim2 100]))

(define rect6 (new rectangle%
                   [pos (list 300 100 0)]
                   [dim1 100]
                   [dim2 100]))
                   

(define l1(list rect1 rect2))
(define l2 (list rect5 rect6))
;(define tick (/ 1 60))

(define (check-for-collision l)
  (cfc-helper0 l '()))

(define (cfc-helper0 l pass-pair);pass-pair is initially null; x= (list obj1 obj2 p n)
  (if (null? l)
      pass-pair
      (begin ;(displayln "1")

             (let ([cfc-helper-ans (begin
                                     ;(displayln (cfc-helper1 (car l) (cdr l) '()))
                                     (cfc-helper1 (car l) (cdr l) '()))])
               (if (null? cfc-helper-ans)
                   (cfc-helper0 (cdr l) pass-pair)
                   (cfc-helper0 (cdr l) (append cfc-helper-ans pass-pair))))

             )))

(define (cfc-helper1 obj l passed);passzed is initially null
  (if (null? l)
      passed
      (match (cfc-helper2 obj (car l));cfc-helper2 should return '() if not pass else (list obj1 obj2 p n)
        
        ['() (cfc-helper1  obj (cdr l) passed)]
        [(cons x y) (cfc-helper1 obj (cdr l) (cons (cons x y) passed))]

        [else (cfc-helper1 obj (cdr l) passed)])))

(define (cfc-helper2 obj1 obj2)
  (cond [(and (equal? (get-field name obj1) "circle") (equal? (get-field name obj2) "rectangle")) (circle-rectangle obj1 obj2)]
        [(and (equal? (get-field name obj1) "circle") (equal? (get-field name obj2) "ground")) (circle-rectangle obj1 obj2)]
        [(and (equal? (get-field name obj1) "ground") (equal? (get-field name obj2) "circle")) (circle-rectangle obj2 obj1)]
        [(and (equal? (get-field name obj1) "circle") (equal? (get-field name obj2) "circle")) (circle-circle obj1 obj2)]
        [(and (equal? (get-field name obj1) "rectangle") (equal? (get-field name obj2) "rectangle")) (rectangle-rectangle obj1 obj2)]
        [(and (equal? (get-field name obj1) "rectangle") (equal? (get-field name obj2) "ground")) (rectangle-rectangle obj1 obj2)]
        [(and (equal? (get-field name obj1) "ground") (equal? (get-field name obj2) "rectangle")) (rectangle-rectangle obj2 obj1)]
        [(and (equal? (get-field name obj1) "rectangle") (equal? (get-field name obj2) "circle")) (circle-rectangle obj2 obj1)]));(rectangle-circle obj1 obj2)]))

(define (circle-rectangle circle rectangle)
  (if (>= (+ (get-field radius circle) (/ (get-field diag rectangle) 2))
          (dist (get-field pos circle) (get-field pos rectangle)))
      (cr circle rectangle)
      '()))

(define (cr circle rectangle);can be made more efficient (probably)
  (let* ([radius (get-field radius circle)]
         [diag/2 (/ (get-field diag rectangle) 2)]
         [centre-circle (get-field pos circle)]
         [centre-rectangle (get-field pos rectangle)]
         [vec1 (get-field vec1 rectangle)]
         [vec2 (perp vec1)]
         [unit-vec1 (unitvector vec1)]
         [unit-vec2 (unitvector vec2)]
         [dim1 (get-field dim1 rectangle)]
         [dim2 (get-field dim2 rectangle)]
         [dim1/2 (/ dim1 2)]
         [dim2/2 (/ dim2 2)]
         [cv1 (vectorsum (vectorscale unit-vec1 dim1/2) (vectorscale unit-vec2 dim2/2))]
         [cv2 (vectorsum (vectorscale unit-vec1 dim1/2) (neg (vectorscale unit-vec2 dim2/2)))]
         [cv3 (vectorsum (neg (vectorscale unit-vec1 dim1/2)) (vectorscale unit-vec2 dim2/2))]
         [cv4 (vectorsum (neg (vectorscale unit-vec1 dim1/2)) (neg (vectorscale unit-vec2 dim2/2)))]
         [c1 (vectorsum centre-rectangle cv1)]
         [c2 (vectorsum centre-rectangle cv2)]
         [c3 (vectorsum centre-rectangle cv3)]
         [c4 (vectorsum centre-rectangle cv4)]
         [r1-vec (vectorscale unit-vec1 radius)]
         [r2-vec (vectorscale unit-vec2 radius)]
         [p1 (vectorsum centre-circle r1-vec)]
         [p2 (vectorsum centre-circle r2-vec)]
         [rect-p1 (vectordiff p1 centre-rectangle)]
         [rect-p2 (vectordiff p2 centre-rectangle)]
         [rp1proj1 (projection rect-p1 unit-vec1)]
         [rp1proj2 (projection rect-p1 unit-vec2)]
         [rp2proj1 (projection rect-p2 unit-vec1)]
         [rp2proj2 (projection rect-p2 unit-vec2)]
         [r3-vec (vectorscale (neg unit-vec1) radius)]
         [r4-vec (vectorscale (neg unit-vec2) radius)]
         [p3 (vectorsum centre-circle r3-vec)]
         [p4 (vectorsum centre-circle r4-vec)]
         [rect-p3 (vectordiff p3 centre-rectangle)]
         [rect-p4 (vectordiff p4 centre-rectangle)]
         [rp3proj1 (projection rect-p3 unit-vec1)]
         [rp3proj2 (projection rect-p3 unit-vec2)]
         [rp4proj1 (projection rect-p4 unit-vec1)]
         [rp4proj2 (projection rect-p4 unit-vec2)])
    ;         [v1 (vectorscale (/ (get-field rectangle dim1) 2) unit-vec1)]
    ;         [v2 (vectorscale (/ (get-field rectangle dim2) 2) unit-vec2)])
    (cond [(> (dist centre-circle centre-rectangle)
              (+ radius diag/2)) '()]
          [(>= radius (mod (vectordiff centre-circle c1))) (displayln 1)
;                                                           (displayln radius)
;                                                           (displayln (mod (vectordiff centre-circle c1)))
;                                                           (displayln c1)
;                                                           (displayln (mod cv1))
;;                                                           (displayln unit-vec1)
;;                                                           (displayln unit-vec2)
;                                                           (displayln centre-rectangle)
;                                                           (if (= state 4) (begin
;                                                                             (displayln (get-field pos rectangle))
;                                                                             (displayln (get-field vec1 rectangle))
;                                                                             (displayln (get-field pos circle))
;                                                                             (error "sdf")) (void))
;                                                           (set! state (+ state 1))
                                                           
                                                          ;(displayln centre-circle)
                                                          ;(displayln radius)
                                                          ;(displayln (mod cv2))
                                                          ;(displayln (vectordiff centre-circle c1))
                                                          ;(displayln (mod (vectordiff centre-circle c1)))
                                                          ;(> radius (mod (vectordiff centre-circle c1)))
                                                           (list circle rectangle c1 (unitvector (vectordiff c1 centre-circle)))]
          [(>= radius (mod (vectordiff centre-circle c2))) ;(displayln 2)
                                                          (list circle rectangle c2 (unitvector (vectordiff c2 centre-circle)))]
          [(>= radius (mod (vectordiff centre-circle c3)))  (list circle rectangle c3 (unitvector (vectordiff c3 centre-circle)))]
          [(>= radius (mod (vectordiff centre-circle c4)))  (list circle rectangle c4 (unitvector (vectordiff c4 centre-circle)))]
          [(and (<= rp1proj1 dim1/2) (<= rp1proj2 dim2/2))   (list circle rectangle p1 (unitvector r1-vec))]
          [(and (<= rp2proj1 dim1/2) (<= rp2proj2 dim2/2))   (list circle rectangle p2 (unitvector r2-vec))]
          [(and (<= rp3proj1 dim1/2) (<= rp3proj2 dim2/2))  
                                                            ;(displayln (vectordiff centre-circle c1))
                                                            ;(displayln (mod (vectordiff centre-circle c1)))
                                                            
                                                            (list circle rectangle p3 (unitvector r3-vec))]
          [(and (<= rp4proj1 dim1/2) (<= rp4proj2 dim2/2))  (displayln 8)  (list circle rectangle p4 (unitvector r4-vec))]
          [else '()])));(error "this case hasn't been handled")])))
          
(define (circle-circle circle1 circle2)
  (if (> (dist (get-field pos circle1) (get-field pos circle2 )) (+ (get-field  radius circle2 ) (get-field radius circle2 )))
      '()
      (let* ([centre-circle1 (get-field pos circle1)]
             [n (vectordiff (get-field pos circle2) centre-circle1)]
             [p (vectorsum centre-circle1 (vectorscale (unitvector n) (get-field radius circle1)))])
        (list circle1 circle2 p n))))

(define (rectangle-rectangle rectangle1 rectangle2)
  (if (>= (+ (/ (get-field diag rectangle1) 2) (/ (get-field diag rectangle2) 2))
          (dist (get-field pos rectangle1) (get-field pos rectangle2)))
      ;(begin ;(displayln "HI")
        (rr rectangle1 rectangle2)
      ;(begin ;(displayln "noway")
             ;(displayln (get-field diag rectangle1))
             ;(displayln (dist (get-field pos rectangle1)(get-field pos rectangle2)
                              '()))

(define (rr rectangle1 rectangle2)
  (define (proj-checker p proj1 proj2 dim1/2 dim2/2 vec1 vec2)
    (cond [(or (> proj1 dim1/2) (> proj2 dim2/2)) '()]
          [(> (- dim1/2 proj1) (- dim2/2 proj2)) (list p vec2)]
          [else  (list p vec1)]))

  (let* ([centre1 (get-field pos rectangle1)]
         [dim1r1 (get-field dim1 rectangle1)]; parallel to vec1r1
         [dim2r1 (get-field dim2 rectangle1)]
         [dim1r1/2 (/ dim1r1 2)]
         [dim2r1/2 (/ dim2r1 2)]
         [u1 (get-field u rectangle1)]
         [w1 (get-field w rectangle1)]
         [vec1r1 (unitvector (get-field vec1 rectangle1))]
         [vec2r1 (begin
;                   (display "in")
;                   (display (get-field vec1 rectangle1))
;                   (displayln "out")
                   (perp vec1r1))]
         [diag1 (get-field diag rectangle1)]
         [diag1/2 (/ diag1 2)]
         [corner-vec1r1 (vectorsum (vectorscale vec1r1 dim1r1/2) (vectorscale vec2r1 dim2r1/2))]
         [corner-vec2r1 (vectorsum (vectorscale vec1r1 dim1r1/2) (neg (vectorscale vec2r1 dim2r1/2)))]
         [corner-vec3r1 (neg corner-vec1r1)]
         [corner-vec4r1 (neg corner-vec2r1)]
         [c1r1 (vectorsum centre1 corner-vec1r1)]
         [c2r1 (vectorsum centre1 corner-vec2r1)]
         [c3r1 (vectorsum centre1 corner-vec3r1)]
         [c4r1 (vectorsum centre1 corner-vec4r1)]

         [centre2 (get-field pos rectangle2)]
         [dim1r2 (get-field dim1 rectangle2)]; parallel to vec1r2
         [dim2r2 (get-field dim2 rectangle2)]
         [dim1r2/2 (/ dim1r2 2)]
         [dim2r2/2 (/ dim2r2 2)]
         [u2 (get-field u rectangle2)]
         [w2 (get-field w rectangle2)]
         [vec1r2 (unitvector (get-field vec1 rectangle2))]
         [vec2r2 (begin
                   ;(displayln vec1r2)
                   (perp vec1r2))]
         [diag2 (get-field diag rectangle2)]
         [diag2/2 (/ diag2 2)]
         [corner-vec1r2 (vectorsum (vectorscale vec1r2 dim1r2/2) (vectorscale vec2r2 dim2r2/2))]
         [corner-vec2r2 (vectorsum (vectorscale vec1r2 dim1r2/2) (neg (vectorscale vec2r2 dim2r2/2)))]
         [corner-vec3r2 (neg corner-vec1r2)]
         [corner-vec4r2 (neg corner-vec2r2)]
         [c1r2 (vectorsum centre2 corner-vec1r2)]
         [c2r2 (vectorsum centre2 corner-vec2r2)]
         [c3r2 (vectorsum centre2 corner-vec3r2)]
         [c4r2 (vectorsum centre2 corner-vec4r2)]
         
         [p1r2 (vectordiff c1r1 centre2)]
         [p2r2 (vectordiff c2r1 centre2)]
         [p3r2 (vectordiff c3r1 centre2)]
         [p4r2 (vectordiff c4r1 centre2)]

         [p1r1 (vectordiff c1r2 centre1)]
         [p2r1 (vectordiff c2r2 centre1)]
         [p3r1 (vectordiff c3r2 centre1)]
         [p4r1 (vectordiff c4r2 centre1)]

         [proj1p1r2 (projection p1r2 vec1r2)]
         [proj2p1r2 (projection p1r2 vec2r2)]
         [proj1p2r2 (projection p2r2 vec1r2)]
         [proj2p2r2 (projection p2r2 vec2r2)]
         [proj1p3r2 (projection p3r2 vec1r2)]
         [proj2p3r2 (projection p3r2 vec2r2)]
         [proj1p4r2 (projection p4r2 vec1r2)]
         [proj2p4r2 (projection p4r2 vec2r2)]

         [proj1p1r1 (projection p1r1 vec1r1)]
         [proj2p1r1 (projection p1r1 vec2r1)]
         [proj1p2r1 (projection p2r1 vec1r1)]
         [proj2p2r1 (projection p2r1 vec2r1)]
         [proj1p3r1 (projection p3r1 vec1r1)]
         [proj2p3r1 (projection p3r1 vec2r1)]
         [proj1p4r1 (projection p4r1 vec1r1)]
         [proj2p4r1 (projection p4r1 vec2r1)]

         [check1 (append (proj-checker c1r1 proj1p1r2 proj2p1r2 dim1r2/2 dim2r2/2 vec1r2 vec2r2)
                         (proj-checker c2r1 proj1p2r2 proj2p2r2 dim1r2/2 dim2r2/2 vec1r2 vec2r2)
                         (proj-checker c3r1 proj1p3r2 proj2p3r2 dim1r2/2 dim2r2/2 vec1r2 vec2r2)
                         (proj-checker c4r1 proj1p4r2 proj2p4r2 dim1r2/2 dim2r2/2 vec1r2 vec2r2))]

         [check2 (append (proj-checker c1r2 proj1p1r1 proj2p1r1 dim1r1/2 dim2r1/2 vec1r1 vec2r1)
                         (proj-checker c2r2 proj1p2r1 proj2p2r1 dim1r1/2 dim2r1/2 vec1r1 vec2r1)
                         (proj-checker c3r2 proj1p3r1 proj2p3r1 dim1r1/2 dim2r1/2 vec1r1 vec2r1)
                         (proj-checker c4r2 proj1p4r1 proj2p4r1 dim1r1/2 dim2r1/2 vec1r1 vec2r1))])


    (cond [(null? check1)             (cond [(null? check2) '()]
                                            [(= 2 (length check2)) ;(displayln 1)
                                                                   (append (list rectangle1 rectangle2) check2)]
                                            [(= 4 (length check2)) ;(displayln 2)
                                                                   ;(displayln 

                                                                   (let ([p (vectorscale (vectorsum (car check2) (caddr check2)) (/ 1 2))]
                                                                         [n (unitvector (perp (vectordiff (car check2) (caddr check2))))])
                                                                     (list rectangle1 rectangle2 p n))]
                                            [else (error "1")])]
          [(= 2 (length check1))      (cond [(null? check2)
                                             ;(displayln 3) ;(displayln check1) ;(error "no-error")
                                             (append (list rectangle1 rectangle2) check1)]
                                            [(= 2 (length check2)) ;(displayln 4)
                                                                   (let* ([p1 (car check1)]
                                                                          [p2 (car check2)]
                                                                          [len1 ;(begin (displayln p1)
                                                                                       (vectordiff p1 centre1)]
                                                                          [len2 ;(begin (displayln p2)
                                                                                       (vectordiff p2 centre2)]
                                                                          [U1 (vectorsum u1 (crossprod w1 len1))]
                                                                          [U2 (vectorsum u2 (crossprod w2 len2))]
                                                                          [U1-U2 (vectordiff U1 U2)]
                                                                          [p1+p2/2 (vectorscale (vectorsum p1 p2) (/ 1 2))]
                                                                          [p1-p2 (vectordiff p1 p2)])
                                                                     (if (< (mod p1-p2) (* tick (mod U1-U2)))
                                                                         (list rectangle1
                                                                               rectangle2
                                                                               p1+p2/2
                                                                               (unitvector p1-p2))
                                                                         (begin; (displayln (unitvector p1-p2))
                                                                         (list rectangle1
                                                                               rectangle2
                                                                               p1+p2/2
                                                                               (perp (unitvector p1-p2))))))]

                                            [(= 4 (length check2)) ;(displayln 5)
                                                                   (let ([p (vectorscale (vectorsum (car check2) (caddr check2)) (/ 1 2))]
                                                                         [n (unitvector (vectordiff (car check2) (caddr check2)))])
                                                                     (list rectangle1 rectangle2 p n))]
                                            [else (error "2")])]
          [(= 4 (length check1))     ;(displayln 6) ;(error "myerror")
                                     (let ([p (vectorscale (vectorsum (car check1) (caddr check1)) (/ 1 2))]
                                           [n (unitvector (perp (vectordiff (car check1) (caddr check1))))])
                                       ;(displayln p)
                                       ;(displayln n)
                                       (list rectangle1 rectangle2 p n))]
          [else ;(displayln check1)
;                (displayln p1r2)
;                (displayln proj1p1r2)
;                (displayln proj2p1r2)
                (error "3")])))

         
         
