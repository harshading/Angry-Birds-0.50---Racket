#lang racket
(require "classes.rkt")
(require "vectors.rkt")
(provide collision)

(define (collision l)
  (cond[(null? l) (void)]
       [#t (begin ;(displayln "hi")
             ;(displayln l)
             ;(displayln (car(cdr(car l))))
             ;(if (not (null? l)) (displayln l) (void))
             (collision2 (car (car l)) (car(cdr(car l))) (car (cdr(cdr(car l)))) (car (reverse (car l))))
             ;(displayln "bye")
             (collision (cdr l)))]))
;                  (displayln "bye"))]))

(define (collision2 obj1 obj2 p n)
  (let* ([name1 (get-field name obj1)]
         [name2 (get-field name obj2)])
    (cond[(and (or (equal? name1 "ground") (equal? name1 "rectangle")) (or (equal? name2 "ground") (equal? name2 "rectangle"))) (let* ([n1 (unitvector n)]) (rectangle-collision obj1 obj2 p n1))]
         [(and (equal? name1 "circle") (equal? name2 "circle")) (circle-collision obj1 obj2 p)]
         [(or (equal? name1 "ground") (equal? name1 "rectangle")) (rectangle-circle-collision obj1 obj2 p)]
         [#t (rectangle-circle-collision obj2 obj1 p)])))

(define (rectangle-collision rect1 rect2 p n)
  ;(displayln p)
  ;(displayln n)
  ;(error "no-error2")
  (let* ([ma (get-field mass rect1)]
         [mb (get-field mass rect2)]
         [Ia (get-field MI rect1)]
         [Ib (get-field MI rect2)]
         [rap (vectordiff p (get-field pos rect1))]
         [rbp (vectordiff p (get-field pos rect2))]
         [wa1 (get-field w rect1)]
         [wb1 (get-field w rect2)]
         [va1 (get-field u rect1)]
         [vb1 (get-field u rect2)]
         [vap1 (vectorsum va1 (crossprod wa1 rap))]
         [vbp1 (vectorsum vb1 (crossprod wb1 rbp))]
         [vab1 (vectordiff vap1 vbp1)]
         [g1 (crossprod rap n)]
         [g2 (crossprod rbp n)]
         [c1 (+ (/ 1 ma) (/ 1 mb) (/ (dotprod g1 g1) Ia) (/ (dotprod g2 g2) Ib))]
         [c2 (* -1 (+ 1 e) (dotprod vab1 n))]
         [j (/ c2 c1)]
         [va2 (vectorsum va1 (map (lambda (x) (* (/ j ma) x)) n))]
         [vb2 (vectordiff vb1 (map (lambda (x) (* (/ j mb) x)) n))])
    (set-field! u rect1 va2) ; (displayln va1) (displayln va2) (displayln vb1) (displayln vb2)
    (set-field! u rect2 vb2)
    (set-field! w rect1 (vectorsum wa1 (map (lambda (x) (* j (/ x Ia))) (crossprod rap n))))
    (set-field! w rect2 (vectordiff wb1 (map (lambda (x) (* j (/ x Ib))) (crossprod rbp n))))))
  


(define (circle-collision circle1 circle2 p)
  (let* ([ma (get-field mass circle1)]
         [mb (get-field mass circle2)]
         [n1 (unitvector (vectordiff p (get-field pos circle1)))]
         [u1 (get-field u circle1)]
         [u2 (get-field u circle2)]
         [ap1 (dotprod u1 n1)]
         [bp1 (dotprod u2 n1)]
         [a (vectordiff u1 (proj u1 n1))]
         [b (vectordiff u2 (proj u2 n1))]
         [J (/ (* (+ 1 e) (- ap1 bp1)) (+ (/ 1 ma) (/ 1 mb)))]
         [ap2 (- ap1 (/ J ma))]
         [bp2 (+ bp1 (/ J mb))]
         [Ap2 (map (lambda (x) (* x ap2)) n1)]
         [Bp2 (map (lambda (x) (* x bp2)) n1)]
         [v1 (vectorsum a Ap2)]
         [v2 (vectorsum b Bp2)])
    (set-field! u circle1 v1) (set-field! u circle2 v2) ;(displayln v1)
    ;(displayln v2) (displayln n1) (displayln p))
    ))

(define (rectangle-circle-collision rect1 circle1 p)
  ;(displayln p)
  (let* ([ma (get-field mass circle1)]
         [mb (get-field mass rect1)]
         [n1 (unitvector (vectordiff p (get-field pos circle1)))]
         [u1 (get-field u circle1)]
         [u2 (get-field u rect1)]
         [ap1 (dotprod u1 n1)]
         [bp1 (dotprod u2 n1)]
         [a (vectordiff u1 (proj u1 n1))]
         [b (vectordiff u2 (proj u2 n1))] 
         [I (get-field MI rect1)]
         [wb1 (get-field w rect1)]
         [rbp (vectordiff p (get-field pos rect1))]
         [vbp1 (dotprod (vectorsum u2 (crossprod wb1 rbp)) n1)]
         [g1 (crossprod rbp n1)]
         [g2 (dotprod g1 g1)]
         [c1 (+ (/ 1 ma) (/ 1 mb) (/ g2 I))]
         [c2 (- ap1 vbp1)] ;;Possible Error
         [J (+ 0 (/ (* (+ 1 e) c2) c1))]
         [ap2 (- ap1 (/ J ma))]
         [bp2 (+ bp1 (/ J mb))]
         [wb2 (vectorsum wb1 (crossprod rbp (map (lambda (x) (* (/ J I) x)) n1)))]
         [v1 (vectorsum a (map (lambda (x) (* ap2 x)) n1))]
         [v2 (vectorsum b (map (lambda (x) (* bp2 x)) n1))]) ;(displayln p) (displayln n1) (displayln (vectorsum u1 u2)) (displayln (vectorsum v1 v2))
    (set-field! u circle1 v1) ;(displayln c2) (displayln J) 
    (set-field! u rect1 v2) (set-field! w rect1 wb2)))
         
