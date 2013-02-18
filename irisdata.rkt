(module irisdata (lib "plt-pretty-big-text.ss" "lang")
  
  
  (provide make-iris iris-sepal-len iris-sepal-wid iris-petal-len iris-petal-wid iris-dist iris-avg iris-data iris-classes iris-k)
  
;; an iris is a (list Num Num Num Num)
;; describing the properties of an iris flower. 
(define-struct iris (sepal-len sepal-wid petal-len petal-wid))

;; distance between two iris flowers (Euclidean)
;; iris-dist: Iris Iris -> Num
(define (iris-dist iris1 iris2)
  (sqrt (+ (sqr (- (iris-sepal-len iris1) (iris-sepal-len iris2))) 
           (sqr (- (iris-sepal-wid iris1) (iris-sepal-wid iris2)))
           (sqr (- (iris-petal-len iris1) (iris-petal-len iris2)))
           (sqr (- (iris-petal-wid iris1) (iris-petal-wid iris2)))
           )))

;; avererage of a list of iris flowers
;; iris-avg: (listof Iris) -> Iris
(define (iris-avg loi)
  (cond [(empty? loi) (make-iris 0 0 0 0)]
        [else (div-iris (foldr add-iris (make-iris 0 0 0 0) loi) (length loi))]))
;; divide an iris flower by a constant
;; div-iris: Iris Num -> Iris
(define (div-iris i c)
  (make-iris (/ (iris-sepal-len i) c) 
             (/ (iris-sepal-wid i) c) 
             (/ (iris-petal-len i) c) 
             (/ (iris-petal-wid i) c)))
;; add two iris flowers
;; add-iris Iris Iris -> Iris
(define (add-iris i1 i2)
  (make-iris (+ (iris-sepal-len i1) (iris-sepal-len i2))
             (+ (iris-sepal-wid i1) (iris-sepal-wid i2))
             (+ (iris-petal-len i1) (iris-petal-len i2))
             (+ (iris-petal-wid i1) (iris-petal-wid i2))))

;; the number of types of iris flowers
(define iris-k 3)

;; the data
(define iris-data (list
(make-iris 5.1 3.5 1.4 0.2)
(make-iris 7.0 3.2 4.7 1.4)
(make-iris 6.5 3.0 5.2 2.0)
(make-iris 4.9 3.0 1.4 0.2)
(make-iris 4.7 3.2 1.3 0.2)
(make-iris 4.6 3.1 1.5 0.2)
(make-iris 5.0 3.6 1.4 0.2)
(make-iris 5.4 3.9 1.7 0.4)
(make-iris 4.6 3.4 1.4 0.3)
(make-iris 5.0 3.4 1.5 0.2)
(make-iris 4.4 2.9 1.4 0.2)
(make-iris 4.9 3.1 1.5 0.1)
(make-iris 5.4 3.7 1.5 0.2)
(make-iris 4.8 3.4 1.6 0.2)
(make-iris 4.8 3.0 1.4 0.1)
(make-iris 4.3 3.0 1.1 0.1)
(make-iris 5.8 4.0 1.2 0.2)
(make-iris 5.7 4.4 1.5 0.4)
(make-iris 5.4 3.9 1.3 0.4)
(make-iris 5.1 3.5 1.4 0.3)
(make-iris 5.7 3.8 1.7 0.3)
(make-iris 5.1 3.8 1.5 0.3)
(make-iris 5.4 3.4 1.7 0.2)
(make-iris 5.1 3.7 1.5 0.4)
(make-iris 4.6 3.6 1.0 0.2)
(make-iris 5.1 3.3 1.7 0.5)
(make-iris 4.8 3.4 1.9 0.2)
(make-iris 5.0 3.0 1.6 0.2)
(make-iris 5.0 3.4 1.6 0.4)
(make-iris 5.2 3.5 1.5 0.2)
(make-iris 5.2 3.4 1.4 0.2)
(make-iris 4.7 3.2 1.6 0.2)
(make-iris 4.8 3.1 1.6 0.2)
(make-iris 5.4 3.4 1.5 0.4)
(make-iris 5.2 4.1 1.5 0.1)
(make-iris 5.5 4.2 1.4 0.2)
(make-iris 4.9 3.1 1.5 0.1)
(make-iris 5.0 3.2 1.2 0.2)
(make-iris 5.5 3.5 1.3 0.2)
(make-iris 4.9 3.1 1.5 0.1)
(make-iris 4.4 3.0 1.3 0.2)
(make-iris 5.1 3.4 1.5 0.2)
(make-iris 5.0 3.5 1.3 0.3)
(make-iris 4.5 2.3 1.3 0.3)
(make-iris 4.4 3.2 1.3 0.2)
(make-iris 5.0 3.5 1.6 0.6)
(make-iris 5.1 3.8 1.9 0.4)
(make-iris 4.8 3.0 1.4 0.3)
(make-iris 5.1 3.8 1.6 0.2)
(make-iris 4.6 3.2 1.4 0.2)
(make-iris 5.3 3.7 1.5 0.2)
(make-iris 5.0 3.3 1.4 0.2)
(make-iris 6.4 3.2 4.5 1.5)
(make-iris 6.9 3.1 4.9 1.5)
(make-iris 5.5 2.3 4.0 1.3)
(make-iris 6.5 2.8 4.6 1.5)
(make-iris 5.7 2.8 4.5 1.3)
(make-iris 6.3 3.3 4.7 1.6)
(make-iris 4.9 2.4 3.3 1.0)
(make-iris 6.6 2.9 4.6 1.3)
(make-iris 5.2 2.7 3.9 1.4)
(make-iris 5.0 2.0 3.5 1.0)
(make-iris 5.9 3.0 4.2 1.5)
(make-iris 6.0 2.2 4.0 1.0)
(make-iris 6.1 2.9 4.7 1.4)
(make-iris 5.6 2.9 3.6 1.3)
(make-iris 6.7 3.1 4.4 1.4)
(make-iris 5.6 3.0 4.5 1.5)
(make-iris 5.8 2.7 4.1 1.0)
(make-iris 6.2 2.2 4.5 1.5)
(make-iris 5.6 2.5 3.9 1.1)
(make-iris 5.9 3.2 4.8 1.8)
(make-iris 6.1 2.8 4.0 1.3)
(make-iris 6.3 2.5 4.9 1.5)
(make-iris 6.1 2.8 4.7 1.2)
(make-iris 6.4 2.9 4.3 1.3)
(make-iris 6.6 3.0 4.4 1.4)
(make-iris 6.8 2.8 4.8 1.4)
(make-iris 6.7 3.0 5.0 1.7)
(make-iris 6.0 2.9 4.5 1.5)
(make-iris 5.7 2.6 3.5 1.0)
(make-iris 5.5 2.4 3.8 1.1)
(make-iris 5.5 2.4 3.7 1.0)
(make-iris 5.8 2.7 3.9 1.2)
(make-iris 6.0 2.7 5.1 1.6)
(make-iris 5.4 3.0 4.5 1.5)
(make-iris 6.0 3.4 4.5 1.6)
(make-iris 6.7 3.1 4.7 1.5)
(make-iris 6.3 2.3 4.4 1.3)
(make-iris 5.6 3.0 4.1 1.3)
(make-iris 5.5 2.5 4.0 1.3)
(make-iris 5.5 2.6 4.4 1.2)
(make-iris 6.1 3.0 4.6 1.4)
(make-iris 5.8 2.6 4.0 1.2)
(make-iris 5.0 2.3 3.3 1.0)
(make-iris 5.6 2.7 4.2 1.3)
(make-iris 5.7 3.0 4.2 1.2)
(make-iris 5.7 2.9 4.2 1.3)
(make-iris 6.2 2.9 4.3 1.3)
(make-iris 5.1 2.5 3.0 1.1)
(make-iris 5.7 2.8 4.1 1.3)
(make-iris 6.3 3.3 6.0 2.5)
(make-iris 5.8 2.7 5.1 1.9)
(make-iris 7.1 3.0 5.9 2.1)
(make-iris 6.3 2.9 5.6 1.8)
(make-iris 6.5 3.0 5.8 2.2)
(make-iris 7.6 3.0 6.6 2.1)
(make-iris 4.9 2.5 4.5 1.7)
(make-iris 7.3 2.9 6.3 1.8)
(make-iris 6.7 2.5 5.8 1.8)
(make-iris 7.2 3.6 6.1 2.5)
(make-iris 6.5 3.2 5.1 2.0)
(make-iris 6.4 2.7 5.3 1.9)
(make-iris 6.8 3.0 5.5 2.1)
(make-iris 5.7 2.5 5.0 2.0)
(make-iris 5.8 2.8 5.1 2.4)
(make-iris 6.4 3.2 5.3 2.3)
(make-iris 6.5 3.0 5.5 1.8)
(make-iris 7.7 3.8 6.7 2.2)
(make-iris 7.7 2.6 6.9 2.3)
(make-iris 6.0 2.2 5.0 1.5)
(make-iris 6.9 3.2 5.7 2.3)
(make-iris 5.6 2.8 4.9 2.0)
(make-iris 7.7 2.8 6.7 2.0)
(make-iris 6.3 2.7 4.9 1.8)
(make-iris 6.7 3.3 5.7 2.1)
(make-iris 7.2 3.2 6.0 1.8)
(make-iris 6.2 2.8 4.8 1.8)
(make-iris 6.1 3.0 4.9 1.8)
(make-iris 6.4 2.8 5.6 2.1)
(make-iris 7.2 3.0 5.8 1.6)
(make-iris 7.4 2.8 6.1 1.9)
(make-iris 7.9 3.8 6.4 2.0)
(make-iris 6.4 2.8 5.6 2.2)
(make-iris 6.3 2.8 5.1 1.5)
(make-iris 6.1 2.6 5.6 1.4)
(make-iris 7.7 3.0 6.1 2.3)
(make-iris 6.3 3.4 5.6 2.4)
(make-iris 6.4 3.1 5.5 1.8)
(make-iris 6.0 3.0 4.8 1.8)
(make-iris 6.9 3.1 5.4 2.1)
(make-iris 6.7 3.1 5.6 2.4)
(make-iris 6.9 3.1 5.1 2.3)
(make-iris 5.8 2.7 5.1 1.9)
(make-iris 6.8 3.2 5.9 2.3)
(make-iris 6.7 3.3 5.7 2.5)
(make-iris 6.7 3.0 5.2 2.3)
(make-iris 6.3 2.5 5.0 1.9)
(make-iris 6.2 3.4 5.4 2.3)
(make-iris 5.9 3.0 5.1 1.8)
))
(define iris-classes (list 
0
1
2
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
1
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
2
))
)