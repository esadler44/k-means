;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname kmeans) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;Question #2

(require "simpledata.rkt")
(require "irisdata.rkt")

;;Needed functions and constants
(define (num-dist num1 num2) (abs (- num1 num2)))

(define (num-avg lon) (cond [(empty? lon) 0] [else (/ (foldr + 0 lon) (length lon))]))

;;----------------------------------------------------------------------------------------
;;a)

(define-struct clusterdata (data index dist))
;; A ClusterData = (make-clusterdata X Nat Num)

;;----------------------------------------------------------------------------------------
;;b)

;;classify-point: (X X -> Num[>=0]) X (ne-listof X) -> ClusterData
;;Purpose: Produces a clusterdata that contains the input data-pt, the index of
;;the nearest cluster centre in the locc (list of cluster-centres), and the distance
;;between the data-pt and its repective cluster centre.
;;Examples:
   (check-expect (classify-point num-dist 0 (list 0 5 9)) (make-clusterdata 0 0 0))
   
   (check-expect (classify-point simple-dist
                                 (make-posn 4 5)
                                 (list
                                  (make-posn 0 1)
                                  (make-posn 3 5)
                                  (make-posn 5 7)))
                 (make-clusterdata (make-posn 4 5) 1 1))



(define (classify-point dist-fn data-pt locc)
  (local [(define index-list (build-list (length locc) (lambda (x) x)))
          ;;These two are used to make the distance list
          (define dist-to-pt (lambda (x) (dist-fn data-pt x)))
          (define dist-list (map dist-to-pt locc))
          ;;These two zip the lists together to associate the distances and the indecies
          (define (zip list1 list2) (map list list1 list2))
          (define list-of-info (zip index-list dist-list))
          ;;These two sort the list of distances and indecies based on the smallest distance
          (define closer-dist-fn (lambda (x y) (< (second x) (second y))))
          (define nearest-cluster (first (quicksort list-of-info closer-dist-fn)))]
    (make-clusterdata data-pt (first nearest-cluster) (second nearest-cluster))))



;;Tests:
(check-expect (classify-point num-dist 4 (list 6 1 3 9 4)) (make-clusterdata 4 4 0))

(check-expect (classify-point num-dist 5 (list 8 6 1 9 7)) (make-clusterdata 5 1 1))

(check-expect (classify-point num-dist 6 (list 3 4 7 9 5 2)) (make-clusterdata 6 2 1))

(check-expect (classify-point simple-dist
                                 (make-posn 1 0)
                                 (list
                                  (make-posn 0 3)
                                  (make-posn 1 2)
                                  (make-posn 4 -2)))
                 (make-clusterdata (make-posn 1 0) 1 2))

(check-expect (classify-point simple-dist
                                 (make-posn 4 4)
                                 (list
                                  (make-posn 4 3)
                                  (make-posn 1 2)
                                  (make-posn 4 5)))
                 (make-clusterdata (make-posn 4 4) 0 1))

;;----------------------------------------------------------------------------------------
;;c)

;;get-new-center: ((listof X) -> X) Nat (listof ClusterData) -> X
;;Purpose: Produces a new cluster centre that is the avergage of all of the data points
;;in the locd (list of ClusterData) by applying the input avg-fn to all of the data-pts
;;that have the same index as the input index.
;;Examples:
   (check-expect (get-new-center num-avg 0 (list (make-clusterdata 1 0 2)
                                                 (make-clusterdata 2 0 1)
                                                 (make-clusterdata 5 1 1)
                                                 (make-clusterdata 0 0 3)
                                                 (make-clusterdata 6 1 0)))
                 1)
   
   (check-expect (get-new-center simple-avg 2 (list (make-clusterdata (make-posn 1 1) 2 1)
                                                    (make-clusterdata (make-posn 4 5) 0 1)
                                                    (make-clusterdata (make-posn -2 0) 2 3)
                                                    (make-clusterdata (make-posn 6 4) 0 2)))
                 (make-posn -0.5 0.5))


   
(define (get-new-center avg-fn index locd)
  (cond
    [(empty? locd) (avg-fn empty)]
    [else
     (local [(define index-filter (lambda (x) (= index (clusterdata-index x))))
             (define same-index-list (filter index-filter locd))
             (define data-pt-list (map clusterdata-data same-index-list))]
       (avg-fn data-pt-list))]))



;;Tests:
(check-expect (get-new-center num-avg 1 empty) 0)

(check-expect (get-new-center num-avg 2 (list (make-clusterdata 4 0 8))) 0)

(check-expect (get-new-center simple-avg 1 empty) (make-posn 0 0))

(check-expect (get-new-center num-avg 1 (list (make-clusterdata 1 1 2)
                                              (make-clusterdata 2 0 1)
                                              (make-clusterdata 5 1 1)
                                              (make-clusterdata 0 0 3)
                                              (make-clusterdata 6 1 0)))
              4)

;;----------------------------------------------------------------------------------------
;;d)

;;get-new-centers: (X X -> Num[>=0]) ((listof X) -> X) (listof X) (listof X) -> (listof X)
;;Purpose: Produces a list of new cluster-centres that are the averages of all of each of the
;;old center's respective data points, calculated by taking in a dist-fn, an avg-fn, a locc
;;(list of cluster centers), and a lodp (list of data points).
;;Examples:
   (check-expect (get-new-centers num-dist num-avg (list 4 10) (list 1 5 12 14)) (list 3 13))
   
   (check-expect (get-new-centers simple-dist simple-avg
                                  (list (make-posn 0 1)
                                        (make-posn -2 -3))
                                  (list
                                   (make-posn 1 1)
                                   (make-posn -3 -2)
                                   (make-posn 1 3)
                                   (make-posn -1 -2)))
                 (list (make-posn 1 2) (make-posn -2 -2)))

   

(define (get-new-centers dist-fn avg-fn locc lodp)
  (local [(define index-list (build-list (length locc) (lambda (x) x)))
          (define locd (map (lambda (x) (classify-point dist-fn x locc)) lodp))
          (define new-centers (map (lambda (index) (get-new-center avg-fn index locd)) index-list))]
    new-centers))



;;Tests:
(check-expect (get-new-centers num-dist num-avg (list 2) (list 8 4 15)) (list 9))

(check-expect (get-new-centers num-dist num-avg (list 8 2) (list 4 8 3 1 6 7)) (list 7 8/3))

(check-expect (get-new-centers simple-dist simple-avg
                               (list (make-posn 5 2))
                               (list
                                (make-posn 5 3)
                                (make-posn 1 1)))
              (list (make-posn 3 2)))

;;----------------------------------------------------------------------------------------
;;e)

;;average-distance: (X X -> Num[>=0]) (ne-listof X) (ne-listof X) -> Num
;;Purpose: Computes the average distortion by taking in a dist-fn, a locc (list of cluster centers,
;;and a lodp (list of data points).
;;Examples:
   (check-expect (average-distance num-dist (list 2 9) (list 1 2 7 0 6 8)) 3/2)
   
   (check-expect (average-distance simple-dist
                                   (list
                                    (make-posn 1 1)
                                    (make-posn 6 6))
                                   (list
                                    (make-posn 0 1)
                                    (make-posn 2 1)
                                    (make-posn 4 6)
                                    (make-posn 5 6)))
                 5/4)




(define (average-distance dist-fn locc lodp)
  (local [(define locd (map (lambda (x) (classify-point dist-fn x locc)) lodp))
          (define dist-list (map (lambda (x) (clusterdata-dist x)) locd))
          (define avg-distortion (/ (foldr + 0 dist-list) (length dist-list)))]
    avg-distortion))


;;Tests:
(check-expect (average-distance num-dist (list 1 2 3) (list 0.8 1.0 1.0 1.8 2.0 2.0 2.8 3.0 3.0)) (/ 0.6 9))

(check-expect (average-distance simple-dist (list (make-posn 0 0)) (list
                                                                    (make-posn 3 4)
                                                                    (make-posn 0 10)
                                                                    (make-posn 5 12)))
              28/3)

;;----------------------------------------------------------------------------------------
;;f)

;;k-means: (X X -> Num[>=0]) ((listof X) -> X) (ne-listof X) (ne-listof X) Num[>0] -> (X -> Nat)
;;Purpose: Produces a function that takes in a data point and produces the index associated with
;;the nearest cluster centre to the input data point, after running the k-means algorithm on
;;the input dist-fn, avg-fn, locc (list of cluster centers), lodp (list of data points), and
;;threshold quantity.
(define (k-means dist-fn avg-fn locc lodp threshold)
  (local [(define new-locc (get-new-centers dist-fn avg-fn locc lodp))]
  (cond
    [(> threshold (abs (- (average-distance dist-fn locc lodp)
                          (average-distance dist-fn new-locc lodp))))
     (lambda (x) (clusterdata-index (classify-point dist-fn x locc)))]
    [else (k-means dist-fn avg-fn new-locc lodp threshold)])))


;;Tests:
;;Natural numbers test
(define num-classifier (k-means num-dist num-avg (list 0 10 20) (list -2 0 1 3 8 9 12 13 18 21 24) 0.01))

(check-expect (num-classifier 7) 1)



;;Simple-data test
(define posn-clusters (list
                       (make-posn 27.07 26.2665)
                       (make-posn 19.2415 -30.6555)
                       (make-posn -21.3195 23.578)
                       (make-posn -23.6035 -31.5695)))
  
(define classifier (k-means simple-dist simple-avg posn-clusters simple-data 0.01))
  
(check-expect (map classifier simple-data) simple-classes)



;;Iris test
(define iris-clusters (list
                       (make-iris 5.1 3.5 1.4 0.2)
                       (make-iris 7.0 3.2 4.7 1.4)
                       (make-iris 6.5 3.0 5.2 2.0)))

(define iris-classifier (k-means iris-dist iris-avg iris-clusters iris-data 0.01))

(define (discrepancy-total list1 list2)
  (cond
    [(empty? list1) 0]
    [(= (first list1) (first list2))
     (discrepancy-total (rest list1) (rest list2))]
    [else (+ 1 (discrepancy-total (rest list1) (rest list2)))]))

(check-expect (discrepancy-total (map iris-classifier iris-data) iris-classes) 12)