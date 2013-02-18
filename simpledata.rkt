(module simpledata (lib "plt-pretty-big-text.ss" "lang")
  
  
  (provide simple-dist simple-avg simple-data simple-classes simple-k)
  
;;computes the distance bewteen two Posns using the Euclidean measure
;;simple-dist: Posn Posn -> Num
  (define (simple-dist posn1 posn2)
    (sqrt (+ (sqr (- (posn-x posn1) (posn-x posn2))) 
             (sqr (- (posn-y posn1) (posn-y posn2))))))
  
  ;; computes the average of a list of Posns
  ;;simple-avg: (listof Posn) -> Posn
  (define (simple-avg lop)
    (cond [(empty? lop) (make-posn 0 0)]
          [else (div-posn (foldr add-posn (make-posn 0 0) lop) (length lop))]))

  ;; divides a Posn by a constant - used by simple-avg
  ;;div-posn: Posn Num -> Posn
  (define (div-posn p c)
    (make-posn (/ (posn-x p) c) (/ (posn-y p) c)))

  ;; adds two Posns - used by simple-avg
  ;;add-posn Posn Posn -> Posn
  (define (add-posn p1 p2)
    (make-posn (+ (posn-x p1) (posn-x p2)) (+ (posn-y p1) (posn-y p2))))
  
;; number of clusters in this data
  (define simple-k 4)

;; the data
(define simple-data
(list
 (make-posn 27.07 26.2665)
 (make-posn 19.2415 -30.6555)
 (make-posn -21.3195 23.578)
 (make-posn -23.6035 -31.5695)
 (make-posn -23.6865 -23.274)
 (make-posn -26.963 -23.2555)
 (make-posn -20.313 -33.549)
 (make-posn 26.654 32.951)
 (make-posn -22.966 -27.9795)
 (make-posn -30.6315 27.699)
 (make-posn 25.77 -27.0845)
 (make-posn 25.8115 -23.5525)
 (make-posn 17.6485 -24.643)
 (make-posn -28.436 -25.4695)
 (make-posn 20.7675 29.0645)
 (make-posn 19.3065 22.326)
 (make-posn 27.4245 28.543)
 (make-posn -17.1805 -23.1455)
 (make-posn 18.96 26.137)
 (make-posn 30.088 -25.1145)
 (make-posn 29.941 27.574)
 (make-posn 31.012 23.3385)
 (make-posn -19.5395 24.672)
 (make-posn -28.414 -17.557)
 (make-posn -15.82 21.348)
 (make-posn -23.732 -24.1595)
 (make-posn 21.942 29.965)
 (make-posn -28.4685 -19.9485)
 (make-posn 24.3645 -26.9985)
 (make-posn 25.2715 29.47)
 (make-posn 21.711 -29.447)
 (make-posn 21.1115 25.4835)
 (make-posn -20.239 -15.045)
 (make-posn -27.258 -28.7085)
 (make-posn 29.308 -25.553)
 (make-posn -18.7655 21.7315)
 (make-posn 13.3205 30.676)
 (make-posn -23.824 27.7235)
 (make-posn 30.635 25.8715)
 (make-posn 22.897 -25.749)
 (make-posn -29.717 -33.5435)
 (make-posn 22.3685 28.799)
 (make-posn 29.7955 24.0525)
 (make-posn 22.6305 26.3415)
 (make-posn 30.8495 28.9135)
 (make-posn -24.4675 20.9955)
 (make-posn -28.599 26.1815)
 (make-posn 18.066 27.442)
 (make-posn -25.2065 26.6745)
 (make-posn 26.3055 -24.2055)
 (make-posn 24.2985 -29.7735)
 (make-posn 26.03 -31.613)
 (make-posn -22.5515 -27.943)
 (make-posn 26.4275 -18.3055)
 (make-posn -30.9545 28.3295)
 (make-posn -27.897 20.294)
 (make-posn -32.687 35.2215)
 (make-posn -21.403 28.204)
 (make-posn -28.278 22.7445)
 (make-posn -20.74 28.112)
 (make-posn -23.326 29.8925)
 (make-posn -25.264 30.773)
 (make-posn -23.816 26.834)
 (make-posn -26.531 -18.682)
 (make-posn -32.603 -30.094)
 (make-posn -25.312 -24.061)
 (make-posn -31.352 24.3585)
 (make-posn -21.018 -24.348)
 (make-posn -23.809 21.096)
 (make-posn 20.8565 24.5795)
 (make-posn -23.388 -25.6035)
 (make-posn -24.907 -27.582)
 (make-posn 28.377 22.0995)
 (make-posn 24.3155 18.7995)
 (make-posn 19.852 -30.4055)
 (make-posn -29.0175 -29.702)
 (make-posn -23.304 -32.0155)
 (make-posn 24.139 -28.1115)
 (make-posn 32.7765 20.7955)
 (make-posn -21.496 -31.3995)
 (make-posn 23.69 26.02)
 (make-posn 21.492 34.6195)
 (make-posn 17.562 26.659)
 (make-posn 33.313 26.332)
 (make-posn -29.897 20.9935)
 (make-posn 21.2585 25.859)
 (make-posn 32.1045 17.2305)
 (make-posn -22.047 -24.594)
 (make-posn 27.239 -24.2875)
 (make-posn 21.936 31.156)
 (make-posn 22.2715 25.784)
 (make-posn 16.0165 -24.7675)
 (make-posn 24.226 21.157)
 (make-posn 27.598 26.341)
 (make-posn -26.184 30.652)
 (make-posn -21.9005 -24.995)
 (make-posn 21.232 -28.283)
 (make-posn -28.7415 -23.454)
 (make-posn 25.6495 21.792)
 (make-posn -31.827 26.6645)
 (make-posn -24.278 -21.612)
 (make-posn 20.5215 -28.6835)
 (make-posn 23.362 -23.587)
 (make-posn -27.229 -28.9975)
 (make-posn -29.967 28.7815)
 (make-posn 15.3255 -23.8085)
 (make-posn 18.7475 23.834)
 (make-posn -20.1875 24.937)
 (make-posn 28.7665 26.4325)
 (make-posn 27.216 16.499)
 (make-posn 31.0585 -29.403)
 (make-posn 29.0225 26.9245)
 (make-posn -18.959 -21.4395)
 (make-posn -18.282 -20.339)
 (make-posn -22.3465 36.903)
 (make-posn -22.5935 21.853)
 (make-posn 28.1025 27.411)
 (make-posn -14.5935 28.7)
 (make-posn 21.864 24.976)
 (make-posn 29.579 -19.4455)
 (make-posn 17.6675 30.521)
 (make-posn -15.565 28.9645)
 (make-posn -26.6945 26.5145)
 (make-posn -18.8755 25.916)
 (make-posn 30.8055 28.174)
 (make-posn -28.793 -25.2095)
 (make-posn -24.2185 -25.3)
 (make-posn -25.49 -23.4605)
 (make-posn -29.572 -22.0205)
 (make-posn -31.354 27.9135)
 (make-posn 24.8975 -28.097)
 (make-posn 16.227 18.3415)
 (make-posn 28.3525 -10.451)
 (make-posn -19.374 23.2515)
 (make-posn -27.679 -21.2075)
 (make-posn -19.7445 28.3585)
 (make-posn -28.598 22.5375)
 (make-posn 19.1455 21.0035)
 (make-posn 20.845 22.5635)
 (make-posn 23.6725 27.5)
 (make-posn -24.0805 -18.644)
 (make-posn 32.6265 20.9335)
 (make-posn -18.3105 -22.1065)
 (make-posn -32.05 -27.857)
 (make-posn -32.312 -28.2955)
 (make-posn -23.112 26.214)
 (make-posn -21.233 29.6825)
 (make-posn 23.8785 27.1785)
 (make-posn -26.5055 -29.617)
 (make-posn -27.105 -22.149)
 (make-posn -22.857 20.297)
 (make-posn 27.345 19.935)
 (make-posn -28.948 14.7835)
 (make-posn 16.4045 23.6175)
 (make-posn -16.7595 22.646)
 (make-posn -15.974 21.188)
 (make-posn 27.024 20.885)
 (make-posn 24.7965 -25.825)
 (make-posn -28.2115 23.6715)
 (make-posn 27.5795 20.205)
 (make-posn -26.5945 -20.032)
 (make-posn 21.447 21.382)
 (make-posn 16.531 32.7675)
 (make-posn 24.234 37.1715)
 (make-posn 19.133 -24.7235)
 (make-posn -23.982 27.002)
 (make-posn 22.923 27.2)
 (make-posn 24.71 -21.7945)
 (make-posn -18.139 30.3585)
 (make-posn -20.514 26.894)
 (make-posn -34.8405 18.786)
 (make-posn 20.413 -32.434)
 (make-posn 20.731 21.975)
 (make-posn -26.3655 25.0205)
 (make-posn -18.092 -23.3245)
 (make-posn 19.41 -15.259)
 (make-posn -24.385 26.3575)
 (make-posn -23.9355 27.77)
 (make-posn -20.7815 -21.3335)
 (make-posn 20.8735 -30.3795)
 (make-posn -30.114 -25.3215)
 (make-posn 23.334 -18.7785)
 (make-posn -30.5415 -30.983)
 (make-posn -33.0475 20.0865)
 (make-posn -22.062 23.8385)
 (make-posn 26.8 -29.7665)
 (make-posn -23.4175 -21.242)
 (make-posn 32.904 -30.7385)
 (make-posn -19.7455 -18.0405)
 (make-posn 24.2545 -26.7555)
 (make-posn -24.99 -27.048)
 (make-posn -25.3225 28.068)
 (make-posn -16.991 25.5875)
 (make-posn -17.376 21.8705)
 (make-posn 14.813 34.204)
 (make-posn 27.2115 -20.634)
 (make-posn -31.199 29.7885)
 (make-posn 25.385 -19.884)
 (make-posn 27.8875 24.8325)
 (make-posn 20.161 19.9015)
 (make-posn 21.2865 21.5695)
 (make-posn -24.648 -26.949)
 (make-posn 18.9565 -22.175)
 (make-posn -24.134 -22.9425)
 (make-posn 28.3205 22.766)
 (make-posn 29.1665 29.396)
 (make-posn 19.023 -24.7435)
 (make-posn 22.108 20.62)
 (make-posn -22.9545 -30.7855)
 (make-posn -31.5315 -26.9755)
 (make-posn 23.685 -24.941)
 (make-posn -27.4325 -22.729)
 (make-posn 27.0315 -26.03)
 (make-posn 19.2255 -25.363)
 (make-posn 24.227 30.9435)
 (make-posn 22.7985 25.0595)
 (make-posn -23.3735 -16.477)
 (make-posn -30.512 -32.557)
 (make-posn -28.3015 28.3575)
 (make-posn -23.119 -20.242)
 (make-posn -24.74 29.041)
 (make-posn 21.315 -24.124)
 (make-posn 24.2895 26.3305)
 (make-posn 20.517 24.688)
 (make-posn -26.564 26.2245)
 (make-posn -26.5875 20.1185)
 (make-posn 20.875 25.972)
 (make-posn 25.356 29.6985)
 (make-posn 33.5815 25.9645)
 (make-posn 21.3865 21.4635)
 (make-posn -28.367 -21.9645)
 (make-posn 19.886 30.5605)
 (make-posn -27.1195 -25.849)
 (make-posn 22.1235 21.998)
 (make-posn 27.377 -35.1915)
 (make-posn -24.7835 -27.877)
 (make-posn 34.0265 -20.8405)
 (make-posn -20.848 -28.809)
 (make-posn 22.453 30.2185)
 (make-posn -27.357 -18.656)
 (make-posn 23.5395 -17.534)
 (make-posn -24.388 -21.0075)
 (make-posn 32.31 -29.167)
 (make-posn -23.1245 23.7215)
 (make-posn -28.4985 -26.1445)
 (make-posn 31.83 -20.0505)
 (make-posn 28.5775 -27.3135)
 (make-posn 17.577 -32.4285)
 (make-posn -20.037 -26.4405)
 (make-posn -20.2615 -19.1685)
 (make-posn 27.933 22.5405)
 (make-posn -14.938 27.7515)
 (make-posn 21.0115 -9.7645)
 (make-posn 26.6245 22.7725)
 (make-posn -19.559 23.6305)
 (make-posn -24.586 -26.027)
 (make-posn 36.0595 -21.0095)
 (make-posn -23.1295 -26.1875)
 (make-posn -18.219 30.447)
 (make-posn 22.57 27.406)
 (make-posn -28.833 26.454)
 (make-posn 29.6785 25.8055)
 (make-posn -26.0185 -19.834)
 (make-posn 24.424 27.214)
 (make-posn -21.7925 -22.425)
 (make-posn -27.137 16.266)
 (make-posn -27.7665 -24.723)
 (make-posn 19.594 29.093)
 (make-posn -24.1695 -27.5025)
 (make-posn -26.4985 -20.2215)
 (make-posn 21.672 -26.121)
 (make-posn -21.6555 -24.5455)
 (make-posn 23.968 17.4335)
 (make-posn -27.6965 -23.1895)
 (make-posn 37.1875 27.3245)
 (make-posn -27.9465 -22.928)
 (make-posn 26.7565 -28.986)
 (make-posn -24.0535 28.9345)
 (make-posn 33.541 -24.1145)
 (make-posn 31.701 24.743)
 (make-posn 24.118 27.2935)
 (make-posn -16.8015 -26.904)
 (make-posn 21.3775 -22.2545)
 (make-posn -31.3905 -31.3655)
 (make-posn 23.571 -17.808)
 (make-posn -25.9215 -22.4905)
 (make-posn -29.2075 -28.7365)
 (make-posn -22.607 20.037)
 (make-posn 26.235 -28.782)
 (make-posn 30.248 -20.9575)
 (make-posn 27.496 25.798)
 (make-posn -22.5185 26.664)
 (make-posn -17.063 30.211)
 (make-posn 20.0805 -25.3855)
 (make-posn -22.776 -28.9775)
 (make-posn -29.0005 -29.124)
 (make-posn 24.4305 27.9795)
 (make-posn -28.98 -28.968)
 (make-posn -19.1055 -17.817)
 (make-posn -36.4615 31.641)
 (make-posn 23.8205 21.944)
 (make-posn -18.4945 22.188)
 (make-posn -19.6825 -24.8015)
 (make-posn -23.9345 25.135)
 (make-posn 26.2545 26.7725)
 (make-posn -21.2565 -23.1065)
 (make-posn -32.0195 29.769)
 (make-posn 27.5485 28.85)
 (make-posn -27.0585 -29.481)
 (make-posn -19.9725 -31.9835)
 (make-posn 22.7755 -26.0175)
 (make-posn -20.9255 27.824)
 (make-posn 20.8195 28.2105)
 (make-posn 31.8335 28.808)
 (make-posn -24.986 -25.994)
 (make-posn -21.318 31.3835)
 (make-posn 27.175 25.264)
 (make-posn -29.216 -21.1235)
 (make-posn -23.8015 -31.424)
 (make-posn 28.2315 -30.5805)
 (make-posn -31.8245 25.968)
 (make-posn 25.68 32.563)
 (make-posn 30.532 -18.2415)
 (make-posn -22.318 21.686)
 (make-posn -25.673 -28.7565)
 (make-posn -21.1305 -29.1925)
 (make-posn -24.5115 28.268)
 (make-posn -24.151 29.951)
 (make-posn 25.6685 22.2575)
 (make-posn 34.4715 23.8355)
 (make-posn -29.803 19.556)
 (make-posn -29.4155 30.7095)
 (make-posn -21.4125 -27.6625)
 (make-posn -24.2655 -23.5465)
 (make-posn -24.2025 31.0075)
 (make-posn -20.841 21.021)
 (make-posn 24.2485 -24.6015)
 (make-posn -25.4335 27.3425)
 (make-posn 20.139 31.2255)
 (make-posn -21.2915 28.387)
 (make-posn 18.13 27.06)
 (make-posn 24.284 20.7265)
 (make-posn -24.7315 -19.467)
 (make-posn -11.619 20.2085)
 (make-posn -21.217 23.7975)
 (make-posn 30.7675 31.729)
 (make-posn 25.273 -29.3995)
 (make-posn 18.093 19.831)
 (make-posn -24.2605 -27.321)
 (make-posn -23.104 -26.047)
 (make-posn -26.682 -21.454)
 (make-posn 28.519 -26.1725)
 (make-posn 24.529 25.832)
 (make-posn 25.977 -25.1145)
 (make-posn 25.6025 19.0485)
 (make-posn 25.844 -25.1115)
 (make-posn -27.949 26.553)
 (make-posn 27.6165 -26.203)
 (make-posn -36.1525 -30.5615)
 (make-posn -29.856 -22.494)
 (make-posn -33.7835 26.8505)
 (make-posn 22.1445 28.587)
 (make-posn 23.428 -30.832)
 (make-posn -27.503 -25.017)
 (make-posn -32.608 -25.052)
 (make-posn -22.3555 -30.6365)
 (make-posn -21.346 -18.7365)
 (make-posn -21.4945 24.334)
 (make-posn 26.5595 23.8175)
 (make-posn -30.1885 18.067)
 (make-posn -28.5575 -21.58)
 (make-posn -23.7825 -18.561)
 (make-posn -20.151 -29.3265)
 (make-posn 18.32 -25.1765)
 (make-posn 29.847 -24.272)
 (make-posn -27.8115 -21.3035)
 (make-posn -28.524 -28.545)
 (make-posn 23.773 -23.697)
 (make-posn -23.012 12.425)
 (make-posn -33.756 -18.534)
 (make-posn -24.9085 -20.592)
 (make-posn -20.0325 -18.4275)
 (make-posn 30.5585 30.5845)
 (make-posn 26.614 -27.1875)
 (make-posn 27.1285 -18.2425)
 (make-posn -32.785 -30.1055)
 (make-posn -14.046 25.9395)
 (make-posn -21.912 26.124)
 (make-posn -25.814 -23.8885)
 (make-posn 19.971 -19.996)
 (make-posn -21.244 26.991)
 (make-posn -22.5485 -18.47)
 (make-posn -31.318 25.287)
 (make-posn 26.034 26.8405)
 (make-posn 21.083 27.3325)
 (make-posn 16.057 18.672)
 (make-posn 27.5375 -25.4415)
 (make-posn 28.781 25.6575)
 (make-posn -31.9335 -25.4155)
 (make-posn 19.5595 20.6545)
 (make-posn -23.694 28.859)
 (make-posn -29.1515 26.8585)
 (make-posn -34.6685 33.988)
 (make-posn 20.2745 26.931)
 (make-posn 29.076 -22.5605)
 (make-posn -16.87 20.997)
 (make-posn -35.57 28.319)
 (make-posn -27.434 -20.9535)
 (make-posn 26.921 -21.411)
 (make-posn 28.546 28.8565)
 (make-posn -22.51 29.5105)
 (make-posn 25.0165 33.0215)
 (make-posn 26.862 -22.34)
 (make-posn 21.9145 -27.7285)
 (make-posn -25.747 -22.308)
 (make-posn 26.1065 -15.4325)
 (make-posn -26.608 -29.1995)
 (make-posn 33.4855 23.3565)
 (make-posn 28.0075 -21.234)
 (make-posn 19.274 24.002)
 (make-posn -19.23 28.781)
 (make-posn -30.011 16.6875)
 (make-posn -27.103 -25.4015)
 (make-posn -24.082 -30.1625)
 (make-posn 25.5455 25.507)
 (make-posn 27.259 -31.917)
 (make-posn 28.6745 33.7065)
 (make-posn 20.8215 -26.0045)
 (make-posn 31.5215 -16.5745)
 (make-posn -27.263 17.6505)
 (make-posn -22.8925 -24.644)
 (make-posn 24.807 21.284)
 (make-posn -28.354 29.137)
 (make-posn 39.603 -22.4065)
 (make-posn -17.4405 -33.5305)
 (make-posn 26.0695 -22.2125)
 (make-posn -24.977 -24.643)
 (make-posn 26.8025 27.423)
 (make-posn -22.918 -25.3585)
 (make-posn 25.6975 25.1495)
 (make-posn -28.381 -29.9855)
 (make-posn -24.951 -26.5025)
 (make-posn -27.6305 20.958)
 (make-posn -31.3255 -27.931)
 (make-posn 26.8855 -23.088)
 (make-posn -24.223 -22.7015)
 (make-posn 17.968 21.782)
 (make-posn 24.41 28.543)
 (make-posn 22.266 -23.6355)
 (make-posn -16.668 16.8115)
 (make-posn -20.3115 24.329)
 (make-posn -27.585 26.5265)
 (make-posn -24.359 -29.38)
 (make-posn 27.3865 -25.5495)
 (make-posn -20.525 -29.6705)
 (make-posn 25.09 26.923)
 (make-posn -30.916 20.903)
 (make-posn 17.972 -30.7805)
 (make-posn 25.6765 26.0265)
 (make-posn 24.5955 34.013)
 (make-posn -32.04 -30.938)
 (make-posn 22.9545 29.1765)
 (make-posn 24.8985 31.676)
 (make-posn 23.3425 17.3095)
 (make-posn -22.684 27.676)
 (make-posn 25.8845 -29.3345)
 (make-posn -28.75 22.5805)
 (make-posn 24.983 22.598)
 (make-posn -26.6 -26.3475)
 (make-posn -28.6505 37.0355)
 (make-posn 21.13 -27.1205)
 (make-posn -24.2785 19.37)
 (make-posn 11.2385 11.5995)
 (make-posn 24.3765 -23.4425)
 (make-posn 22.842 -30.229)
 (make-posn -30.9865 -27.6645)
 (make-posn -22.9275 31.9535)
 (make-posn -23.634 -27.95)
 (make-posn 21.116 -22.122)
 (make-posn 24.513 19.366)
 (make-posn -23.582 -25.2305)
 (make-posn -28.911 -38.045)
 (make-posn -19.1655 16.0745)
 (make-posn -20.9185 -28.3725)
 (make-posn -15.085 21.981)
 (make-posn 17.638 28.114)
 (make-posn 22.3295 21.2475)
 (make-posn -25.7675 -31.714)
 (make-posn 30.3675 -25.288)
 (make-posn -14.0835 -24.113)
 (make-posn 30.0035 -35.8485)
 (make-posn 20.359 26.937)
 (make-posn 20.279 23.6195)
 (make-posn 26.4515 -20.6375)
 (make-posn 31.472 20.153)
 (make-posn 27.909 -26.5155)
 (make-posn -29.422 23.445)
 (make-posn -28.973 22.7865)
 (make-posn -31.87 -23.4165)
 (make-posn -22.4245 -28.235))
)

(define simple-classes
(list
 0
 1
 2
 3
 3
 3
 3
 0
 3
 2
 1
 1
 1
 3
 0
 0
 0
 3
 0
 1
 0
 0
 2
 3
 2
 3
 0
 3
 1
 0
 1
 0
 3
 3
 1
 2
 0
 2
 0
 1
 3
 0
 0
 0
 0
 2
 2
 0
 2
 1
 1
 1
 3
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
 3
 3
 3
 2
 3
 2
 0
 3
 3
 0
 0
 1
 3
 3
 1
 0
 3
 0
 0
 0
 0
 2
 0
 0
 3
 1
 0
 0
 1
 0
 0
 2
 3
 1
 3
 0
 2
 3
 1
 1
 3
 2
 1
 0
 2
 0
 0
 1
 0
 3
 3
 2
 2
 0
 2
 0
 1
 0
 2
 2
 2
 0
 3
 3
 3
 3
 2
 1
 0
 1
 2
 3
 2
 2
 0
 0
 0
 3
 0
 3
 3
 3
 2
 2
 0
 3
 3
 2
 0
 2
 0
 2
 2
 0
 1
 2
 0
 3
 0
 0
 0
 1
 2
 0
 1
 2
 2
 2
 1
 0
 2
 3
 1
 2
 2
 3
 1
 3
 1
 3
 2
 2
 1
 3
 1
 3
 1
 3
 2
 2
 2
 0
 1
 2
 1
 0
 0
 0
 3
 1
 3
 0
 0
 1
 0
 3
 3
 1
 3
 1
 1
 0
 0
 3
 3
 2
 3
 2
 1
 0
 0
 2
 2
 0
 0
 0
 0
 3
 0
 3
 0
 1
 3
 1
 3
 0
 3
 1
 3
 1
 2
 3
 1
 1
 1
 3
 3
 0
 2
 1
 0
 2
 3
 1
 3
 2
 0
 2
 0
 3
 0
 3
 2
 3
 0
 3
 3
 1
 3
 0
 3
 0
 3
 1
 2
 1
 0
 0
 3
 1
 3
 1
 3
 3
 2
 1
 1
 0
 2
 2
 1
 3
 3
 0
 3
 3
 2
 0
 2
 3
 2
 0
 3
 2
 0
 3
 3
 1
 2
 0
 0
 3
 2
 0
 3
 3
 1
 2
 0
 1
 2
 3
 3
 2
 2
 0
 0
 2
 2
 3
 3
 2
 2
 1
 2
 0
 2
 0
 0
 3
 2
 2
 0
 1
 0
 3
 3
 3
 1
 0
 1
 0
 1
 2
 1
 3
 3
 2
 0
 1
 3
 3
 3
 3
 2
 0
 2
 3
 3
 3
 1
 1
 3
 3
 1
 2
 3
 3
 3
 0
 1
 1
 3
 2
 2
 3
 1
 2
 3
 2
 0
 0
 0
 1
 0
 3
 0
 2
 2
 2
 0
 1
 2
 2
 3
 1
 0
 2
 0
 1
 1
 3
 1
 3
 0
 1
 0
 2
 2
 3
 3
 0
 1
 0
 1
 1
 2
 3
 0
 2
 1
 3
 1
 3
 0
 3
 0
 3
 3
 2
 3
 1
 3
 0
 0
 1
 2
 2
 2
 3
 1
 3
 0
 2
 1
 0
 0
 3
 0
 0
 0
 2
 1
 2
 0
 3
 2
 1
 2
 0
 1
 1
 3
 2
 3
 1
 0
 3
 3
 2
 3
 2
 0
 0
 3
 1
 3
 1
 0
 0
 1
 0
 1
 2
 2
 3
 3)
)
)