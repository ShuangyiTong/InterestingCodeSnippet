#lang racket
(struct AAt (lvl lAAt val rAAt) #:transparent)

(struct 2-tuple (fst snd) #:transparent)

(define (key-method val) val)

(define (skew aat)
  (match aat
    [(AAt lvl (AAt slvl slAAt sval srAAt) val rAAt)
     (cond[(= slvl lvl) (AAt lvl slAAt sval
                             (AAt lvl srAAt val rAAt))]
          [else aat])]
    [t t]))

(define (split aat)
  (match aat
    [(AAt lvl lAAt val
          (AAt slvl slAAt sval
               (AAt sslvl sslAAt ssval ssrAAt)))
     (cond[(and (= lvl slvl) (= slvl sslvl))
           (AAt (+ 1 lvl) (AAt lvl lAAt val slAAt) sval (AAt lvl sslAAt ssval ssrAAt))]
          [else aat])]
    [t t]))

(define (insert val aat)
  (cond[(empty? aat) (AAt 1 empty val empty)]
       [(< (key-method val) (AAt-val aat))
        (split(skew (AAt (AAt-lvl aat)
                         (insert val (AAt-lAAt aat))
                         (AAt-val aat)
                         (AAt-rAAt aat))))]
       [(> (key-method val) (AAt-val aat))
        (split(skew (AAt (AAt-lvl aat)
                         (AAt-lAAt aat)
                         (AAt-val aat)
                         (insert val (AAt-rAAt aat)))))]
       [else (AAt (AAt-lvl aat)
                  (AAt-lAAt aat)
                  val
                  (AAt-rAAt aat))]))

(define (lv aat)
  (if (empty? aat) 0 (AAt-lvl aat)))

(define (singl aat)
  (cond[(empty? aat) #f]
       [(empty? (AAt-rAAt aat) #t)]
       [else (> (AAt-lvl aat) (AAt-lvl (AAt-rAAt aat)))]))

(define (nlvl aat)
  (if (singl aat) (- (lv aat) 1) (lv aat)))

(define (dellrg aat)
  (cond[(empty? (AAt-rAAt aat)) (2-tuple (AAt-lAAt aat) (AAt-val aat))]
       [else (2-tuple (AAt (AAt-lvl aat) (AAt-lAAt aat) (AAt-val aat) (2-tuple-fst (dellrg (AAt-rAAt aat))))
                      (2-tuple-snd (dellrg (AAt-rAAt aat))))]))

(define (adjust aat)
  (cond[(and (= (lv (AAt-lAAt aat)) (- (lv aat) 1))
             (>= (lv (AAt-rAAt aat)) (- (lv aat) 1))) aat]
       [(and (< (lv (AAt-rAAt aat)) (- (lv aat) 1)) (singl (AAt-lAAt aat)))
        (skew (AAt (- (lv aat) 1) (AAt-lAAt aat) (AAt-val aat) (AAt-rAAt aat)))]
       [(< (lv (AAt-rAAt aat)) (- (lv aat) 1))
        (match aat
          [(AAt lvl (AAt slvl slAAt sval (AAt sslvl sslAAt ssval ssrAAt)) val rAAt)
           (AAt (+ sslvl 1) (AAt slvl slAAt sval sslAAt) ssval (AAt (- lvl 1) ssrAAt val rAAt))])]
       [(< (lv (AAt-rAAt aat)) (lv aat)) (split (AAt (- (lv aat) 1) (AAt-lAAt aat) (AAt-val aat) (AAt-rAAt aat)))]
       [else (match aat
               [(AAt lvt lt kt (AAt lvr (AAt lva c ka d) kr b))
                (AAt (+ lva 1) (AAt (- lvt 1) lt kt c) ka (split (AAt (nlvl (AAt lva c ka d)) d kr b)))])]))

(define (delete val aat)
  (cond[(empty? aat) empty]
       [(and (empty? (AAt-lAAt aat)) (= (key-method val) (AAt-val aat))) (AAt-rAAt aat)]
       [(and (empty? (AAt-rAAt aat)) (= (key-method val) (AAt-val aat))) (AAt-lAAt aat)]
       [(< (key-method val) (AAt-val aat)) (adjust (AAt (AAt-lvl aat)
                                                        (delete val (AAt-lAAt aat))
                                                        (AAt-val aat)
                                                        (AAt-rAAt aat)))]
       [(> (key-method val) (AAt-val aat)) (adjust (AAt (AAt-lvl aat)
                                                        (AAt-lAAt aat)
                                                        (AAt-val aat)
                                                        (delete val (AAt-rAAt aat))))]
       [else (adjust (AAt (AAt-lvl aat)
                          (2-tuple-fst (dellrg (AAt-lAAt aat)))
                          (2-tuple-snd (dellrg (AAt-lAAt aat)))
                          (AAt-rAAt aat)))]))
