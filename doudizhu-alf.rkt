;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname doudizhu-alf) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Ryan Tonucci (21059852)
;;Assignment 9

;;Q4a
;;(hand->ccal hand) produces a CCAL from a Hand
;;Examples
(check-expect (hand->ccal (list 4 4 6 7 8 8))
              (list (list 4 2) (list 6 1)
                    (list 7 1) (list 8 2)))
(check-expect (hand->ccal (list 4 4 6 7 8 8 'Ace 'Ace 'Ace 'Red))
              (list (list 4 2) (list 6 1)
                    (list 7 1) (list 8 2)
                    (list 'Ace 3) (list 'Red 1)))

;find first element, count how many instances,
;;then filter it from the rest of the list

(define (hand->ccal hand)
  ;;zip them together
  (map list ;need to produce the first elements
       (reverse ((lambda (hand) (foldl (lambda (x acc)
                                         (cond [(member? x acc)
                                                acc]
                                               [else (cons x acc)]))
                                       empty
                                       hand)) hand))
       ;produces the second elements of AL
       (reverse (map (lambda (x) (length (filter (lambda (y) (cond [(and (number? x)
                                                                         (number? y))
                                                                    (= x y)]
                                                                   [(and (symbol? x)
                                                                         (symbol? y))
                                                                    (symbol=? x y)]
                                                                   [else false])) hand)))
                     ((lambda (hand) (foldl (lambda (x acc)
                                              (cond [(member? x acc)
                                                     acc]
                                                    [else (cons x acc)]))
                                            empty
                                            hand)) hand)))))
              

;;Q4b
;;(find-kind CCAL)
;;Examples
(check-expect (find-kind 2 (list (list 4 2) (list 7 1) (list 8 1) (list 'Jack 2))) (list 4 'Jack))
(check-expect (find-kind 5 (list (list 4 5) (list 7 1) (list 8 1) (list 'Jack 2))) (list 4))


(define (find-kind n CCAL)
  (map first (filter (lambda (lst) ((lambda (x) (>= x n)) (second lst))) CCAL)))
  

;;Test
(check-expect (find-kind 2 (list (list 5 2) (list 6 1) (list 8 2)
                                 (list 'Ace 2) (list 'Black 1))) (list 5 8 'Ace))

;;4c
(check-expect (trios (list (list 4 3) (list 7 1) (list 8 1) (list 'Jack 3))) (list 4 'Jack))
(check-expect (trios (list (list 4 5) (list 7 1) (list 8 1) (list 'Jack 2))) (list 4))
(check-expect (trios (list (list 4 5) (list 7 1) (list 8 1) (list 'Jack 2))) (list 4))

(define trio 3)

(define (trios CCAL)
  (map first (filter (lambda (lst) ((lambda (x) (>= x trio)) (second lst))) CCAL)))

;;4d
(check-expect (ccal->hand (list (list 4 3) (list 6 1)
                                (list 7 1) (list 8 2)))
              (list 4 4 4 6 7 8 8))
(check-expect (ccal->hand (list (list 4 2) (list 6 1)
                                (list 7 1) (list 8 2)
                                (list 'Ace 3) (list 'Red 1))) (list 4 4 6 7 8 8 'Ace 'Ace 'Ace 'Red))

;; card-foldr-n: (Card Nat X -> X) Card X Nat -> X ;; Requires: Nat >= 0

(define (ccal->hand ccal)
  (local
    [(define (card-foldr-n combine card base n)
       (cond [(zero? n) base]
             [else (combine card n
                            (card-foldr-n combine card base (sub1 n)))]))]
    (foldr append empty (map (lambda (card)
                               (card-foldr-n
                                (lambda (card n acc)
                                  (append (build-list (- (+ n 1) n) (lambda (index) card)) acc))
                                (first card) empty (second card)))
                             ccal))))

;;4e
;;Tests
(check-expect (remove-one-of-each
               (list 3 3 3 3 4 5 6 7 7 7 'Jack 'Jack 'Queen 'King 'Ace 2 2)) (list 3 3 3 7 7 'Jack 2))
(check-expect (remove-one-of-each (list 3 3 4 6 9 'Jack 'Ace)) (list 3))
(check-expect (remove-one-of-each (list 3 4 6 9 'Jack 'Jack 'Jack 'Ace 'Ace)) (list 'Jack 'Jack 'Ace))
(check-expect (remove-one-of-each (list 5 7 8)) (list))
;(check-expect (remove-one-of-each (list 5 7 7 8)) (list 7))
(check-expect (remove-one-of-each (list 5 5 5 7 7 8 9 9)) (list 5 5 7 9))
(check-expect (remove-one-of-each empty) empty)
(check-expect (remove-one-of-each (list 3 3 6 7 7 9 9 9 'Queen 2 2))
              (list 3 7 9 9 2))
(check-expect (remove-one-of-each (list 4 5 9 'Jack 'Jack 'Queen
                                        'King 'Black 'Black))
              (list 'Jack 'Black))

;always keeps the second value?
;how can I check if it's second?
(define (remove-one-of-each lst)
  (cond [(empty? lst) empty]
        [else (foldr (lambda (card result)
                       (cond
                         [(and (equal? (second lst) card)
                               (not (equal? (first lst) (second lst))))
                          (rest result)]
                         [(empty? result) (list card)]
                         [(not (equal? card (first result))) (cons card (rest result))]
                         [else (cons card result)]))
                     empty (rest lst))]))

;stat: only use foldr to append all of the 

;;4f
(check-expect (remove-one-of-each2
               (list 3 3 3 3 4 5 6 7 7 7 'Jack 'Jack 'Queen 'King 'Ace 2 2)) (list 3 3 3 7 7 'Jack 2))
(check-expect (remove-one-of-each2 (list 3 3 4 6 9 'Jack 'Ace)) (list 3))
(check-expect (remove-one-of-each2 (list 3 3 4 6 9 'Jack 'Jack 'Jack 'Ace 'Ace)) (list 3 'Jack 'Jack 'Ace))
(check-expect (remove-one-of-each2 (list 5 7 8)) (list))
(check-expect (remove-one-of-each2 (list 5 7 7 8)) (list 7))
(check-expect (remove-one-of-each2 (list 5 5 5 7 7 8 9 9)) (list 5 5 7 9))

(define (remove-one-of-each2 hand)
  (ccal->hand (map (lambda (ccal) (list (first ccal) (sub1 (second ccal)))) (hand->ccal hand))))


        
