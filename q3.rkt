;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Ryan Tonucci (21059852)
;;Assignment 9

;;3a
;;(occ lst num) returns how many instances of num
;;are in lst
;;Examples
(check-expect (occ '(2 3 4 4 4 5 6) 4) 3)
(check-expect (occ '(7 2 3 7 4 4 7 4 5 6 7 7) 7) 5)
;;occ: (listof Num) Num -> Nat

(define (occ lst num)
  (length (filter (lambda (x) (= x num)) lst)))

;;3b
;;(pocket-change lst) takes a list of coins and produces
;;the total value of the change.
;;Examples
(check-expect (pocket-change (list 'penny 'penny 'nickel
                                   'quarter 'loonie 'toonie))
              3.32)
(check-expect (pocket-change (list 'penny 'nickel 'dime
                                   'dime
                                   'quarter 'loonie ' toonie
                                   'toonie))
              5.51)
;;pocket-change: (listof Sym) -> Num
;;Requires: Sym to be one of 'penny 'nickle 'dime 'quarter
;;'loonie or 'toonie
;;Requires: Num to be >= 0

(define (pocket-change lst)
  (foldr + 0 (map (lambda (x) (cond [(symbol=? x 'penny) .01]
                                    [(symbol=? x 'nickel) 0.05]
                                    [(symbol=? x 'dime) 0.1]
                                    [(symbol=? x 'quarter) 0.25]
                                    [(symbol=? x 'loonie) 1]
                                    [(symbol=? x 'toonie) 2]
                                    [else 0]))
                  lst)))

;;3c
;;(first-col matrix) produces the first column of the matrix
;;Examples
(check-expect (first-col '((1 2 3 4)
                           (5 6 7 8)
                           (9 10 11 12))) (list 1 5 9))
(check-expect (first-col '((9 7 4)
                           (4 5 8)
                           (3 5 8)
                           (5 6 7))) '(9 4 3 5))
;;first-col: Matrix -> (listof Num)

(define (first-col matrix)
  (map (lambda (x) (first x)) matrix))


;;3d
;;(add1-mat matrix) add 1 to every element in the matrix
;;Examples
(check-expect (add1-mat '((1 2 3 4)
                          (5 6 7 8)
                          (9 10 11 12)))
              (list (list 2 3 4 5)
                    (list 6 7 8 9)
                    (list 10 11 12 13)))
(check-expect (add1-mat '((4 3 2 1)
                          (6 7 5 4)
                          (11 12 10 9)))
              (list (list 5 4 3 2)
                    (list 7 8 6 5)
                    (list 12 13 11 10)))
;;add1-mat: Matrix -> Matrix

(define (add1-mat matrix)
  (map (lambda (x) (map (lambda (y) (add1 y)) x)) matrix))

;;3e
;;(sum-at-zero lst) produces the sum of all the functions where x=0
;;Examples
(check-expect (sum-at-zero (list add1 sqr add1)) 2)
(check-expect (sum-at-zero (list add1 sqrt sub1 add1)) 1)
;;sum-at-zero: (listof (Num -> Num)) -> Num

(define (sum-at-zero lst)
  (foldr + 0 (map (lambda (f) (f 0)) lst)))




















