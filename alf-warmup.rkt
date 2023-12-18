;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname alf-warmup) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Ryan Tonucci (21059852)
;;Assignment 9

;;Q2a

;;(absolutely-odd lst) produces the sum of the absolute
;;values of the list of integers.
;;Examples
(check-expect (absolutely-odd '(1 -5 4 6 5)) 11)
(check-expect (absolutely-odd '()) 0)
;;absolutely-odd: (listof Int) -> Nat

(define (absolutely-odd lst)
  (foldr + 0 (map abs (filter (lambda (x) (odd? x)) lst))))

;;Tests
(check-expect (absolutely-odd '(2 4 6)) 0)
(check-expect (absolutely-odd '(-2 -3 -5)) 8)

;;Q2b
;;(zip list1 list2) creates an associated list with each index of each list being related.
;;the first being the key and the second is the corresponding values.
;;Examples
(check-expect (zip (list 9 8 7 6) (list 'z 'y 'x 'w))
              (list (list 9 'z) (list 8 'y) (list 7 'x) (list 6 'w)))
(check-expect (zip (list 'a 'b 'c 'd) (list "a" "b" "c" "d"))
              (list (list 'a "a") (list 'b "b") (list 'c "c") (list 'd "d")))
;;zip: (listof Any) (listof Any)->AL
;;Requires list1 and list2 to be equal in length.

(define (zip list1 list2)
  (map list list1 list2))

;;Q2c
;;(unzip AL) produces two lists from an AL
;;one containing all the first elements the other with the second.
;;Examples
(check-expect (unzip '((1 a)(2 b)(3 c))) '((1 2 3) (a b c)))
(check-expect (unzip '()) '(()()))
;;unzip: AL -> (listof X) (listof Y)

(define (unzip AL)
  (list (map first AL)
        (map second AL)))

;;Q2d
;;(dedup lst) procudes a list with only the first instance of
;;each element
;;Examples
(check-expect (dedup '(1 2 1 3 3 2 4)) '(1 2 3 4))
(check-expect (dedup '(1 3 2 1 3 3 6 9 3 2 4)) '(1 3 2 6 9 4))
;;dedup: (listof Num) -> (listof Num)

(define (dedup lst)
  (reverse (foldl (lambda (current acc)
                    (cond [(foldl (lambda (x found) (or found (= current x))) false acc) acc]
                          [else (cons current acc)]))
                  empty lst)))

;;Q2e
;;(zero-fill str) adds "0" to the beginning of the string until
;;it is 20 characters long
;;Examples
(check-expect (zero-fill "abcdefghijklmn") "000000abcdefghijklmn")
(check-expect (zero-fill "he00llo") "0000000000000he00llo")
;;zero-fill: Str -> Str
;;Requires str to be <= 20 characters long.

(define (zero-fill str)
  (list->string
   (append
    (build-list (- 20 (length (string->list str))) (lambda (_) #\0))
    (string->list str))))


;;Q2f
;;(sebsequence lst from to) consumes a list and two natural numbers. It produces the subsequence
;;from lst that begins at index from and ends just before index to.
;;Examples
(check-expect (subsequence '(2 4 5 7 9) 1 4) '(4 5 7))
(check-expect (subsequence '(2 10 4 9 8 4 4 3 5) 3 7) '(9 8 4 4))

(define (subsequence lst from to)
  ((lambda (acc n)
     (foldr (lambda (elem acc)
              (cond [(< (length acc) n)
                      (cons elem acc)]
                    [else acc]))
            empty acc)) (reverse ((lambda (lst n)
                           (foldl (lambda (elem acc)
                                    (cond [(< (length acc) n)
                                           (cons elem acc)]
                                          [else acc]))
                                  empty lst)) lst to)) (- to from)))

(check-expect (subsequence '(1 2 3 4 5) 1 4) '(2 3 4))
(check-expect (subsequence '(a b c d e) 0 2) '(a b))
(check-expect (subsequence '(10 20 30 40 50) 2 5) '(30 40 50))
(check-expect (subsequence '() 0 2) '())
(check-expect (subsequence '(a b c d) 1 1) '())
(check-expect (subsequence '(x y z) 0 3) '(x y z))



