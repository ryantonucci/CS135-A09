;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname doudizhu) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;A Hand is a type that requires a list of playing cards.
;;Q3a
;;(solos Hand) pruduces a list of lists each containing one element.
;;No list is replicated more than once in the result.
(check-expect (solos (list 3 5 5 8 9 'Queen 'King 2))
              (list (list 3) (list 5) (list 8) (list 9) (list 'Queen) (list 'King) (list 2)))
(check-expect (solos (list 3 3 3 5 5 8 9 'Queen 'King 2))
              (list (list 3) (list 5) (list 8) (list 9) (list 'Queen) (list 'King) (list 2)))
;;solos: listof Hand->listof Hand

(define (solos list-of-cards)
  (cond [(empty? list-of-cards) empty]
        [(member? (first list-of-cards) (rest list-of-cards)) (solos (rest list-of-cards))]
        [else (cons (list (first list-of-cards)) (solos (rest list-of-cards)))]))


;;Q3b
;;(pairs list-of-cards) produces a list of pairs in the hand
;;Examples
(check-expect (pairs (list 4 4 5 7 7 10 'Jack 'Jack 'King 2))
              (list (list 4 4) (list 7 7) (list 'Jack 'Jack))) 
(check-expect (pairs (list 3 3 3 4 5 5 6 7 7 7 8 8 8 8 8 10 'Jack 'Jack 'King 'Ace 'Ace 2 'Red))
              (list (list 3 3) (list 5 5) (list 7 7) (list 8 8) (list 'Jack 'Jack) (list 'Ace 'Ace)))
;;pairs: listof Hand->listof Hand

(define pair-length 2)

(define (pairs list-of-cards)
  (one-instance
   (cond [(empty? (find-kind pair-length list-of-cards)) empty]
         [(member? (first list-of-cards) (find-kind pair-length list-of-cards))
          (cons (list (first list-of-cards) (first list-of-cards)) (pairs (rest list-of-cards)))]
         [else (pairs (rest list-of-cards))])))

(check-expect (pairs (list 5 5 6 7 8 8 'Ace 'Ace 2 2 2 'Black)) (list (list 5 5) (list 8 8) (list 'Ace 'Ace) (list 2 2)))
(check-expect (pairs (list 3 3 4 4 5 5 10 10 'Jack 'Jack 'Queen 'Queen 'King 'King))
              (list (list 3 3) (list 4 4) (list 5 5) (list 10 10) (list 'Jack 'Jack)
                    (list 'Queen 'Queen) (list 'King 'King)))

;;(find-kind n list-of-cards) finds cards which have n number of instances or more
;;Examples
(check-expect (find-kind 2 (list 4 4 7 8 'Jack 'Jack)) (list 4 'Jack))
(check-expect (find-kind 5 (list 4 4 4 4 4 4 7 8 'Jack 'Jack)) (list 4))
;;find-kind: (Num Hand)->Hand
;;Requires n>=1 and list-of-cards to be sorted according to game rules.

(define (find-kind n list-of-cards)
  (cond [(empty? list-of-cards) empty]
        [(= n 1) (one-instance list-of-cards)] ;should return one instance of each card
        [(<= n (length (list-of-instances (first list-of-cards) list-of-cards)))
         (one-instance (cons (first list-of-cards) (find-kind n (rest list-of-cards))))]
        [else (find-kind n (rest list-of-cards))]))
        
(check-expect (find-kind 1 (list 4 4 7 8 'Jack 'Jack)) (list 4 7 8 'Jack))
(check-expect (find-kind 2 (list 5 5 6 7 8 8 'Ace 'Ace 'Black)) (list 5 8 'Ace))
(check-expect (find-kind 3 (list 5 5 5 5 6 7 8 8 8 'Ace 'Ace 'Black 'Red)) (list 5 8))
(check-expect (find-kind 2 (list 5 5 6 7 8 8 'Ace 'Ace 2 2 2 'Black)) (list 5 8 'Ace 2))

;;(one-instance list-of-cards) Returns the list of cards without multiple instances of the same card
;;Examples
(check-expect (one-instance (list 4 4 5 9 9 'Jack 'Queen 'Queen)) (list 4 5 9 'Jack 'Queen))
(check-expect (one-instance (list 4 5 9 'Jack  'Queen)) (list 4 5 9 'Jack 'Queen))
(check-expect (one-instance (list 4 4 4 4 5 9 9 9 9 9 'Jack 'Queen 'Queen)) (list 4 5 9 'Jack 'Queen))
;;one-instance: listof Any->listof Any

(define (one-instance list-of-cards) 
  (cond [(empty? list-of-cards) empty]
        [(member? (first list-of-cards) (rest list-of-cards)) (one-instance (rest list-of-cards))]
        [else (cons (first list-of-cards) (one-instance (rest list-of-cards)))]))

;;(list-of-instances element list-of-cards) Creates a list of element in list-of-cards
;;Examples
(check-expect (list-of-instances 5 (list 5 5 5 5 6 8 8 9 'Jack 'Queen 'King)) (list 5 5 5 5))
(check-expect (list-of-instances 'Jack (list 5 5 6 9 'Jack 'Jack 'Queen 'King 'Ace 2)) (list 'Jack 'Jack))
(check-expect (list-of-instances 4 (list 4 4 5 7 7 10 'Jack 'Jack 'King 2)) (list 4 4))
;;list-of-instances: Any (listof Any)->listof Any

(define (list-of-instances element list-of-cards) 
  (cond [(empty? list-of-cards) empty]
        [(cond [(and (number? element) (number? (first list-of-cards)))
                (= element (first list-of-cards))]
               [(and (symbol? element) (symbol? (first list-of-cards))) (symbol=? element (first list-of-cards))] 
               [else false])
         (cons element (list-of-instances element (rest list-of-cards)))]
        [(cons? list-of-cards) (list-of-instances element (rest list-of-cards))])) 

;;Q3c
;;(trios list-of-cards)
;;Examples
(check-expect (trios (list 3 3 4 4 4 7 9 9 'Jack 'King 'King 'King 2 2 2 2))
              (list (list 4 4 4) (list 'King 'King 'King) (list 2 2 2)))
(check-expect (trios (list 3 3 3 3 4 4 10 10 10 'Jack 'Queen 'Queen 'King))
              (list (list 3 3 3) (list 10 10 10)))
;;trios: listof Hand->listof Any
;;Requires Any to be a valid card type (2-10, 'Jack 'Queen 'King 'Ace 'Black "Red)

(define trio-length 3)

(define (trios list-of-cards)
  (one-instance (cond [(empty? (find-kind trio-length list-of-cards)) empty]
                      [(member? (first list-of-cards) (find-kind trio-length list-of-cards))
                       (cons (list (first list-of-cards) (first list-of-cards) (first list-of-cards))
                             (trios (rest list-of-cards)))]
                      [else (trios (rest list-of-cards))])))

;;Q3d
;;(sort-hands list-of-hands) sorts a list of cards  
;;Examples
(check-expect (sort-hands (list (list 6 7 8) (list 'Jack 'Jack) (list 4 4) (list 4 4) (list 2 2 2)
                                (list 'Black 'Red)))
              (list (list 4 4) (list 4 4) (list 'Jack 'Jack) (list 6 7 8)
                    (list 2 2 2) (list 'Black 'Red)))

(check-expect (sort-hands (list (list 3 4 5 6 7) (list 'Jack 'Jack) (list 'Queen 'Queen)
                                (list 4 4) (list 2 2) (list 'Black 'Red) (list 9 9 9 9)
                                (list 7 7 7) (list 7 8 9)))
              (list (list 4 4) (list 'Jack 'Jack) (list 'Queen 'Queen)
                    (list 2 2) (list 7 7 7) (list 7 8 9) (list 3 4 5 6 7) (list 9 9 9 9) (list 'Black 'Red)))
;;sort-hands: Hand->Hand

(define (sort-hands list-of-hands)
  (cond [(empty? list-of-hands) empty]
        [else (insert (first list-of-hands)
                      (sort-hands (rest list-of-hands)))]))

;;(insert hand sorted-list-of-hands) inserts a card into the sorted hand
;;Example
(check-expect (insert (list 9 9) (list (list 4 4) (list 'Jack 'Jack) (list 4 5 6)))
              (list (list 4 4) (list 9 9) (list 'Jack 'Jack) (list 4 5 6)))
;;insert: Hand listof Hand->listof Hand

(define (insert hand sorted-list-of-hands)
  (cond [(empty? sorted-list-of-hands) (cons hand empty)]
        [(hand<? hand (first sorted-list-of-hands)) (cons hand sorted-list-of-hands)]
        [else (cons (first sorted-list-of-hands) (insert hand (rest sorted-list-of-hands)))]))


;;(Rocket? list-of-cards) determines whether a set of cards is a rocket.
;;Examples
(check-expect (Rocket? (list 'Black 'Red)) true)
(check-expect (Rocket? (list 'Red 'Black)) false)
(check-expect (Rocket? (list 5 5)) false)
;;Rocket?: Hand->Bool

(define (Rocket? list-of-cards)
  (cond [(and (symbol? (first list-of-cards)) (symbol? (second list-of-cards))
              (symbol=? (first list-of-cards) 'Black)
              (symbol=? (second list-of-cards) 'Red)
              (= (length list-of-cards) 2)) true]
        [else false]))

;;(Bomb? list-of-cards) determines whether a set of cards is a bomb.
;;Examples
(check-expect (Bomb? (list 6 6 6 6)) true)
(check-expect (Bomb? (list 'Black 'Red)) false)
(check-expect (Bomb? (list 4 5 6)) false)
;;Rocket?: Hand->Bool

(define bomb-length 4)

(define (Bomb? list-of-cards)
  (cond [(and (= (length list-of-cards) bomb-length)
              (member? (first list-of-cards) (find-kind bomb-length list-of-cards))) true]
        [else false]))

;;(hand<? hand1 hand2) Determines whether hand1 is less than hand2
;;Examples
(check-expect (hand<? (list 5 5) (list 6 6)) true)
(check-expect (hand<? (list 'Black 'Red) (list 5 6 7 8)) false)
(check-expect (hand<? (list 5 5 5) (list 5 6 7)) true)
(check-expect (hand<? (list 9 9) (list 4 4 4)) true)

               
(define (hand<? hand1 hand2)
  (cond [(Rocket? hand1) false]
        [(and (Rocket? hand2) (not (Rocket? hand1))) true]
        [(and (Bomb? hand2) (not (or (Bomb? hand1) (Rocket? hand1)))) true]
        [(and (Rocket? hand1) (not (Rocket? hand2))) false]
        [(and (Bomb? hand1) (or (Bomb? hand2) (Rocket? hand2))) false]
        [(and (< (length hand1) (length hand2)) (not (Rocket? hand1)) (not (Bomb? hand1))) true]
        [(and (> (length hand1) (length hand2)) (not (Rocket? hand1)) (not (Bomb? hand1))) false]
        [else (tie-breaker hand1 hand2)]))


;;(tie-breaker hand1 hand2) if the hands are the same length this will check the first element
;;to see which one is the greatest if the firsts are the same it will continue to the seconds
;;and so on.
;;Examples
(check-expect (tie-breaker (list 4 4 4) (list 4 5 6)) true)
(check-expect (tie-breaker (list 'Jack 'Jack 'Jack) (list 'Jack 'Queen 'King)) true)
;;tie-breaker: Hand Hand->Bool

(define (tie-breaker hand1 hand2) 
  (cond [(empty? hand1) false]
        [(> (first (Sym->Num hand2)) (first (Sym->Num hand1))) true]
        [(< (first (Sym->Num hand2)) (first (Sym->Num hand1))) false]    
        [else (tie-breaker (rest hand1) (rest hand2))]))

(check-expect (tie-breaker (list 5 6 7) (list 4 4 4)) false)
(check-expect (tie-breaker (list 5 6 7) (list 5 5 5)) false)
(check-expect (tie-breaker (list 9 9 9) (list 'Jack 'Queen 'King)) true)

;;(Sym->Num list-of-cards) Changes cards from symbols or numbers to number values.
;;Examples
(check-expect (Sym->Num (list 'Ace 'King 2 4)) (list 14 13 15 4))
(check-expect (Sym->Num (list 4 7 8 'Jack 'Jack 'Queen 'King 'Ace 2 'Black 'Red))
              (list 4 7 8 11 11 12 13 14 15 16 17))
;;Sym->Num: listof Any->listof Nat
;;Requires listof Any constists of valid cards.

(define (Sym->Num list-of-cards)  
  (cond [(empty? list-of-cards) empty]
        [else (cons (card->value (first list-of-cards))
                    (Sym->Num (rest list-of-cards)))]))

;;(card->value card) ;gives a card a value (important for cards represented by symbols)
;;Examples
(check-expect (card->value 2) 15)
(check-expect (card->value 6) 6)
(check-expect (card->value 'Jack) 11)
;;card->value Any->Nat
;;Requires Any to be a valid card. (2-10 or 'Jack 'Queen 'King 'Ace 'Black 'Red)

(define (card->value card)  ;Takes a card and gives it a value
  (cond [(and (number? card) (>= card 3) (<= card 17)) card]
        [(and (number? card) (= card 2)) 15]
        [(symbol=? card 'Jack) 11]
        [(symbol=? card 'Queen) 12]
        [(symbol=? card 'King) 13]
        [(symbol=? card 'Ace) 14]
        [(symbol=? card 'Black) 16] ;Note a space is left to insert 2 cards.
        [(symbol=? card 'Red) 17]))










;;Q3e
;;(straights hand) produces a list of straights 
;;Example
(check-expect (straights (list 3 3 4 4 4 5 5 7 7 7 8 8 8 8 9 9 9 9
                               10 10 10 'Jack 'Jack 'Jack' Queen 'Queen 'Ace 2 'Black))
              (list (list 7 8 9 10 'Jack)
                    (list 8 9 10 'Jack 'Queen) (list 7 8 9 10 'Jack 'Queen)))

(check-expect
 (straights (list 3 3 3 3 4 5 6 7 8 9
                  'Jack 'Jack 'Queen 'King 'Ace 2 2)) (list (list 3 4 5 6 7) (list 4 5 6 7 8) (list 5 6 7 8 9)
                                                            (list 3 4 5 6 7 8) (list 4 5 6 7 8 9) (list 3 4 5 6 7 8 9)))

(check-expect (straights (list 3 4 5 6 7 9 10 'Jack 'Queen 'King))
              (list (list 3 4 5 6 7) (list 9 10 'Jack 'Queen 'King)))
;;straights: Hand->listof Hand

(define minimum-for-straight 5)

(define (straights hand)
  (sort-hands (cond [(< (length (longest-substring (one-of-each (cutoff hand)))) minimum-for-straight) empty]       
                    [else (append (Num->Sym (sort-hands (possible-straights minimum-for-straight (longest-substring (one-of-each (cutoff hand))))))
                                  (straights (after-eliminating-substr (longest-substring (one-of-each (cutoff hand))) hand)))])))

;;(possible-straights substring) gives all the straights for a substring
;;Example
(check-expect (possible-straights minimum-for-straight (list 3 4 5 6 7 8))
              (list (list 3 4 5 6 7 8) (list 3 4 5 6 7) (list 4 5 6 7 8)))
;;possible-straights: listof Nat->listof Hand

(define (possible-straights minimum-length substring)  
  (cond  [(= (length substring) minimum-length) (list substring)]
         [else (append (all-straights-for-one minimum-length substring)
                       (possible-straights minimum-length (rest substring)))]))

;;(all-straights-for-one substring) finds all the straights for the first in the sequence
;;Example
(check-expect (all-straights-for-one minimum-for-straight (list 3 4 5 6 7 8)) (list (list 3 4 5 6 7 8) (list 3 4 5 6 7)))
;;all-straights-for-one: listof Nat->listof Hand

(define (all-straights-for-one minimum-length substring) 
  (cond [(< (length substring) minimum-length) empty]
        [else (cons substring (all-straights-for-one minimum-length (eliminate-last substring)))]))

;;(cutoff hand) eliminates all elements 2 'Black 'Red
;;Example
(check-expect (cutoff (list 6 9 'Jack 'King 2 'Black 'Red)) (list 6 9 'Jack 'King))
;;cutoff: Hand->Hand

(define (cutoff hand) 
  (cond [(empty? hand) empty]
        [(or (and (number? (first hand)) (= (first hand) 2))
             (and (symbol? (first hand)) (symbol=? (first hand) 'Black))
             (and (symbol? (first hand)) (symbol=? (first hand) 'Red)))
         (cutoff (rest hand))]
        [else (cons (first hand) (cutoff (rest hand)))]))

;;(eliminate-last list) eliminates the last element of the list
;;Example
(check-expect (eliminate-last (list 3 4 5 6 7 8)) (list 3 4 5 6 7))
;;eliminate-last: listof Any->listof Any

(define (eliminate-last list)  
  (cond [(empty? (rest list)) empty]
        [else (cons (first list) (eliminate-last (rest list)))]))
        
;;(after-eliminating-substr substring hand) eliminates the substring from the original hand
;;Example
(check-expect (after-eliminating-substr (list 4 5 6 7 8) (list 4 5 6 7 8 10 'Jack 'Queen 'King 'Ace))
              (list 10 'Jack 'Queen 'King 'Ace))
;;after-eliminating-substr: (listof Nat) Hand->Hand

(define (after-eliminating-substr substring hand) 
  (cond [(empty? hand) empty]  
        [(member? (first hand) substring) (after-eliminating-substr substring (rest hand))]
        [else (cons (first hand) (after-eliminating-substr substring (rest hand)))]))



;;(longest-substring hand) identifies the longest substring in a hand
;;Example
(check-expect (longest-substring (list 4 5 6 6 8 9 10 'Jack 'Queen 'King)) (list 8 9 10 11 12 13))
;;longest-substring: Hand->Hand

(define (longest-substring hand)
  (longest-seq-substr (Sym->Num (one-of-each hand))))


;;(one-of-each hand) makes it so there is only one occurence for each card in the hand
(check-expect (one-of-each (list 3 4 4 6 7 7 7 10 'Jack 'Jack 'Jack 'King 'King 'Ace))
              (list 3 4 6 7 10 'Jack 'King 'Ace))
(check-expect (one-of-each (list 8 8 9 'King 'King 'Ace 'Black))
              (list 8 9 'King 'Ace 'Black))
;;one-of-each: Hand->Hand

(define (one-of-each hand) 
  (cond [(empty? hand) empty]
        [(not (member? (first hand) (rest hand))) (cons (first hand) (one-of-each (rest hand)))]
        [else (one-of-each (rest hand))]))


(define (longest-seq-substr lon) ;help find longest sequence taken directly from tutorial 5
  (cond [(empty? lon) empty]
        [(cons? lon) (longest-list (seq-prefix lon)
                                   (longest-seq-substr (rest lon)))]))

(define (seq-prefix lon)  ;help find longest sequence taken directly from tutorial 5
  (cond [(empty? lon) empty]
        [(empty? (rest lon)) lon]
        [(= (add1 (first lon)) (second lon)) (cons (first lon) (seq-prefix (rest lon)))]
        [else (list (first lon))]))

(define (longest-list lst1 lst2) ;taken directly from tutorial 5
  (cond [(> (length lst1) (length lst2)) lst1]
        [else lst2]))


;;(Num->Sym list-of-lists) Changes cards from there number value to symbol
;;Example
(check-expect (Num->Sym (list (list 5 15 16 12 11) (list 5 12 13))) (list (list 5 2 'Black 'Queen 'Jack) (list 5 'Queen 'King)))
;;Num->Sym: listof (listsof Nat)-> listof Hand

(check-expect (Num->Sym (list (list 3 3) (list 4 4) (list 9 9) (list 11 11) (list 12 12)))
              (list (list 3 3) (list 4 4) (list 9 9) (list 'Jack 'Jack) (list 'Queen 'Queen)))

(define (Num->Sym list-of-lists)  
  (cond [(empty? list-of-lists) empty]
        [else (cons (Num->Sym-more (first list-of-lists))
                    (Num->Sym (rest list-of-lists)))]))

;;(Num->Sym-more list) executes for Num->Sym inside the sublists
;;Example
(check-expect (Num->Sym-more (list 4 12 13)) (list 4 'Queen 'King))
;;Num->Sym-more: listof Nat-> listof Any
;;Requires Nat to be >=2 and <=17.

(define (Num->Sym-more list)
  (cond [(empty? list) empty]
        [else (cons (value->card (first list)) (Num->Sym-more (rest list)))]))


;;(value->card card-as-num) gives a value a card symbol
;;Example
(check-expect (value->card 11) 'Jack)
;;value->card: Nat->Any
;;Requires Nat to be >=2 and <=17. 

(define (value->card card-as-num)
  (cond [(and (>= card-as-num 3) (<= card-as-num 10)) card-as-num]
        [(= card-as-num 11) 'Jack]
        [(= card-as-num 12) 'Queen]
        [(= card-as-num 13) 'King]
        [(= card-as-num 14) 'Ace]
        [(= card-as-num 15) 2]
        [(= card-as-num 16) 'Black]
        [(= card-as-num 17) 'Red]))


;;Q3f
;;(straight-pairs hand) creates of list of straights in pairs  
              
(check-expect (straight-pairs (list 3 3 3 4 5 5 5 6 6 6 6 7 7 7 8 8 9 'Ace 'Ace))
              (list (list 5 5 6 6 7 7) (list 6 6 7 7 8 8) (list 5 5 6 6 7 7 8 8)))
(check-expect (straight-pairs (list 3 3 4 4 5 5 7 10 10 'Jack 'Jack' Queen 'Queen 'King 'King))
              (list (list 3 3 4 4 5 5) (list 10 10 'Jack 'Jack 'Queen 'Queen)
                    (list 'Jack 'Jack 'Queen 'Queen 'King 'King)
                    (list 10 10 'Jack 'Jack 'Queen 'Queen 'King 'King)))
;;straight-pairs: Hand -> (listof Hand)

(define (straight-pairs hand)
  (sort-hands (cond [(< (length (longest-substring (one-of-each-fg (pairs (cutoff hand))))) minimum-for-straight-pair) empty]
                    [else (append (Num->Sym (add-one-of-each 
                                             (possible-straights minimum-for-straight-pair (longest-substring
                                                                                            (one-of-each-fg (pairs hand))))))
                                  (straight-pairs (after-eliminating-substr
                                                   (longest-substring (one-of-each-fg (pairs (cutoff hand)))) (Sym->Num hand))))])))
                

(define (add-one-of-each hand)
  (cond [(empty? hand) empty]
        [else (cons (add-one-of-each-helper (first hand)) (add-one-of-each (rest hand)))]))

(define (add-one-of-each-helper list) ;adds another element of each element in the list
  (cond [(empty? list) empty]
        [else (cons (first list) (cons (first list) (add-one-of-each-helper (rest list))))]))

(check-expect (add-one-of-each (list (list 4 5 6 7) (list 4 5 6 7 8)))
              (list (list 4 4 5 5 6 6 7 7) (list 4 4 5 5 6 6 7 7 8 8)))

                               

(define (one-of-each-fg hand) 
  (cond [(empty? hand) empty]
        [else (cons (first (first hand)) (one-of-each-fg (rest hand)))]))

(check-expect (one-of-each-fg (list (list 4 4) (list 6 6) (list 'King 'King)))
              (list 4 6 'King))
                             
(define minimum-for-straight-pair 3)


;;Q3g
;;(airplanes hand) produces a list of all airplanes
;;Examples
(check-expect (airplanes
               (list 3 3 4 4 4 5 5 7 7 7 8 8 8 8 9 9 9 9 10
                     'Jack 'Jack 'Jack' Queen 'Queen 'King 'Ace 2 'Black))
              (list (list 7 7 7 8 8 8) (list 8 8 8 9 9 9) (list 7 7 7 8 8 8 9 9 9)))

(check-expect (airplanes (list 4 4 4 5 5 5 8 8 
                               'Jack 'Jack 'Jack 'Queen 'Queen 'Queen 'King))
              (list (list 4 4 4 5 5 5) (list 'Jack 'Jack 'Jack 'Queen 'Queen 'Queen)))
;;airplanes: hand->listof Hand

(define minimum-for-airplane 2)



(define (airplanes hand)
  (sort-hands (cond [(< (length (longest-substring (one-of-each-fg (trios (cutoff hand))))) minimum-for-airplane) empty]
                    [else (append (Num->Sym (add-two-of-each 
                                             (possible-straights minimum-for-airplane (longest-substring
                                                                                       (one-of-each-fg (trios hand))))))
                                  (airplanes (after-eliminating-substr
                                              (longest-substring (one-of-each-fg (trios (cutoff hand)))) (Sym->Num hand))))])))
  



(define (add-two-of-each hand)
  (cond [(empty? hand) empty]
        [else (cons (add-two-of-each-helper (first hand)) (add-two-of-each (rest hand)))]))

(check-expect (add-two-of-each (list (list 4 5 6)))
              (list (list 4 4 4 5 5 5 6 6 6)))

(define (add-two-of-each-helper hand)
  (cond [(empty? hand) empty]
        [else (cons (first hand) (cons (first hand) (cons (first hand)
                                                          (add-two-of-each-helper (rest hand)))))]))

(check-expect (add-two-of-each-helper (list 4 5 6))
              (list 4 4 4 5 5 5 6 6 6))


