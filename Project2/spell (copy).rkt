; *********************************************
; *  314 Principles of Programming Languages  *
; *  Spring 2018                              *
; *  Student Version                          *
; *********************************************
#lang racket
;; contains "ctv", "A", and "reduce" definitions
(require "include.rkt")

;; contains dictionary definition
;(require "test-dictionary.rkt")
(require "dictionary.rkt")



;; -----------------------------------------------------
;; HELPER FUNCTIONS

;; *** CODE FOR ANY HELPER FUNCTION GOES HERE ***

;----reverses input string list so we can get the correct key----
(define (reverse l)
  (reversehelp l '()))

(define (reversehelp l newl)
  (if (null? l)
      newl
      (reversehelp (cdr l) (cons (car l) newl))))

;;-------------------------------------------------------

;;(1)takes a list of procedures and a word and returns a list of hash values for that specific word using different hash functions (hash-1, hash2, hash-3, etc)
(define (makelist hashlist term) ;;given a word, puts all its hash values into list
  (if (null? hashlist)
      '()
      (cons  ((car hashlist) term)  (makelist (cdr hashlist) term) ) ;;(hash-1 '(h e l l o))    ((car hashlist) term)
      
  ))

;;(2)takes a hash function and a dictionary, returns the list of the hashed dictionary values
(define (makedicthashvalues hashfunction dict) ;;takes a hash function and makes a list from it using all the dict words
  (if (null? dict)
     '()
     (cons (hashfunction (car dict)) (makedicthashvalues hashfunction (cdr dict) ))
     
   )
 )


;;(3)returns #t if in value in list, otherwise returns #f if it reaches end of list and still finds no matching value in list
(define (ismember? value list) ;;checks if a value is in a list (using "=")  ie. value = 674,   list = '(674 404 302.7)   should evaluate to #t
  (if (empty? list)
      0
      (if (= value (car list))
          1
          (ismember? value (cdr list))
  
  )))

;;(4)goal of checkdict is to either return #t or #f
(define (checkdict hashfunction hashvalue dict) ;;check if the hashvalue match a value in dict  ;; NOTE: we must make a dict hashvalues for the appropriate hash function
  (letrec (      (dictlist (makedicthashvalues hashfunction dict))      )  ;; dictlist is the list of dict hashvalues      ie. dictlist = (674 404 302.0),   hashvalue = 674
    ;;(if (= hashvalue (car dictlist) )  ;; "=" must be used with numbers because we can either get 17 or 17.0
    ;;    #t ;; or 0
    ;;    )
    (ismember? hashvalue dictlist)
    ;dictlist
    )
  )

;;(4)should return a vector like '(1 0 1 1), '(0 0 0 1 0 1 0), '(1 0), etc.
(define (makevector hashfunctionlist hashvalues dict)  ;;creates a bitvector representation ie. (1 0 1) //or in this case, (#t #f #t)
  ;;hashfunctionlist = (<procedure h1> <procedure h2>,etc)  ;;hashvalues = (674 552.0) ;;dict = ( '(word) '(word2) '(word3) )
  (if (null? hashvalues)
      '()
      (cons    (checkdict (car hashfunctionlist) (car hashvalues) dict)          (makevector (cdr hashfunctionlist) (cdr hashvalues) dict)   )
      )
  )


;;(5)list at this point should be something like '(1 0 1), '(0 0 1), '(1 0 0)
;;given '(0 3), what does this return?  ie. vector = '(0 3), returns #f
;;this function evaluates whether or not this could have false positives and returns #f if there is a chance for false positives
(define (evaluate vector)  ;;we know that if given (1 0 1), there was a situation where the word wasn't found in dictionary, thus it could be a false positive, return #f if not all 1's.
  (if (empty? vector)
      '(#t)
      (if (= 0 (car vector))
          '(#f)
          (evaluate (cdr vector))
  )))


(define index 0)
;;(6)given a vector, should be able to generate the bitvector from that,    ; ie. given (tobitvector '(1 0 0 1 0 0) index), returns '(0 3)
(define (tobitvector vector index)  ;;assume vector is something like '(1 0 0 1 0 0), should return '(0 3)
  (letrec ( (value 0))
    (if (null? vector)
      '()
      (if (= 1 (car vector))
          (cons index (tobitvector (cdr vector) (+ 1 index)))
          (tobitvector (cdr vector) (+ 1 index)) 
          
  ))))

;;(7)used for reduce function, both values must be true in order to return true
(define (vAnd x y)
  (and x y))



;;-------other code below------------



;; -----------------------------------------------------
;; KEY FUNCTION

;*******you get the correct key by using: "(key (reverse '(d a y)))" ************
(define key
  (lambda (w)
    ;'SOME_CODE_GOES_HERE ;; *** FUNCTION BODY IS MISSING ***
      (if(null? w)
         0
         (+    (* 31 (key(cdr w)) )  (ctv (car w))   )
      )
))


;; -----------------------------------------------------
;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size) ;; range of values: 0..size-1
    (lambda (k)
     ;'SOME_CODE_GOES_HERE ;; *** FUNCTION BODY IS MISSING ***
     ;(key (reverse k))
      (letrec (  (x (key (reverse k)))   (m (/ x size) )  (n (* size (floor m)))    )
        ;x = 3900; m = 5.56348074; n = (floor of m) * size   ;;I just want to get 5 from m
        ;;FIGURE OUT HOW TO GET THE FLOOR OF A NUMBER----(floor m)
        ;(- x n)
        (modulo x size)
      )
)))

;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;;       format, e.g., 17.0 for 17

(define gen-hash-multiplication-method
  (lambda (size) ;; range of values: 0..size-1
    (lambda (k)
     ;'SOME_CODE_GOES_HERE ;; *** FUNCTION BODY IS MISSING ***
      (letrec (   (x (key (reverse k)))   (A 0.7071067812)  (t (* x A))  (f (floor (* x A)))  (r (- t f))  )
        (floor (* size r))
        
      )
)))

;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 129971))
(define hash-2 (gen-hash-division-method 150001))
(define hash-3 (gen-hash-multiplication-method 700000))
(define hash-4 (gen-hash-multiplication-method 900000))
;(define hash-1 (gen-hash-division-method 701))
;(define hash-2 (gen-hash-division-method 899))
;(define hash-3 (gen-hash-multiplication-method 700))
;(define hash-4 (gen-hash-multiplication-method 900))


(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))


;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test your hash function implementation
;;
;;  (hash-1 '(h e l l o)) ==> 674
;;  (hash-1 '(d a y))     ==> 395
;;  (hash-1 '(c l a s s)) ==> 360
;;
;;  (hash-2 '(h e l l o)) ==> 139
;;  (hash-2 '(d a y))     ==> 304
;;  (hash-2 '(c l a s s)) ==> 205
;;
;;  (hash-3 '(h e l l o)) ==> 552.0
;;  (hash-3 '(d a y))     ==> 501.0
;;  (hash-3 '(c l a s s)) ==> 247.0
;;
;;  (hash-4 '(h e l l o)) ==> 710.0
;;  (hash-4 '(d a y))     ==> 644.0
;;  (hash-4 '(c l a s s)) ==> 317.0


;; -----------------------------------------------------
;; SPELL CHECKER GENERATOR

(define gen-checker
  (lambda (hashfunctionlist dict)
     ;'SOME_CODE_GOES_HERE ;; *** FUNCTION BODY IS MISSING ***
    (lambda (term) ;; '(h e l l o)
      (letrec( (hlist (makelist hashfunctionlist term))    ) ;;(vector (checkdict hlist dict))
        
        (reduce vAnd (evaluate (makevector hashfunctionlist hlist dict)) #t) ;;  hlist = '(647 552.0) ;; dict = ('(h e l l o) '(w o r l d) '(s c h e m e)) ;;returns a list (#t #t) in the end.
        ;(makevector hashfunctionlist hlist dict)
        ;(tobitvector (makevector hashfunctionlist hlist dict) index)
        ;(tobitvector '(1 0 0 1 0 1 0 0 1 0) index)
        ;hlist
               
      )
)))


;; -----------------------------------------------------
;; EXAMPLE SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))

;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;
;;  (checker-1 '(a r g g g)) ==> #f
;;  (checker-2 '(h e l l o)) ==> #t
;;  (checker-2 '(d a y)) ==> #f
;;  (checker-2 '(c l a s s)) ==> #f

