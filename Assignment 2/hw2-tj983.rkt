;1st
; Comment: If length of both the list is different then return value would contains least length value. i.e (zip '(1 2 3) '(a b c)) would return
; ((1 a) (2 b))
(define (zip lis1 lis2)
      (cond
         ((null? lis1) lis1)
         ((null? lis2) lis2)
         (else (cons (list (car lis1) (car lis2))  (zip (cdr lis1) (cdr lis2))))))


;2nd
; If number of element is not same i.e. 2 then second list would have less element. 
(define unzip (lambda (list1)
      (cond
         ((null? list1) list1 )
         (else (cons (firstlementList list1)   (list (secondementList list1)))))))

;helper function to return list of first element of list
(define firstlementList (lambda (list1)
  (cond
    ((null? list1) list1 )
    (else   ( cons  (car (car list1)) (firstlementList (cdr list1)))))))

;helper function to return list of second element of list
(define secondementList  (lambda (list1)
  (cond
    ((null? list1) list1 )
    ((null? (cdr (car list1))) '()) 
    (else   ( cons   (car (cdr (car list1)) )  (secondementList (cdr list1)))))))

; 3rd
; Comment: If position is greater than length length of list1 then return value would be list1 +  list2. i.e (splice 3 '(H e r l d) '(l l o W o))
; would return (h e r l d l l o w o)
(define (splice position list1 list2)
  (cond
    ((null? list1) list2)
    ((null? list2) list1 )
    ((= 1 position) (merge list2 list1))
    (else  (cons (car list1) (splice (- position 1) (cdr list1) list2)))))

; helper function to merge list2 into list1
(define (merge list1 list2)
  (cond
    ((null? list1) list2)
    ((null? list2) list1)
    (else  (cons (car list1)  (merge (cdr list1) list2)))))

;4th
; If length of both lists are different mapfun would evaluate according to the minimum length size
(define (mapfun list1 list2 )
  (cond
    ((null? list1) list1)
    ((null? list2) list2)
    (else ( cons ((car list1) (car list2)) (mapfun (cdr list1) (cdr list2)) ))))

;5th
; List1 is list of function and x is input
(define (filter list1 x)
  (cond
    ((null? list1) list1)
    (else  (if (eq? ((car list1) x) #f) (filter (cdr list1) x ) (cons (car list1) (filter (cdr list1) x ))))))

;6th
; For negative value of N it would return an empty list
(define (comparatorFuns N)
  (cond
     ((> 0 N) '())
     ((= 0 N) '())
     (else (merge (comparatorFuns (- N 1)) (list (lambda (x) (> x N)))))))

;1st
(zip '(1 2 3) '(a b c ))
;2nd
(unzip '((1 a) (2 b) (3 c)))
;3rd
(splice 3 '(H e r l d) '(l l o W o))
;4th
(mapfun (list cadr car cdr) '((A B) (C D) (E F)))
;5th
(filter (list (lambda (Z) (equal? Z 'A)) (lambda (Z) (member Z '(D E F))) (lambda (Z) (member Z '(C B A)))) 'A )
;6th
(mapfun (comparatorFuns 4) '(20 10 0 -10))
(mapfun (filter (comparatorFuns 4) 3) '(10 1))