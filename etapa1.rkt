#lang racket
(require "suffix-tree.rkt")

(provide (all-defined-out))

; TODO 2
; Implementați o funcție care primește două cuvinte (liste
; de caractere) w1 și w2 și calculează cel mai lung prefix
; comun al acestora, împreună cu restul celor două cuvinte
; după eliminarea prefixului comun.
; ex:
; (longest-common-prefix '(#\w #\h #\y) '(#\w #\h #\e #\n))
; => '((#\w #\h) (#\y) (#\e #\n))
; Folosiți recursivitate pe coadă.
(define (longest-common-prefix w1 w2)
  (append (list (longest-pref-rec w1 w2 '()))
          (list (remove-pref w1 (longest-pref-rec w1 w2 '())))
          (list (remove-pref w2 (longest-pref-rec w1 w2 '())))
          )
  )

;; determina cel mai lung prefix comun pt 2 cuvinte
(define (longest-pref-rec w1 w2 pref)
  (cond
    ((null? w1) pref)
    ((null? w2) pref)
    ((char=?  (car w1) (car w2)) (longest-pref-rec (cdr w1) (cdr w2) (append pref (list (car w1)))))
    (else pref)
   )
  )

; elimina prefixul dat ca parametru dintr-un cuvant
(define (remove-pref w pref)
  (cond
   ((null? pref) w)
   ((char=? (car w) (car pref)) (remove-pref (cdr w) (cdr pref)))
   (else w)
    )
  )


; TODO 3
; Implementați recursiv o funcție care primește o listă nevidă 
; de cuvinte care încep cu același caracter și calculează cel 
; mai lung prefix comun al acestora.
; Opriți căutarea (parcurgerea) în momentul în care aveți garanția 
; că prefixul comun curent este prefixul comun final.
(define (longest-common-prefix-of-list words)
  (longest-pref-list-rec (cdr words) (car words))
  )

(define (longest-pref-list-rec words pref)
  (cond
    ((null? words) pref)
    (else (longest-pref-list-rec (cdr words) (longest-pref-rec (car words) pref '()) ))
   )
  )

;; Următoarele două funcții sunt utile căutării unui șablon
;; (pattern) într-un text cu ajutorul arborelui de sufixe.
;; Ideea de căutare este următoarea:
;; - dacă șablonul există în text, atunci există un sufix care
;;   începe cu acest șablon, deci există o cale care începe din
;;   rădăcina arborelui care se potrivește cu șablonul
;; - vom căuta ramura a cărei etichetă începe cu prima literă
;;   din șablon
;; - dacă nu găsim această ramură, șablonul nu apare în text
;; - dacă șablonul este conținut integral în eticheta ramurii,
;;   atunci el apare în text
;; - dacă șablonul se potrivește cu eticheta dar nu este conținut
;;   în ea (de exemplu șablonul "nana$" se potrivește cu eticheta
;;   "na"), atunci continuăm căutarea în subarborele ramurii
;; - dacă șablonul nu se potrivește cu eticheta (de exemplu
;;   șablonul "numai" nu se potrivește cu eticheta "na"), atunci
;;   el nu apare în text (altfel, eticheta ar fi fost "n", nu
;;   "na", pentru că eticheta este cel mai lung prefix comun al
;;   sufixelor din subarborele său)


; TODO 4
; Implementați funcția match-pattern-with-label care primește un
; arbore de sufixe și un șablon nevid și realizează un singur pas 
; din procesul prezentat mai sus - identifică ramura arborelui a
; cărei etichetă începe cu prima literă din șablon, apoi
; determină cât de bine se potrivește șablonul cu eticheta,
; întorcând ca rezultat:
; - true, dacă șablonul este conținut integral în etichetă
; - lista (etichetă, nou pattern, subarbore), dacă șablonul se
;   potrivește cu eticheta dar nu este conținut în ea
;   (ex: ("na", "na$", subarborele de sub eticheta "na")
;   pentru șablonul inițial "nana$" și eticheta "na")
; - lista (false, cel mai lung prefix comun între etichetă și
;   șablon), dacă șablonul nu s-a potrivit cu eticheta sau nu
;   s-a găsit din start o etichetă care începe cu litera dorită
;   (ex1: (false, "n") pentru șablonul "numai" și eticheta "na")
;   (ex2: (false, "") pentru etichetă negăsită)
; Obs: deși exemplele folosesc stringuri pentru claritate, vă
; reamintim că în realitate lucrăm cu liste de caractere.
(define (match-pattern-with-label st pattern)
 (define br (get-ch-branch st (car pattern)))
  (if (list? br)
    (verif-pattern-in-branch br pattern)
    (cons #f (list '()))
      )
  )
 
(define (verif-pattern-in-branch branch pattern)
 (cond
   ((null? branch) (append (list #f) (list '())))
   (else (help-func branch pattern))))


 (define (help-func branch pattern) 
 (define pref (longest-pref-rec (car branch) pattern '())) 
  (cond
    ((equal? pattern pref) #t)
    ((equal?  pref (car branch)) (append (list pref) (list (remove-pref pattern pref)) (list (get-branch-subtree branch))))     
    (else (cons #f (list  (longest-pref-rec (car branch) pattern '())))))
  )

; TODO 5
; Implementați funcția st-has-pattern? care primește un
; arbore de sufixe și un șablon și întoarce true dacă șablonul
; apare în arbore, respectiv false în caz contrar.
(define (st-has-pattern? st pattern)
  (define br (get-ch-branch st (car pattern)))
  (verif-pattern-st st br pattern)
  )

(define (verif-pattern-st st br pattern)
  (cond
    ((null? br) #f)
    (else (veriff-pattern-if-br-!null br pattern st))
  ))

(define (veriff-pattern-if-br-!null br pattern st)
         
  (define res (verif-pattern-in-branch br pattern))
  (cond
    ((equal? res #t) #t)
    ((equal? (car res) #f) #f)
    (else (st-has-pattern? (other-branches st) (cadr res)))
    ))                             

