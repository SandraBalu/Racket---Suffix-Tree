#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".

(define (list->stream L)
  (if (null? L)
      empty-stream
      (stream-cons (car L) (list->stream (cdr L)))))

; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).]

(define (longest-common-prefix w1 w2)
  (append (list (longest-common-pref w1 w2 '()))
          (list (remove-pref w1 (longest-common-pref w1 w2 '())))
          (list (remove-pref w2 (longest-common-pref w1 w2 '())))
          )
  )

(define (remove-pref w pref)
  (cond
   ((null? pref) w)
   ((char=? (car w) (car pref)) (remove-pref (cdr w) (cdr pref)))
   (else w)
    )
  )


(define (longest-common-pref w1 w2 pref)
  (cond
    ((null? w1) pref)
    ((null? w2) pref)
    ((equal? (car w1) (car w2)) (longest-common-pref (cdr w1) (cdr w2) (append pref (list (car w1)))))
    (else pref)))


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection

(define (longest-common-prefix-of-collection words)
  (longest-pref-collection-rec   (collection-first words) (collection-rest words)))

(define (longest-pref-collection-rec pref words)
  (cond
    ((collection-empty? words) pref)
     (else (longest-pref-collection-rec  (longest-common-pref pref (collection-first words) '()) (collection-rest words)))
      ))

(define (match-pattern-with-label st pattern)
  (define br (get-ch-branch st (car pattern)))
   (if (not (null? br))
      (let* ((label (car br))
             (pref (longest-common-pref label pattern '())))
        (cond
          ((equal? label pattern) #t)
          ((equal? (length pref) (length pattern)) #t)
          ((< (length pref) (length (car br))) (append (list #f pref)))
          (else (append (list pref) (list (remove-pref pattern pref)) (list (cdr br))))))
      (list #f '())))


(define (st-has-pattern? st pattern)
 (let loop ((current-st st) (remaining-pattern pattern))
    (if (null? remaining-pattern)
        #t 
        (let ((match-result (match-pattern-with-label current-st remaining-pattern)))
          (cond
            ((boolean? match-result) match-result)
            ((equal? (car match-result) #f) #f)
             (else (loop (third match-result) (second match-result))))
          ))
   ))  


(define (get-suffixes text)
  (if (collection-empty? text)
      '()
      (collection-cons text (get-suffixes (cdr text)))
      )
  )


(define (get-ch-words words ch)
    (collection-filter (lambda (word) (and (not (null? word)) (starts-with? word ch))) words))


(define (starts-with? word ch)
  (if (char=? (car word) ch) #t #f))


(define (ast-func suffixes)
  (define first-letter (collection-first (collection-first suffixes)))
  (cons (list first-letter) (collection-map (lambda (suff) (collection-rest suff)) suffixes)))

(define (cst-func suffixes)
  (define pref (longest-common-prefix-of-collection suffixes))
  (cons pref (collection-map (lambda (suff) (drop suff (length pref))) suffixes)))


; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)



(define (suffixes->st labeling-func suffixes alphabet)
  
  (define (filter-false lst) (collection-filter (lambda (x) (not (eq? x #f))) lst))
 
  (let ((list-suffixes
         (collection-map (lambda (ch)
                (let ((suff-ch (get-ch-words suffixes ch)))
                  (if (collection-empty? suff-ch) #f
                        (if ((compose not null?) (cdr (labeling-func suff-ch)))
                            (cons (car (labeling-func suff-ch)) (suffixes->st labeling-func (cdr (labeling-func suff-ch)) alphabet))
                            (car (labeling-func suff-ch))))))
              alphabet)))
    (filter-false list-suffixes)))


; nu uitați să convertiți alfabetul într-un flux
(define text->st
  (lambda (label-func)
    (lambda (text)
      (define text-with-marker (append text '(#\$)))
      (define alphabet (sort (remove-duplicates text-with-marker) char<?))
      (define st (suffixes->st label-func (get-suffixes text-with-marker) (list->stream alphabet)))
        st)))


(define text->ast 
  (text->st ast-func))


(define text->cst 
  (text->st cst-func))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
(define (substring? text pattern)
   (define st (text->cst text))
  (st-has-pattern? st pattern))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  (let loop ((st (text->cst text)) (need-len len) (result '()))
    (cond ((st-empty? st) #f)
          ((<= need-len 0) (take result len))
          (else
           (let* ((branch (first-branch st)) (label (get-branch-label branch)) (subtree (get-branch-subtree branch)))
             (or (loop subtree (- need-len (length label)) (append result label))
                 (loop (other-branches st) need-len result)))))))