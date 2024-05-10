#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


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
   (cond
    ((null? text) #f) 
    ((equal? pattern (longest-pref-rec text pattern '())) #t)
    (else (substring? (cdr text) pattern))
    ) )
   

; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).
(define (longest-common-substring text1 text2)
  (define suffixes (get-suffixes text2))
  
  (define (find-longest-match suffix)
  (let* ((st1 (text->cst text1))
         )
    (let loop ((st st1)
               (suffix suffix)
               (match '()))
      (let ((pattern-result (match-pattern-func st suffix)))
      (cond
        ;;verificam daca tot sufixul se afla in branchul la care lucram
        ((equal? text1 suffix) suffix)
        ((equal? pattern-result #t) (append suffix))
        ;;returnam doar prefixul comun, sufixul nu se portiveste cu branchul
        ((equal? (car pattern-result) #f) (append match (list-ref pattern-result 1)))
        ;;branchul se potriveste cu cu sfixul, insa mai raman litere neverificate din sufix
        (else (loop (cdr (get-ch-branch st (car suffix))) (list-ref pattern-result 1) (append match (car pattern-result)))) 
       )
      ))))

  
  ;; Parcurgem sufixele din al doilea text și găsim cel mai lung subșir comun
  (define (find-longest-common-suffix)
    (define max-length 0)
    (define result '())
    ;;pt fiecare suffix aflam cel mai lung subsir comun si daca este mai lung ca cel de dinainte actualizam maximul
    (for-each
     (lambda (suffix)
       (let ((match (find-longest-match suffix)))
         (if (> (length match) max-length)
             (begin
               (set! max-length (length match))
               (set! result match))
         '())))
     suffixes)
    result)
  
  (find-longest-common-suffix))
                 

(define (match-pattern-func st pattern)
  (define br (get-ch-branch st (car pattern)))
   (if (not (null? br))
      (let* ((label (car br))
             (pref (common-prefix label pattern)))
        (cond
          ((equal? label pattern) #t)
          ((equal? (length pref) (length pattern)) #t)
          ((< (length pref) (length (car br))) (append (list #f pref)))
          (else (append (list pref) (list (remove-pref pattern pref)) (list (cdr br))))))
      (list #f '())))        
  

(define (common-prefix str1 str2)
    (let loop ((s1 str1) (s2 str2) (prefix '()))
      (cond
        ((or (null? s1) (null? s2)) (reverse prefix))
        ((char=? (car s1) (car s2)) (loop (cdr s1) (cdr s2) (cons (car s1) prefix)))
        (else (reverse prefix)))))



; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.
(define (repeated-substring-of-given-length text len)
  'your-code-here)

