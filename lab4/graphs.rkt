#lang racket
; ignorați următoarele linii de cod.
(define show-defaults 2) ; câte exerciții la care s-au întors rezultate default să fie arătate detaliat
(define prepend #f) (define name-ex '(testul . testele)) ; variante: '(exercițiul . exercițiile) sau '(testul . testele) sau '(task . taskurile)
(define default-results '(your-code-here ())) ; ce rezultate default sunt întoarse în exerciții
(define : 'separator) (define punct 'string) (define puncte 'string) (define BONUS 'string) (define total 0) (define all '()) (define n-ex 0) (define p-ex 0) (define defaults '())
(define (ex n sep p . s) (set! n-ex n) (set! p-ex p) (set! all (cons (list n p) all))) (define exercițiul ex) (define (p L) (map (λ (e) (display e) (display " ")) L) (newline))
(define (check-exp given expected) (check-exp-part "" 1 given expected)) (define (check-exp-part part percent given expected) (if (not (equal? given expected)) (and
                                                                                                                                                            (when (member given default-results) (set! defaults (cons (if (< percent 1) (cons n-ex part) n-ex) defaults))) (when (or (not (member given default-results))
                                                                                                                                                                                                                                                                                         (<= (length defaults) show-defaults)) (p `(NU: la ,(car name-ex) ,(if (< percent 1) (cons n-ex part) n-ex) rezultatul ,given diferă de cel așteptat: ,expected))))
                                                                                                                                  (let ((pts (* p-ex percent))) (and (if prepend (printf "+~v: " pts) (printf "OK: "))
                                                                                                                                                                     (p `(,(car name-ex) ,(if (< percent 1) (cons n-ex part) n-ex) trecut + ,pts ,(if (= pts 1) 'punct 'puncte))) (set! total (+ total pts))))))(define (sumar) (when (and (not (null? defaults)) (< show-defaults (length defaults))) (p `(... rezultatul implicit dat la ,(cdr name-ex) ,(reverse defaults)))) (p `(total: ,total puncte)))
(define (mark-helper) (printf "----------~nEx  puncte    Total până aici~n") (foldr (λ (e-p t) (p `(,(car e-p) : ,(cadr e-p) puncte. total 1 - ,(car e-p) : ,(+ t (cadr e-p)))) (+ t (cadr e-p))) 0 all))

;; Ne propunem să realizăm o aplicație care să certifice stăpânirea următoarelor concepte/competențe:
;; - abstractizarea la nivel de date (tipuri de date abstracte care oferă o interfață (constructori + operatori)
;;   utilizatorului, astfel încât acesta să nu fie nevoit să cunoască implementarea internă)
;; - abstractizarea la nivel de proces (abilitatea de a folosi funcționale)
;; - caracteristica funcțiilor de a fi valori de ordinul întâi
;; - abilitatea de a implementa funcțional un program mai mare 
;; - folosirea expresiilor de legare statică a variabilelor (let, let*, letrec, named let, etc.)
;;    - ATENȚIE: Nu duplicați calcule atunci când puteți evita asta cu o formă de let!
;;               Nu scrieți funcții care doar apelează alte funcții atunci când puteți folosi un named let!

;; Pentru aceasta, definim structura de date graf neorientat.

;; Reținem un graf ca pe o listă de noduri și o listă de muchii, unde o muchie este o listă (u v).
;; Muchiile se dau într-un singur sens, fiind valabile implicit și în sensul celălalt.
(define (make-graph V E) (cons V E))
(define G1 (make-graph
            '(1 2 3 4 6 5 7 8)
            '((1 4) (2 1) (3 4) (2 3) (3 5) (2 6) (5 2) (3 6) (8 7) (6 8) (7 6))))
(define G2 (make-graph
            '(1 2 3 4 5 6 7 8 9 10)
            '((1 2) (1 3) (1 5) (2 3) (2 4) (3 4) (3 5) (5 7) (5 10) (6 2) (6 4) (6 7)
                      (7 3) (7 4) (7 9) (7 10) (8 6) (8 9) (8 10) (9 6) (10 9))))

(exercițiul 1 : 4 puncte)
;; Completați tipul de date graf neorientat cu următorii operatori:

;; lista de noduri din graf (0,25p)
(define (get-nodes G)
  (car G))

(check-exp-part 'a (/ 1. 16) (get-nodes G2) '(1 2 3 4 5 6 7 8 9 10))

;; lista de muchii din graf (0,25p)
(define (get-edges G)
  (cdr G))

(check-exp-part 'b (/ 1. 16) (get-edges G1) '((1 4) (2 1) (3 4) (2 3) (3 5) (2 6) (5 2) (3 6) (8 7) (6 8) (7 6)))

;; lista sortată crescător a vecinilor nodului v (considerați că nodurile se dau ca numere naturale) (1.5p)
;; Se cere o implementare folosind funcționale și/sau funcții anonime, fără recursivitate.
;; Pentru sortare, folosiți funcția sort a limbajului.

(define (neighbours G v)
  (let ((edges (get-edges G))
        (f (lambda (e) (if (equal? (car e) v)
                      (car (cdr e))
                      (if (equal? (car (cdr e)) v)
                          (car e)
                          '() )))))
    (sort (filter number? (map f edges)) <)))

(check-exp-part 'c (/ 3. 16) (neighbours G1 2) '(1 3 5 6))
(check-exp-part 'd (/ 3. 16) (neighbours G2 6) '(2 4 7 8 9))

;; lista de adiacență a grafului, exprimată ca listă de liste (1p)
;; fiecare listă internă este de forma (nod vecin-1 vecin-2 ... vecin-n)
;; lista de adiacență este sortată după primul nod din listele interne
;; Se cere o implementare folosind funcționale și/sau funcții anonime, fără recursivitate.
(define (adj-list G)
  (let ((nodes (sort (get-nodes G) < ))
        (f (lambda (e) (cons e (neighbours G e)))))
    (map f nodes)))

(check-exp-part 'e (/ 1. 4) (adj-list G1) '((1 2 4) (2 1 3 5 6) (3 2 4 5 6) (4 1 3) (5 2 3) (6 2 3 7 8) (7 6 8) (8 6 7)))

;; testul dacă într-un graf toate nodurile au grad par (1p)
;; Se cere să folosiți andmap.
(define (even-degrees? G) 
  (let ((adjList (adj-list G))
        (f (lambda (e) (even? (length (cdr e))))))
    (andmap f adjList)))

(check-exp-part 'f (/ 1. 8) (even-degrees? G1) #t)
(check-exp-part 'g (/ 1. 8) (even-degrees? G2) #f)

(exercițiul 2 : 6 puncte)
;; o funcție care elimină dintr-o listă de adiacență muchia (u v) (3p)
;; funcția întoarce noua listă de adiacență dacă muchia există în listă, #f altfel
;; atenție, muchia (u v) este totuna cu muchia (v u)!
;; când un nod nu mai are vecini, el dispare cu totul din lista de adiacență!
;; Puteți folosi funcția remove.
;; Folosiți recursivitate, NU folosiți fold!
(define (remove-edge adj-list u v)
  (let (
        (f (lambda (list) (if (equal? (car list) u)
                         (remove v list)
                         (if (equal? (car list) v)
                             (remove u list)
                             list)))))
    (let ((new-adj-list (filter (lambda (list) (>= (length list) 2)) (map f adj-list))))
      (if (equal? adj-list new-adj-list)
          #f
          new-adj-list)))
  )

(check-exp-part 'a (/ 1. 48) (remove-edge '((1 2 4) (2 1 3 5 6) (3 2 4 5 6) (4 1 3) (5 2 3) (6 2 3 7 8) (7 6 8) (8 6 7)) 9 3) #f)
(check-exp-part 'b (/ 1. 48) (remove-edge '((1 2 4) (2 1 3 5 6) (3 2 4 5 6) (4 1 3) (5 2 3) (6 2 3 7 8) (7 6 8) (8 6 7)) 7 3) #f)
(check-exp-part 'c (/ 11. 48) (remove-edge '((1 2 4) (2 1 3 5 6) (3 2 4 5 6) (4 1 3) (5 2 3) (6 2 3 7 8) (7 6 8) (8 6 7)) 2 3)
                '((1 2 4) (2 1 5 6) (3 4 5 6) (4 1 3) (5 2 3) (6 2 3 7 8) (7 6 8) (8 6 7)))
(check-exp-part 'd (/ 11. 48) (remove-edge '((1 2 4) (2 1 3 5 6) (3 2 4 6) (4 1 3) (5 2) (6 2 3 7 8) (7 6 8) (8 6 7)) 2 5)
                '((1 2 4) (2 1 3 6) (3 2 4 6) (4 1 3) (6 2 3 7 8) (7 6 8) (8 6 7)))

;; o funcție care pleacă de la un nod și o listă de adiacență și călătorește de-a lungul unor muchii
;; încă nevizitate (alese cum doriți) până se intoarce în nodul de start (creând un ciclu) (3p)
;; o muchie odată traversată este eliminată din lista de adiacență
;; traseul este reținut într-o listă de noduri, de exemplu (3 5 1 3)
;; Funcția întoarce o pereche dintre lista de adiacență actualizată și ciclul parcurs (traseul).
;; Se cere să folosiți named let, NU folosiți fold!
(define (get-cycle v adj-list)
  'your-code-here)

;; decomentați partea de mai jos pentru a testa
;; apoi lăsați-o decomentată pentru a testa exercițiul 3
;(define (valid-edges? cycle edges)
;  (or (null? (cdr cycle))
;      (let* ((u (first cycle))
;             (v (second cycle))
;             (uv (list u v))
;             (vu (list v u))
;             (edges (cond ((member uv edges) (remove uv edges))
;                          ((member vu edges) (remove vu edges))
;                          (else #f))))
;        (and edges (valid-edges? (cdr cycle) edges)))))
;
;(define (valid-cycle? pair v adj G)
;  (let* ((edges (get-edges G)) (new-adj (car pair)) (cycle (cdr pair)) (cycle-edges (map list (drop-right cycle 1) (cdr cycle))))
;    (and (equal? (first cycle) (last cycle))
;         (valid-edges? cycle edges)
;         (equal? new-adj (foldr (lambda (x seed) (if seed (apply remove-edge seed x) #f)) adj cycle-edges)))))

;(check-exp-part 'e (/ 1. 2) (valid-cycle? (get-cycle 1 '((1 2 4) (2 1 3 5 6) (3 2 4 5 6) (4 1 3) (5 2 3) (6 2 3 7 8) (7 6 8) (8 6 7)))
;                                          1 '((1 2 4) (2 1 3 5 6) (3 2 4 5 6) (4 1 3) (5 2 3) (6 2 3 7 8) (7 6 8) (8 6 7)) G1) #t)

;; BONUS
(exercițiul 3 : 5 puncte)

;; ciclul obținut prin inserarea unui ciclu în alt ciclu astfel încât rezultatul să fie un ciclu (2p)
;; exemplu: (2 4 5 2) inserat în (1 4 3 2 1) devine (1 4 3  2 4 5 2  1)
;; NU trebuie să verificați validitatea datelor de intrare, trebuie doar să inserați.
;; Se cere să folosiți splitf-at și let-values.
(define (insert-cycle c-new c)
  'your-code-here)

(check-exp-part 'a (/ 1. 5) (insert-cycle '(2 5 3 6 2) '(1 4 3 2 1)) '(1 4 3 2 5 3 6 2 1))
(check-exp-part 'b (/ 1. 5) (insert-cycle '(2 4 3 2) '(2 5 3 6 2)) '(2 4 3 2 5 3 6 2))

;; o funcție care găsește un ciclu eulerian într-un graf conex, dacă există, altfel întoarce #f (3p)
;; un ciclu eulerian este un ciclu care parcurge fiecare muchie din graf fix o dată
;; un graf conex are un ciclu eulerian dacă și numai dacă toate nodurile au grad par
;; algoritm:
;; - pleacă dintr-un nod oarecare și, mergând doar pe muchii încă nevizitate, găsește un ciclu
;; - piiiioioujuu++eacă dintr-un nod din ciclul anterior și repetă operația de mai sus, apoi integrează al doilea ciclu în primul
;; - tot așa până am acoperit toate muchiile
;; Puteți folosi funcția findf.
;; Se cere să folosiți named let, NU folosiți fold!
(define (euler-cycle G)
  'your-code-here)

(check-exp-part 'c (/ 1. 20) (euler-cycle G2) #f)

;; decomentați partea de mai jos pentru partea b a testului
;(define (valid-euler? cycle G)
;  (let ((edges (get-edges G)))
;    (and (equal? (first cycle) (last cycle))
;         (equal? (length cycle) (add1 (length edges)))
;         (valid-edges? cycle edges))))
;
;(check-exp-part 'd (/ 11. 20) (valid-euler? (euler-cycle G1) G1) #t)

(sumar)
                       