;;; Rendu TP1 :

;   Exercice 1.1

35 ; atome de type nombre
'(35) ; liste contenant un atome de type nombre et de profondeur 0
'(((3) 5) 6) ; liste contenant 3 elements de type nombre et de profondeur 2
'-34RRRR ; symbole
T ; symbole, l'une des deux constantes de LISP
NIL ; symbole, l'autre constantes de LISP
'() ; liste vide evaluee a NIL



;   Exercice 1.2

'( ((A) (B) (C)) G (((((D)) F ) H)))
; Fait sur le rapport pdf



;   Exercice 1.3

(CADR (CDR (CDR (CDR '(DO RE MI FA SOL LA SI)))))

; > sol

; Retourne le 5eme element
; etape 1 : CDR <===> reste : RE MI FA SOL LA SI
; etape 2 : CDR <===> reste : MI FA SOL LA SI
; etape 3 : CDR <===> reste : FA SOL LA SI
; etape 4 : CADR <===> (CAR (CDR '(FA SOL LA SI))))
; etape 4.2 : CDR <===> reste : SOL LA SI
; etape 4.2 : CAR <===> reste : SOL



(CONS (CADR '((A B)(C D))) (CDDR '(A (B (C)))))

; > ((C D))

; Retourne une liste
; etape 1 : (CADR '((A B)(C D))) <===> reste : (C D)
; etape 2 : (CDDR '(A (B (C)))) <===> reste : NIL
; etape 3 :  (CONS '(CD) NIL) <===> retourne ((C D))



(CONS (CONS 'HELLO NIL) '(HOW ARE YOU))

; > ((HELLO) HOW ARE YOU)

; Retourne une liste
; etape 1 : (CONS 'HELLO NIL) <===> (HELLO)
; etape 2 : (CONS '(HELLO) '(HOW ARE YOU)) <===> ((HELLO) HOW ARE YOU)



(CONS 'JE (CONS 'JE (CONS 'JE (CONS 'BALBUTIE NIL))))

; >(JE JE JE BALBUTIE)

; Retourne une liste
; etape 1 : (CONS 'BALBUTIE NIL) <===> (BALBUTIE)
; etape 2 : (CONS 'JE '(BALBUTIE)) <===> (JE BALBUTIE)
; etape 3 : (CONS 'JE '(JE BALBUTIE)) <===> (JE JE BALBUTIE)
; etape 4 : (CONS 'JE '(JE JE BALBUTIE)) <===> (JE JE JE BALBUTIE)



(CADR (CONS 'TIENS (CONS '(C EST SIMPLE) ())))

; > (C EST SIMPLE)

; Retourne une liste
; etape 1 : (CONS '(C EST SIMPLE) ()) <===>  (CONS '(C EST SIMPLE) NIL) <===> ((C EST SIMPLE))
; etape 2 : (CONS 'TIENS '((C EST SIMPLE))) <===> (TIENS (C EST SIMPLE))
; etape 3 : (CADR '(TIENS (C EST SIMPLE))) <===> (CAR (CDR '(TIENS (C EST SIMPLE))))
; etape 3.1 : (CDR '(TIENS (C EST SIMPLE))) <===> ((C EST SIMPLE))
; etape 3.2 : (CAR '((C EST SIMPLE))) <===> (C EST SIMPLE)



;   Exercice 1.4

(defun nombre3 (l)
  ;;; retourne BRAVO si les 3 premiers elements de la liste L sont des nombres
  ;;; retourne Perdu sinon (n'a pas un nombre pour ses 3 permiers elements ou tout autre cas d'echec : pas une liste, liste de taille inferieure a 3)

  (if (AND (listp l) (>= (length l) 3) (numberp (car l)) (numberp (cadr l)) (numberp (caddr l)))
       'BRAVO
       'PERDU
  )
)

(nombre3 '(1 2 3 4)) ; test de reussite
(nombre3 '(1 2 Faux 4)) ; test d'echec 1 : n'a pas ses trois premiers elements sous forme de nombre
(nombre3 '(5)) ; test d'echec 2 : une liste de taille 1
(nombre3 5); test d'echec 3 : un atome



(defun nombre3 (l)
  ;;; retourne BRAVO si les 3 premiers elements de la liste L sont des nombres
  ;;; retourne Perdu sinon (n'a pas un nombre pour ses 3 permiers elements ou tout autre cas d'echec : pas une liste, liste de taille inferieure a 3)

  (if (AND (listp l) (numberp (car l)) (numberp (cadr l)) (numberp (caddr l)))
       'BRAVO
       'PERDU
  )
)

(nombre3 '(1 2 3 4)) ; test de reussite
(nombre3 '(1 2 Faux 4)) ; test d'echec 1 : n'a pas ses trois premiers elements sous forme de nombre
(nombre3 '(5)) ; test d'echec 2 : une liste de taille 1
(nombre3 5); test d'echec 3 : un atome





(defun grouper (L1 L2)

  ;;; Si les deux listes sont non vides on ajoute dans la liste de retour
  ;;; une liste composee de leur premier element respectif, et on ajoute a
  ;;; ca un appel recursif sur la fonction avec le cdr de chaque liste
  (if (and L1 L2)
    (append
      (list (list (car L1) (car L2)))
      (grouper (cdr L1) (cdr L2))
    )

    ;;; Dans cette situation la deuxieme liste est vide, on ajoute donc seuelement
    ;;; le car de la premiere liste et on continue tant que cette liste n'est pas vide
    (if L1
      (append
        (list (list (car L1)))
        (grouper (cdr L1) NIL)
      )

      ;;; meme chose que precedemment mais dans le cas ou la premiere liste est vide
      (if L2
        (append
          (list (list (car L2)))
          (grouper NIL (cdr L2))
        )

        ;;; dans ce cas les deux listes sont vides, on retourne NIL
        NIL
      )
    )
  )
)

(grouper '(1 2 3) '(4 5 6)) ; test utilisation normale
(grouper '(1 2 3) '(4 5 6 7 8 9)) ; test deuxieme liste plus longue
(grouper '(1 2 3 a b c) '(4 5 6)) ; test premiere liste plus longue
(grouper NIL '(1 2 3)) ; test premiere liste vide
(grouper '(1 2 3) NIL) ; test deuxieme liste vide
(grouper NIL NIL) ; test deux listes vides




(defun monReverse(L)
  ;;; retourne la liste inverse de maniere recursive

  (if (>= (length L) 3)
    ; si la liste est de taille 3 ou plus
    ; on ajoute le premier element à la fin et on appelle recursivement le reste en le placant avant

    (append (monReverse (cdr L)) (list (car L)))

    (if (= (length L) 2)
        ; sinon la liste est de taille 2, 1, vide

        (append (cdr L) (list (car L))) ; liste de taille 2, meme chose que la taille 3 sans l'appel recursif

        L ; 1 ou vide on retourne la liste
    )
  )
)

(monReverse '(b o n j o u r)) ; test liste non vide
(monReverse NIL) ; test liste vide



(defun monReverse (L)
  (if L
    ;;; On ajoute le reverse du cdr de la liste, puis son car
    (append (monReverse (cdr L)) (list (car L)))
    NIL
  )
)

(monReverse '(b o n j o u r)) ; test liste non vide
(monReverse NIL) ; test liste vide



(defun palindrome (L)
  ;;; on verifie l'egalite avec le mot inverse
  (equal L (monReverse L))
)

(palindrome '(b o n j o u r)) ; test liste non palindrome
(palindrome '(k a y a k)) ; test liste palindrome
(palindrome NIL) ; test liste vide



(defun palindrome (L)
  ;;; si le mot est de taille 0 ou 1, c'est un palindrome, ceci est notre condition d'arret
  (if (or (equal (length L) 1) (equal (length L) 0))
    T

    ;;; sinon, on verifie l'egalite entre le premier et dernier element,
    ;;; puis on fait un appel recursif avec la liste sans le premier et dernier element
    (if (equal (car L) (car (last L)))
      (palindrome (cdr (reverse (cdr (reverse l)))))
      NIL
    )
  )
)

(palindrome '(b o n j o u r)) ; test liste non palindrome
(palindrome '(k a y a k)) ; test liste palindrome
(palindrome NIL) ; test liste vide