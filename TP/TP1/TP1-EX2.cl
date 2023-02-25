;;; Rendu TP1 :

; grouper de l'exercice 1

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


;   Exercice 2

(defun list-triple-couple (L)
  ;;; on se sert de la fonction grouper de l'exercice 1 pour faire des couples
  ;;; on calcul le triple de chaque nombre a l'aide de mapcar
  (grouper L (mapcar #'(lambda (x) (* x 3)) L))
)


(list-triple-couple '(1 2 3)) ; test avec une liste de taille 3
(list-triple-couple '(1 2 3 4 5 6 7 8 9 10)) ; test avec une liste de taille 10
(list-triple-couple '()) ; test avec une liste vide
