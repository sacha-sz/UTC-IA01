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

    ;;; Dans cette situation la deuxieme liste est vide, on ajoute donc seulement
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


;   Exercice 3.1

(defun my-assoc (cle a-list)
  ;;; on verifie qu'on a bien une liste non vide
  (if a-list
    ;;; si le premier element est une liste dont le car est la cle,
    ;;; alors on retourne cette liste
    (if (equal (car (car a-list)) cle)
      (car a-list)

      ;;; Sinon, on refait une recherche sur le reste de la liste
      (my-assoc cle (cdr a-list))
    )

    ;;; Si l'element n'est pas dans la liste, on renvoit NIL
    NIL
  )
)

(my-assoc 'Pierre '((Yolande 25) (Pierre 22) (Julie 45))) ; test de succes de recherche d'une cle
(my-assoc 'Yves '((Yolande 25) (Pierre 22) (Julie 45))) ; test d'echec de recherche d'une cle


;   Exercice 3.2

;;; version recursive

(defun cles (a-list)
  (if a-list
    ;;; On recupere le car du car soit la premiere cle, puis on fait un appel
    ;;; recursif sur le cdr de la liste pour recuperer les autres cles.
    (append (list (car (car a-list))) (cles (cdr a-list)))
    NIL
  )
)

(cles '((Yolande 25) (Pierre 22) (Julie 45))) ; retourne les cles d'une liste
(cles NIL) ; retourne les cles d'une liste


;;; version iterative
(defun cles (a-list)
  ;;; Retourne les differentes cles d'une a-list
  ;;; Le car de chaque element

  (mapcar #'car a-list)
)

(cles '((Yolande 25) (Pierre 22) (Julie 45))) ; retourne les cles d'une liste
(cles NIL) ; retourne les cles d'une liste


;   Exercice 3.3

(defun creation (listeCles listeValeurs)
  ;;; on verifie que les deux listes soient de meme taille
  (if (equal (length listeCles) (length listeValeurs))

    ;;; si c'est le cas on utilise simplement la fonction grouper de l'exercice 1
    (grouper listeCles listeValeurs)

    ;;; sinon on ne peut pas creer de a-liste
    NIL
  )
)

(creation '(Yolande Pierre Julie) '(25 22 45)) ; test de succes de creation d'une a-list a partir de deux listes de taille �gale
(creation '(Yolande Pierre Julie) '(25 22)) ; test d'echec de creation d'une a-list a partir de deux listes de taille differentes
