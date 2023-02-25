;; TP2-Resolution-2

;; Donnees enonce
(defparameter *actions* '((16 3 2 1)
                (15 3 2 1)
                (14 3 2 1)
                (13 3 2 1)
                (12 3 2 1)
                (11 3 2 1)
                (10 3 2 1)
                (9 3 2 1)
                (8 3 2 1)
                (7 3 2 1)
                (6 3 2 1)
                (5 3 2 1)
                (4 3 2 1)
                (3 3 2 1)
                (2 2 1)
                (1 1)))

;; fonction qui donne tous les successeurs d'un etat :
(defun successeurs (allumettes actions)
  (cdr (assoc allumettes actions)))


(defun Randomsuccesseurs (actions)
  (let ((r (random (length actions))))
    ;;(format t "~&~2t Resultat du random ~s~&" r)
    (nth r actions)
  )
)

;; Q1 - JeuJoueur

;; Version while
(defun JeuJoueur (nbAllu actions)
    (if (AND (< nbAllu 17) (> nbAllu -1) (integerp nballu) (listp actions))
    ;; test des parametres
        (let ((coup nil) (succ (successeurs nbAllu actions)))
           (format t "min:~d, max:~d~&" (car (last succ)) (car succ))
           (princ "Entrez le nombre d'allumettes a enlever :")
           (setq coup (read)) ;; recuperation de la valeur

           (while (not (member coup succ))
                ;; si erreur saisie alors nous demandons de nouveau une saisie
               (format t "Erreur saisie le nombre~&")
               (format t "min:~d,max:~d~&" (car(last succ)) (car succ))
               (princ "Entrez le nombre d'allumettes a enlever :")
               (setq coup (read))
         )
        coup
      )
    )
)

;; Version loop while
(defun JeuJoueur (allumettes actions)
  (if (AND (< allumettes 17) (> allumettes -1) (integerp allumettes) (listp actions))
      (let ((choix NIL)(choix_possibles (successeurs allumettes actions)))
        (loop while (not (member choix choix_possibles)) do
          (format t "Nombre d'allumettes ~s~& min : ~s~& max : ~s~&" allumettes (car (last choix_possibles)) (car choix_possibles))
          (princ "Entrez le nombre d'allumettes : ")
          (setq choix (read))
          (if (not (member choix choix_possibles))
              (format t "Erreur saisie nombre~&")
          )
        )
        choix
      )
  )
)

; (print (JeuJoueur 16 *actions*)) ; test pour une saisie comprise entre 1 et 3
; (print (JeuJoueur 2 *actions*)) ; test pour une saisie comprise entre 1 et 2
; (print (JeuJoueur 1 *actions*)) ; test ayant pour seule saisie : 1


;; Q2 - Explore-renf : sans renforcement, retour liste action


;; Version 1 : tour = soit IA soit humain
(defun explore-renf (nb_allu actions joueur i)
  (cond
   ((and (eq joueur 'humain) (eq nb_allu 0)) nil) ;; si l'IA ne gagne pas on retourne nil
   ((and (eq joueur 'IA) (eq nb_allu 0)) actions) ;; sinon la liste des actions
   (t
    (let  ((sol nil) (coup nil))
      (if (eq joueur 'humain)
          (progn
            ;; a l'humain de jouer, la ligne suivante est a decommenter si besoin
            ;; (setq coup (JeuJoueur nb_allu *actions*))
            (setq coup (Randomsuccesseurs (cdr (assoc nb_allu actions))))
            (format t "~%~V@tJoueur ~s joue ~s allumettes - il reste ~s allumette(s) " i joueur coup (- nb_allu coup))
            (setq sol (explore-renf (- nb_allu coup) actions 'IA (+ i 3)))
          )
          (progn
            ;; a l'IA de joueur
            (setq coup (Randomsuccesseurs (cdr (assoc nb_allu actions))))
            (format t "~%~V@tJoueur ~s joue ~s allumettes - il reste ~s allumette(s) " i joueur coup (- nb_allu coup))
            (setq sol (explore-renf (- nb_allu coup) actions 'humain (+ i 3)))
          )
        )
      sol
      )
    )
   )
)
; (print (explore-renf 16 *actions* 'IA 0))

;; Version 2 : tour = IA puis humain
(defun explore-renf (allumettes actions)
  (let ((choix_possibles (successeurs allumettes actions))(action_IA NIL)(action_humain NIL))

    ;; Tour le l'IA
    (format t "Restant IA : ~s ~s~&" allumettes choix_possibles)
    (setq action_IA (Randomsuccesseurs choix_possibles))
    (format t "choisi IA : ~s~2&" action_IA)

    (if (equal (- allumettes action_IA) 0)
      NIL

      ;; Tour de l'humain
      (progn

        (format t "Restant humain : ~s ~s~&" (- allumettes action_IA) (successeurs (- allumettes action_IA) actions))
        ;;(setq action_humain (JeuJoueur (- allumettes action_IA) actions)) ;; choix reel de l'humain
        (setq action_humain (Randomsuccesseurs (successeurs (- allumettes action_IA) actions))) ;; choix aleatoire de l'humain
        (format t "choisi humain : ~s~2&" action_humain)

        (if (equal (- allumettes action_IA action_humain) 0)
          actions
          (explore-renf (- allumettes action_IA action_humain) actions)
        )
      )
    )
  )
)

; (print (explore-renf 16 *actions*))


;; Q3 - Exploration avec renforcement

(defun explore-renf (allumettes actions)
  (let ((choix_possibles (successeurs allumettes actions))(action_IA NIL)(action_humain NIL))

    ;; Tour le l'IA
    (format t "Restant IA : ~s ~s~&" allumettes choix_possibles)
    (setq action_IA (Randomsuccesseurs choix_possibles))
    (format t "choisi IA : ~s~2&" action_IA)

    (if (equal (- allumettes action_IA) 0)
      NIL

      ;; Tour de l'humain
      (progn

        (format t "Restant humain : ~s ~s~&" (- allumettes action_IA) (successeurs (- allumettes action_IA) actions))
        ;;(setq action_humain (JeuJoueur (- allumettes action_IA) actions)) ;; choix reel de l'humain
        (setq action_humain (Randomsuccesseurs (successeurs (- allumettes action_IA) actions))) ;; choix aleatoire de l'humain
        (format t "choisi humain : ~s~2&" action_humain)

        (if (equal (- allumettes action_IA action_humain) 0)
          (progn
            (setq *actions* (renforcement allumettes action_IA *actions*))
            actions
          )
          (explore-renf (- allumettes action_IA action_humain) actions)
        )
      )
    )
  )
)

;; Q4 - Renforcement

; Version 1 - utilisant un dolist
(defun renforcement (nb_allu coup actions)
  (let ((new_liste nil))
    (format t "Renforcement de l'etat ~s avec l'action ~s~&" nb_allu coup)
    (dolist  (cp actions new_liste)
      ;; iteration sur la liste d'actions
      (if (= (car cp) nb_allu)
        ;; si je suis a l'action a renforcer
        ;; ajout du coup a la liste des coups possibles
        (setq new_liste (append new_liste (list (reverse (cons coup (reverse cp))))))

        ;; sinon ajout de la valeur deja presente dans la liste d'actions
        (setq new_liste (append new_liste (list cp)))
      )
    )

    ;; Modification de la liste globale
    (setq *actions* (reverse new_liste))
  )
)

; Version 2 - utilisant un let* et un tri avec sort
(defun renforcement (allumettes coup_gagnant actions)
  (let* ((etat (assoc allumettes actions))(actions_nouv (remove etat actions)))
    (format t "Renforcement de l'etat ~s avec l'action ~s~&" allumettes coup_gagnant)
    (setq etat (append (list (car etat) coup_gagnant) (cdr etat)))
    (push etat actions_nouv)
    (sort actions_nouv #'> :key #'car)
  )
)

; (renforcement 5 1 *actions*))


;; Q5 - explore-renf-rec

;; Version 1 : tour = soit IA soit humain
(defun explore-renf-rec (nb_allu actions joueur &optional (i 0))
  (cond
    ((and (eq joueur 'humain) (eq nb_allu 0)) nil) ;; l'IA ne gagne pas on retourne nil
    ((and (eq joueur 'IA) (eq nb_allu 0)) *actions*) ;; l'IA gagne on retourne *actions*
    (t
      (let  ((sol nil) (coup nil))
        (if (equal joueur 'humain)
          (progn
            ;; a l'humain de jouer, la ligne suivante est a decommenter si besoin
            ;; (setq coup (JeuJoueur nb_allu *actions*))
            (setq coup (Randomsuccesseurs (cdr (assoc nb_allu *actions*))))
            (format t "~%~V@tJoueur ~s joue ~s allumettes - il reste ~s allumette(s) " i joueur coup (- nb_allu coup))
            (setq sol (explore-renf-rec (- nb_allu coup) *actions* 'IA (+ i 3)))
          )

          (progn
            ;; a l'IA de jouer
            (setq coup (Randomsuccesseurs (cdr (assoc nb_allu *actions*))))
            (format t "~%~V@tJoueur ~s joue ~s allumettes - il reste ~s allumette(s) " i joueur coup (- nb_allu coup))
            (setq sol (explore-renf-rec (- nb_allu coup) *actions* 'humain (+ i 3)))
            (if sol
              ;; Si ce choix a mener a une victoire de l'IA on renforce la liste des actions avec ce coup
              (renforcement nb_allu coup *actions*)
            )
          )
        )
      sol
      )
    )
  )
)

;; Version 2 : tour = IA puis humain
(defun explore-renf-rec (allumettes actions)
  (let ((choix_possibles (successeurs allumettes actions))(action_IA NIL)(action_humain NIL))

    ;; Tour le l'IA
    (format t "Restant IA : ~s ~s~&" allumettes choix_possibles)
    (setq action_IA (Randomsuccesseurs choix_possibles))
    (format t "choisi IA : ~s~2&" action_IA)

    (if (equal (- allumettes action_IA) 0)
      NIL

      ;; Tour de l'humain
      (progn

        (format t "Restant humain : ~s ~s~&" (- allumettes action_IA) (successeurs (- allumettes action_IA) actions))
        ;;(setq action_humain (JeuJoueur (- allumettes action_IA) actions)) ;; choix reel de l'humain
        (setq action_humain (Randomsuccesseurs (successeurs (- allumettes action_IA) actions))) ;; choix aleatoire de l'humain
        (format t "choisi humain : ~s~2&" action_humain)

        (if (equal (- allumettes action_IA action_humain) 0)
          (progn
            (setq *actions* (renforcement allumettes action_IA *actions*))
            actions
          )
          (if (explore-renf-rec (- allumettes action_IA action_humain) actions)
            (progn
              (setq *actions* (renforcement allumettes action_IA *actions*))
              actions
            )
            NIL
          )
        )
      )
    )
  )
)

;; ***** BONUS : *****
(defun aff(l)
  ; permet un affichage plus clair de la liste des actions
  (mapcar 'print l)
)


; (print (explore-renf-rec 16 *actions*))
; (aff *actions*)

;; ***** BONUS - On entraine l'IA : *****
; (dotimes (x 50)(explore-renf-rec 16 *actions*))
; (dotimes (x 50)(explore-renf-rec 16 *actions* 'IA))