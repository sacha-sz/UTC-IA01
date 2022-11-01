;;; Q1

;; 20 états
;; 1 état initial (Entrée) 1 état final (Sortie)
;; ensemble d’action : états juxtaposés (que l’on peut visiter)


;;; Q2

(setq laby
	'
	(
		(E 1)
		(1 E 2)
		(2 1 7)
		(7 2 8 6)
		(6 7 3)
		(3 6)
		(8 7 9)
		(9 8 10)
		(10 9 15 11)
		(11 10 14 12)
		(12 11 5)
		(5 12 4)
		(4 5)
		(14 11)
		(15 10 16)
		(16 15 17)
		(17 16 18)
		(18 17 19)
		(19 18 20)
		(20 19 S 13)
		(13 20)
		(S 20)
	)
)


;;; Q3
(defun successeurs (etat laby)
    (cdr (assoc etat laby))
)

(successeurs 20 laby)

(defun successeurs_valides (etat lab chem)
  (let
      ((succ (successeurs etat lab)))

      (dolist (x succ succ)

        (if (member x chem)

            (setq succ (remove x succ))
        )
      )
   )
)

(successeurs_valides 20 laby '(19 16 15 17))