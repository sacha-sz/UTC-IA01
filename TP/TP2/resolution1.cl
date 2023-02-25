;; TP2-Resolution-1

;; Données enonce
(setq actions '((16 3 2 1)
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

;; EXPLORE
(defun explore (allumettes actions joueur i)
  (cond
   ((and (eq joueur 'humain) (eq allumettes 0)) nil)
   ((and (eq joueur 'IA) (eq allumettes 0)) t)
    (t (progn
         (let ((sol nil) (coups (successeurs allumettes actions)))
           (while (and coups (not sol))
             (progn
               (format t "~%~V@tJoueur ~s joue ~s allumettes - il reste ~s allumette(s) " i joueur (car coups) (- allumettes (car coups)))
                (setq sol (explore (- allumettes (car coups)) actions (if (eq joueur 'IA) 'humain 'IA) (+ i 3)))
                (if sol
                    (setq sol (car coups)))
               (format t "~%~V@t sol = ~s~%" i sol)
               (pop coups)
               )
             )
           sol)))))

(defvar nbCoupsAJouer nil)
(setq nbCoupsAJouer (explore 16 actions 'IA 0))
(setq nbCoupsAJouer (explore 8 actions 'IA 0))
(setq nbCoupsAJouer (explore 3 actions 'IA 0))


;; Explication fonction explore :
(defun explore (allumettes actions joueur i)
;; Définition de la fonction explore ayant pour paramètre :
;; + le nombre d'allumettes restantes
;; + la liste des actions possibles (cf actions)
;; + le joueur ayant la main
;; + i : utile pour l'espace permettant l'affichage

  (cond
;; On entre dans un bloc conditionnel

   ((and (eq joueur 'humain) (eq allumettes 0)) nil)
;; Si c'est à l'humain de joueur et qu'il ne reste plus d'allumettes (il a gagné)
;; La fonction retourne nil ce qui est considéré comme faux puisque ne faisant pas gagner l'IA

   ((and (eq joueur 'IA) (eq allumettes 0)) t)
;; Si c'est à l'IA de joueur et qu'il ne reste plus d'allumettes (elle a gagné)
;; La fonction retourne t ce qui est considéré comme vrai puisque faisant gagner l'IA

    (t (progn
;; Si ce n'est aucun de ces deux cas finaux alors nous sommes encore en cours de partie et dans ce cas :

         (let ((sol nil) (coups (successeurs allumettes actions)))
  ;; On initialise deux variables locales (au let)
  ;; + sol : initialisé à nil (faux) permettant de savoir si ce chemin mène à la victoire de l'IA
  ;; + coups : permettant de savoir combien d'allumettes la personne peut retirer (exemple à 16 nous avons 3 2 1)

           (while (and coups (not sol))
           ;; Tant que coups n'est pas vide et que nous n'avons trouvé un chemin menant à la victoire de l'IA

             (progn
             ;; Réalisation de plusieurs actions

               (format t "~%~V@tJoueur ~s joue ~s allumettes - il reste ~s allumette(s) " i joueur (car coups) (- allumettes (car coups)))
               ;; Affichage de :
               ;; + un espace (grâce à i)
               ;; + qui joue
               ;; + le nombre d'allumettes qui va être retirer (le car donc on commence par le plus grand nombre possible)
               ;; + le nombre d'allumettes restantes après ce coup

                (setq sol (explore (- allumettes (car coups)) actions (if (eq joueur 'IA) 'humain 'IA) (+ i 3)))
                ;; On affecte à sol le résultat de l'exploration de coup, va valoir soit nil (faux, l'IA ne gagne pas)
                ;; soit le nombre d'allumettes permettant la victoire (vrai)

                (if sol
                ;; Si une solution est trouvée

                    (setq sol (car coups)))
                ;; on affecte à sol le nombre d'allumettes a retiré

               (format t "~%~V@t sol = ~s~%" i sol)
               ;; On affiche ce nombre d'allumettes

               (pop coups)
               ;; On retire le nombre d'allumettes a retiré pour ce coup et s'il n'y a pas de solution on passe au nombre suivant (3 : faux -> 2: ?)

               )
             )
           sol)
         )
       )
     )
   ) ;; on retourne la solution
