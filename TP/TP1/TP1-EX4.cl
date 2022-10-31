;;; Rendu TP1 :

;   Exercice 4.1


;; Le format de chaque conflit est le suivant :
; ("nom de la guerre" DateDebut DateFin ((liste allies) (liste ennemis)) (liste lieux))

(setq BaseTest
 '(

     ("Campagnes de Clovis Ier" 486 508 (("Royaume franc")("Domaine gallo romain de Soissons")) ("Soissons"))

     ("Campagnes de Clovis Ier" 486 508 (("Royaume franc")("Royaume des Burgondes")) ("Dijon"))

     ("Campagnes de Clovis Ier" 486 508 (("Royaume franc")("Royaume alaman")) ("Zulpich" "Cologne"))

     ("Campagnes de Clovis Ier" 486 508 (("Royaume franc")("Royaume wisigoth")) ("Vouille" "Vienne"))

     ("Campagnes de Clovis Ier" 486 508 (("Royaume franc")("Royaume ostrogoth" "Royaume wisigoth")) ("Arles" "Bouches du Rhone"))

     ("Guerre de Burgondie" 523 533 (("Royaume franc")("Royaume des Burgondes")) ("Vezeronce" "Arles"))

     ("Conquete de la Thuringe" 531 531 (("Royaume franc")("Thuringes")) ("Thuringe"))

     ("Guerre des Goths" 535 553 (("Royaume ostrogoth" "Alamans" " Royaume franc" "Royaume wisigoth" "Burgondes")("Empire byzantin")) ("Peninsule italienne"))

     ("Conquete de l'Alemanie" 536 536 (("Royaume franc")("Alamans")) ("Alemanie"))

     ("Conquete de la Baviere" 555 555 (("Royaume franc")("Bavarii")) ("Baviere"))

     ("Campagnes de Bretagne" 560 578 (("Royaume franc")("Royaume du Vannetais")) ("Vannetais"))

     ("Guerre de succession merovingienne" 584 585 (("Royaume franc")("Royaume d Aquitaine")) ("Comminges"))

     ("Guerre franco-frisonne" 600 793 (("Royaume franc")("Royaume de Frise")) ("Pays Bas" "Allemagne"))

     ("Guerre civile des Francs" 715 719 (("Neustrie")("Austrasie")) ("Royaume franc"))

     ("Invasion omeyyade en France" 719 759 (("Royaume franc")("Califat omeyyade")) ("Royaume d Aquitaine" "Septimanie"))

     ("Guerre des Lombards" 755 758 (("Royaume franc")("Lombards")) ("Lombardie"))

     ("Guerre d Aquitaine" 761 768 (("Royaume franc")("Aquitains")) ("Vasconie Aquitaine"))

     ("Guerre des Saxons" 772 804 (("Royaume franc")("Saxons")) ("Germanie"))

     ("Guerre des Lombards" 773 774 (("Royaume franc")("Lombards")) ("Lombardie"))

     ("Guerre des Avars" 791 805 (("Royaume de France")("Avars")) ("Pannonie"))

     ("Invasions sarrasines en Provence" 798 990 (("Royaume de France" "Comte de Provence")("Sarrasins")) ("Provence"))

     ("Guerre civile entre les fils de Louis le Pieux" 830 842 (("Francie occidentale" "Francie orientale")("Francie mediane")) ("Fontenoy"))

     ("Guerre franco-bretonne" 843 851 (("Royaume de France")("Royaume de Bretagne" "Vikings")) ("Royaume de Bretagne"))

     ("Luttes inter-dynastiques carolingiennes" 876 946 (("Francie occidentale" "Francie orientale")("Royaume de Bourgogne" "Francie orientale")) ("Ardennes" "Saone-et-Loire" "Rhenanie-Palatinat" "Aisne"))

     ("Invasions vikings en France" 799 1014 (("Royaume de France")("ennemis")) ("Normandie" "Bretagne"))

     ("Premiere croisade" 1096 1099 (("Comte de Blois" "Comte de Toulouse" "Comte de Boulogne" "Marquisat de Provence" "Comte de Flandre" "Duche de Normandie" "Diocese du Puy-en-Velay" "Comte de Vermandois" "Republique de Genes" "Duche de Basse-Lotharingie" "Principaute de Tarente" "Empire byzantin" "Royaume de Petite-Armenie" "Croises" "Royaume de France")("Sultanat de Roum" "Danichmendides" "Califat fatimide")) ("Terre sainte"))
 )
)


;   Exercice 4.2

(defun dateDebut (conflit)
  ;;; Retourne le 2 eme element du conflit
  (cadr conflit)
)


(dateDebut (car BaseTest)) ; test liste non vide
(dateDebut NIL) ; test liste vide


(defun nomConflit (conflit)
  ;;; Retourne le 1er element du conflit
  (car conflit)
)


(nomConflit (car BaseTest)) ; test liste non vide
(nomConflit NIL) ; test liste vide



(defun allies (conflit)
  ;;; Retourne le 1 eme element du 4eme element du conflit
  (car (cadddr conflit))
)


(allies (car BaseTest)) ; test liste non vide
(allies NIL) ; test liste vide



(defun ennemis (conflit)
  ;;; Retourne le 2 eme element du 4eme element du conflit
  (cadr (cadddr conflit))
)


(ennemis (car BaseTest)) ; test liste non vide
(ennemis NIL) ; test liste vide



(defun lieu (conflit)
  ;;; Retourne le 5 eme element du conflit
  (car (cddddr conflit))
)


(lieu (car BaseTest)) ; test liste non vide
(lieu NIL) ; test liste vide


;;; Fonction supplementaire dateFin

(defun dateFin (conflit)
  ;;; Retourne le 3 eme element du conflit
  (caddr conflit)
)


(dateFin (car BaseTest)) ; test liste non vide
(dateFin NIL) ; test liste vide


;   Exercice 4.3

; fonction qui affiche tous les conflits
(defun FB1 (base_test)
  (if (and base_test (listp base_test))
    (mapcar 'print base_test)
    NIL
  )
)


(FB1 BaseTest) ; test liste non vide
(FB1 NIL) ; test liste vide
(FB1 0) ; test non liste

; fonction qui affiche tous les conflits
(defun FB1 (base_test)
  (if (and base_test (listp base_test))
    (mapcar #'(lambda (conflit)
                (list
                 (nomConflit conflit)
                 (dateDebut conflit)
                 (dateFin conflit)
                 (allies conflit)
                 (ennemis conflit)
                 (lieu conflit)
                 )
                )
    base_test)
    NIL
  )
)


(FB1 BaseTest) ; test liste non vide
(FB1 NIL) ; test liste vide
(FB1 0) ; test non liste

; fonction qui affiche les conflits du "Royaume Franc"
(defun FB2 (base_test)
  (if (and base_test (listp base_test))
    (mapcar #'(lambda (conflit)
                (dolist (allie (allies conflit))
                  (if (equal allie "Royaume franc")
                    (print conflit)
                  )
                )
              )
    base_test)
    NIL
  )
)


(FB2 BaseTest) ; test liste non vide
(FB2 NIL) ; test liste vide
(FB2 0) ; test non liste

; fonction qui retourne la liste des conflits dont un allie est precise en argument
(defun FB3 (base_test allie_rec)
  (if (and base_test (listp base_test) (stringp allie_rec))
    (let ((liste_conflits NIL))
      (mapcar #'(lambda (conflit)
        (dolist (allie (allies conflit))
          (if (equal allie allie_rec)
            (setq liste_conflits (cons conflit liste_conflits))
          )
        )
      )
      base_test)

      (reverse liste_conflits)
    )
    NIL
  )
)


(FB3 BaseTest "Lombards") ; test liste non vide et recherche non vide
(FB3 BaseTest "abcde") ; test liste non vide et recherche non vide avec élément non présent
(FB3 BaseTest NIL) ; test liste non vide et recherche vide
(FB3 NIL NIL) ; test liste vide et recherche vide
(FB3 0 "Lombards") ; test non liste


; fonction qui retourne le conflit dont la date de debut est 523
(defun FB4 (base_test)
  (if (and base_test (listp base_test))
    (let ((copie_base base_test))
      (loop
        (if (not (equal (dateDebut (car copie_base)) 523))
              (pop copie_base)
              (return (car copie_base))
        )
      )
    )
    NIL
  )
)


(FB4 BaseTest) ; test liste non vide
(FB4 NIL) ; test liste vide
(FB4 0) ; test non liste


; fonction qui retourne la liste des conflits dont la date de debut est comprise entre 523 et 715
(defun FB5 (base_test)
  (if (and base_test (listp base_test))
    (let ((liste_conflits NIL))
      (mapcar #'(lambda (conflit)
                  (if (and (>= (dateDebut conflit) 523) (<= (dateDebut conflit) 715))
                    (setq liste_conflits (cons conflit liste_conflits))
                  )
                )
      base_test)

      (reverse liste_conflits)
    )
    NIL
  )
)


(FB5 BaseTest) ; test liste non vide
(FB5 NIL) ; test liste vide
(FB5 0) ; test non liste


; fonction qui calcule et retourne le nombre de conflits ayant pour ennemis les "Lombards"
(defun FB6 (base_test)
  (if (and base_test (listp base_test))
    (let ((compteur 0))
      (mapcar #'(lambda (conflit)
        (dolist (ennemi (ennemis conflit))
          (if (equal ennemi "Lombards")
            (setq compteur (+ compteur 1))
          )
        )
      )
      base_test
      )

      compteur
    )
    NIL
  )
)


(FB6 BaseTest) ; test liste non vide
(FB6 NIL) ; test liste vide
(FB6 0) ; test non liste

; fonction qui calcule et retourne le nombre de conflits ayant pour ennemis les "Lombards"
(defun FB6 (base_test)
  (if (and base_test (listp base_test))
    (let ((liste_lombard NIL))
       (length
          (dolist (conflit base_test liste_lombard)
            (dolist (ennemi (ennemis conflit))
              (if (equal ennemi "Lombards")
                  (setq liste_lombard (append liste_lombard (list conflit)))
              )
            )
          )
      )
    )
    NIL
  )
)



(FB6 BaseTest) ; test liste non vide
(FB6 NIL) ; test liste vide
(FB6 0) ; test non liste
