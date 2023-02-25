;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base de regles, base de faits, solutions et questions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *regles* NIL)
(defvar *faits* NIL)
(defvar *solutions* NIL)
(defvar *questions* NIL)
(defvar valeur NIL)
(defvar *pb_courant* '(
  (ANALYSE_ANTIVIRUS
    0.4
    "~&Ton probleme est lie a ton Os, ton ordinateur demarre et est capable de faire des mises a jour~&Cependant le probleme affecte les performances et/ou l'integrite de ton ordinateur malgre un redemarrage.")
  (REMPLACEMENT_BATTERIE
    0.2
    "~&Ton probleme est materiel, empeche ton ordinateur de fonctionner pleinement.~&Il est lie a l'energie et persiste apres un changement de chargeur.")
  (INSTALLATION_OS
    0.1
    "~&Votre ordinateur ne demarre pas et ne possede qu'un seul OS.")
  (MAJ_REINSTALLATION
    0.3
    "~&Votre probleme est lie a un logiciel specifique qui ne fonctionne plus sur votre ordinateur.")))


; Remise à zéro des bases de faits pour permettre de tester plusieurs fois le programme
(defun init ()
  (loop while *faits* do (pop *faits*))
  (ajouter_fait 'description NIL)
  (ajouter_fait 'solution NIL)

  (setq valeur NIL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonctions pour les manipuler ces bases ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ajouter_regle (conditions conclusion)
  ;;; Permet l'ajout d'une regle
  (let ((num_regle (gentemp "R")))
    (set num_regle (list conditions conclusion))
    (pushnew num_regle *regles*)))


(defun modifier_fait (variable valeur)
  ;;; Permet de modifier la valeur d'un fait existant dans la base de faits
  (setf (cadr (assoc variable *faits*)) valeur))


(defun ajouter_fait (variable valeur)
  ;;; Permet d'ajouter un fait
  (if (assoc variable *faits*)
    (modifier_fait variable valeur)
    (pushnew (list variable valeur) *faits*)))


(defun ajouter_solution (solution texte_solution)
  ;;; Permet d'ajouter une solution
  (pushnew (list solution texte_solution) *solutions*))


(defun afficher_solution (solution)
  ;;; Permet d'afficher la solution passee en parametre
  (let ((element (assoc solution *solutions*)))
    (if element
      (progn
        (format t "**********************************************************")
        (format t "~&Voici la solution a votre probleme :")
        (format t (cadr element)))

      (format t "Solution inconnue : ~s ~&" (symbol-name solution)))))


(defun ajouter_question (question texte_question valeur_valide)
  ;;; Permet d'ajouter une question
  (pushnew (list question texte_question valeur_valide) *questions*))


(defun demander_valeur_question (question)
  ;;; Recupere le resultat d'une question tout en verifiant la valeur
  (let* ((element (assoc question *questions*))(valeur NIL)(valeur_valide (caddr element)))
    (if element
      (progn
        (loop while (not (eval valeur_valide)) do
          (progn
            (format t (cadr element))
            (setq valeur (read))))

        (cond ((eq valeur 'y) T)
              ((eq valeur 'n) nil)
              (t valeur)))

      (progn
        (format t "Question inconnue : ~s ~&" (symbol-name question))
        NIL))))


;;;;;;;;;;;;;;;;;;;;;;;;
;; fonctions usuelles ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun appartient_aux_faits (fait)
  (let ((element (assoc (cadr fait) *faits*)))
    (if element
      (funcall (car fait) (cadr element) (caddr fait))
      NIL)))


(defun conclusion_regle (regle)
  (cadr (eval regle)))


(defun conditions_regle (regle)
  (car (eval regle)))


(defun regles_candidates (but regles)
  (if regles
    (if (equal (conclusion_regle (car regles)) but)
      (cons (car regles) (regles_candidates but (cdr regles)))
      (regles_candidates but (cdr regles)))))


(defun chainage_arriere (but)
  ;; On teste si le but déjà est un fait
  (if (appartient_aux_faits but)
    T
    (let
      ((cand (regles_candidates but *regles*)) (ok NIL) (prem NIL) (condition NIL) (regle NIL)(valeur NIL))

      ;; On teste les regles candidates pour arriver au but
      (loop while (and cand (not ok)) do

        ;; On récupére les conditions de chaque regle candidate
        (setq regle (pop cand))
        (setq prem (conditions_regle regle))
        (setq ok T)

        ;; On teste si toutes les conditions sont remplies
        (loop while (and prem ok) do
          (setq condition (pop prem))
          (setq ok (chainage_arriere condition))) ;; appel récursif pour procéder à la vérification

        ;; Si la condition est remplie, on peut ajouter la conclusion de la regle à la base de faits
        (if ok
          (ajouter_fait (cadr (conclusion_regle regle)) (caddr (conclusion_regle regle)))))

      ;; Si la condition n'est pas remplie, et que la variable dans le but recherché n'a
      ;; pas encore de valeur dans la base de faits, on demande à l'utilisateur de la renseigner
      (if (not ok)
        (if (not (assoc (cadr but) *faits*))
          (progn
            (setq valeur (demander_valeur_question (cadr but)))
            (if valeur
              (progn
                ;; On ajoute la valeur de la variable dans la base de faits
                (ajouter_fait (cadr but) valeur)

                ;; On teste à nouveau si le but est rempli
                (setq ok (appartient_aux_faits but)))))))

      ok)))


(defun chainage-arriere ()
  (let ((sol nil))
    ;; On affiche les solutions possible
    (format t "~2&Quelle solution vous semble appropriee pour le probleme que vous avez ?~&")
    (loop for solution in *solutions* do
      (progn
        (format t "~2&~a : " (car solution))
        (format t (cadr solution))))
    ;; On demande à l'utilisateur de choisir une solution tant que celle-ci n'est pas dans la liste des solutions possibles
    (loop while (not (assoc sol *solutions*)) do
      (progn
        (format t "~2&Votre choix : ")
        (setq sol (read))))

    ;; On procède un chainage arrière et on indique si la solution testée est la bonne ou non
    (if (chainage_arriere `(eq Solution ,sol))
      (format t "~&La solution ~a est la bonne" sol)
      (format t "~&La solution ~a n'est pas la bonne" sol))))


;----------------------
;   Chainage avant    |
;----------------------

(defun get-description-val-regles (regle)
  ;;; Recherche la valeur de description dans la regle
  (let ((val nil))
    (dolist (condition (conditions_regle regle))
      (if (member 'Description condition)
        (setq val (caddr condition))))
    val))


(defun get-nom-var-regles (regle)
  ;;; Retourne le nom de la seconde valeur tester dans les regles
  (cadr (cadr (conditions_regle regle))))


(defun find-next-regles (desc)
  ;;; Cherche la regle qui suit une valeur prise par Description
  (let ((next-regle nil))
    (dolist (regle *regles*)
      (if (eq (get-description-val-regles regle) desc)
        (pushnew regle next-regle)))
    next-regle))


(defun sort-list-of-lists (ma-liste)
  (if (> (length ma-liste) 1)
    (sort ma-liste (lambda (a b) (> (second a) (second b))))
    ma-liste))


(defun demander-nom ()
  ;;; Demande a l'utilisateur d'entrer le nom d'un probleme et verifie qu'il est dans la liste des solutions possibles.
  (let ((nom nil))
    (loop
      (format t "~&Entrez le nom de la solution parmis cette liste : ")
      (mapcar #'print (mapcar #'first *solutions*))
      (format t "~&Votre saisie :")
      (setq nom (read))
      (if (member nom (mapcar #'first *solutions*))
        (return nom)
        (format t "~&Ce nom de probleme n'est pas valide. Veuillez reessayer.~%")))))


(defun remove-elements-with-zero-cadr (lst)
  (let ((result nil))
    (dolist (elt lst result)
      (if (not (= 0 (cadr elt)))
        (push elt result)))))


(defun ajouter-pb-courant ()
  (let ((nom nil) (description nil) (somme_proba 0) (prob_temp -1))
    (format t "~&**************** MAJ probleme courant ****************")
    ;;; Saisie du nom
    (setq nom (demander-nom))

    ;;; Saisie de la description
    (format t "~&Entrez la description du probleme : ")
    (setq description (concatenate 'string "~&" (read-line)))

    (pushnew (list nom 1 description) *pb_courant*)

    ;;; Saisie de toutes les probabilites
    (loop while (not (= (apply #'+ (mapcar #'second *pb_courant*)) 1)) do
      (format t "~&La somme des probabilites doit être egale a 1.~&Veuillez verifier les probabilites des problemes deja enregistres et entrer de nouvelles probabilites si necessaire.")
      (setq somme_proba 0)
      (dolist (pb  *pb_courant*)
        (setq prob_temp -1)
        (format t "~&Somme proba actuelle : ~,3f~&Reste : ~,3f" somme_proba (- 1 somme_proba))
        (format t "~&Entrez la probabilite du probleme dont la solution est '~a'" (first pb))
        (loop while (not (and (floatp  prob_temp) (<= 0 prob_temp) (<= prob_temp 1))) do
          (format t "~&Votre saisie (entre 0 et 1): ")
          (setq prob_temp (READ))
          (format t "~&Entree non valide, veuillez entrer un nombre entre 0 et 1"))
        (setf (second pb) prob_temp)
        (setq somme_proba (+ somme_proba prob_temp))))

    ;;; On delete les elements dont la proba est de 0
    (setq *pb_courant* (remove-elements-with-zero-cadr *pb_courant*))

    (format t "~&La base des problemes les plus courant a ete mise a jour")
    (format t "~&********************************************")))


(defun chainage-avant ()
  (let ((val nil) (next-regles nil) (var nil) (ok t) (rep_pb_courant nil) (pb_courant *pb_courant*))
    ;; Debut => demander les problemes les plus courants
    (format t "~&*************************************************************")

    ;; tri par ordre decroissant des probabilites des problemes les plus courants
    ;; du plus courant au moins courant
    (setq pb_courant (sort-list-of-lists pb_courant))

    (format t "~&Commencons par voir les problemes les plus courants.")
    (dolist (pb_cour pb_courant)
      (setq rep_pb_courant nil)
      (format t "~&Votre probleme correspond a la description suivante :")
      (format t (caddr pb_cour))
      (loop while (not (OR (eq rep_pb_courant 'y) (eq rep_pb_courant 'n))) do
        (format t "(Y/N)~&Votre choix : ")
        (setq rep_pb_courant (READ)))
      (if (eq rep_pb_courant 'y)
        (progn
          (ajouter_fait 'solution (car pb_cour))
          (afficher_solution (cadr (assoc 'solution *faits*)))
          (return-from chainage-avant T))))

    (format t "~&*************************************************************")
    (format t "~&Votre probleme ne semble pas etre un probleme courant.~&Decrivez moi plus en details votre probleme.")
    (format t "~&*                                                           *")

    (setq val (demander_valeur_question 'Type))
    (if val
      (progn
        (ajouter_fait 'Type val)
        (ajouter_fait 'Description val)

        (loop while (not (cadr (assoc 'solution *faits*))) do
          (setq next-regles (find-next-regles (cadr (assoc 'Description *faits*))))
          (setq var (get-nom-var-regles (car next-regles)))
          (setq val (demander_valeur_question var))
          (ajouter_fait var val)

          (loop for regles in next-regles do
            (progn
              (setq ok t)
              (dolist (condition (conditions_regle regles))
                (setq ok (appartient_aux_faits condition)))
              (if ok
                (ajouter_fait (cadr (conclusion_regle regles)) (caddr (conclusion_regle regles)))))))

        (afficher_solution (cadr (assoc 'solution *faits*)))
        (format t "~&*************************************************************")))))

;----------------------------
;     Ajout des solutions   |
;----------------------------

(ajouter_solution 'CONTACT_OPERATEUR "~&Votre ordinateur ne parvient pas a se connecter a internet ou avec une connexion lente~&Cela peut etre du a un probleme de routeur ou alors a votre moyen de connexion ~&Contactez votre operateur pour resoudre ces problemes de connexion.")
(ajouter_solution 'REMPLACEMENT_REPARATEUR "~&Reparez ou remplacez le composant faisant defaut.~&Si vous n'avez pas les connaissances pour le faire, contactez un reparateur.")
(ajouter_solution 'REMPLACEMENT_BATTERIE "~&Votre batterie arrive en fin de vie, pensez a la changer~&Les batteries en fin de vie ont tendance a se vider plus vite et a charger moins bien.")
(ajouter_solution 'REMPLACEMENT_CHARGEUR "~&Changez de chargeur, votre chargeur actuel arrive en fin de vie.")
(ajouter_solution 'MAJ_CARTE_GRAPHIQUE "~&Le probleme vient de la carte graphique~&Mettez a jour les pilotes de votre carte graphique~&Si le probleme persiste, faites reparer votre carte graphique ou remplacez-la.")
(ajouter_solution 'REMPLACEMENT_ECRAN "~&Votre ecran est probablement endommage, faites-le, reparez ou reparez-le.~&S'il n'est pas reparable, alors remplacez-le.")
(ajouter_solution 'REMPLACEMENT_PERIPH "~&Votre peripherique est probablement endommage ou en fin de vie~&Si une reparation est impossible, remplacez-le.")
(ajouter_solution 'REMPLACEMENT_PORT "~&Il se peut que votre port soit endommage, reparez-le ou faites-le reparer")
(ajouter_solution 'INSTALLATION_PILOTE "~&Votre ordinateur ne possede probablement pas le pilote lui permettant de communiquer avec le peripherique.~&Cherchez le sur internet et installez-le.")
(ajouter_solution 'SIGNALER "~&Le probleme vient probablement du logiciel et non de votre ordinateur.~&Les logiciels mettent souvent en place un moyen de signaler un probleme aux developpeurs, en particulier pour les logiciels open source~&N'hesitez pas a le faire, vous contriburez ainsi au perfectionnement des outils que vous utilisez.")
(ajouter_solution 'LOGICIEL_ALTERNATIF "~&Envisagez de passer a un logiciel alternatif.~&Les logiciels open source sont souvent compatibles avec un grand nombre d'appareils.")
(ajouter_solution 'MAJ_REINSTALLATION "~&Essayez de mettre a jour le logiciel.~&Si cela ne fonctionne pas, sauvegardez vos donnees, desinstallez et reinstallez-le.~&Il est possible que le logiciel ait ete mal installe depuis le depart ou que des actions de l'utilisateur aient pu corrompre une partie du logiciel.~&Une reinstallation permet de remettre tout au propre.")
(ajouter_solution 'INSTALLATION_OS "~&Installer un systeme d'exploitation, avec une cle usb bootable par exemple.")
(ajouter_solution 'INSTALLATION_BOOTLOADER "~&Reinstaller le bootloader correspondant a votre systeme.~&Par exeple, installer windows boot loader pour windows par exemple.")
(ajouter_solution 'INSTALLATION_BOOTLOADER_MULTIPLE "~&Installer un bootloarder permettant de lancer plusieurs systemes d'exploiration, GNU grub par exemple.")
(ajouter_solution 'ACHETER_ORDI "~&Achetez un ordinateur plus recent ou installez un systeme d'exploitation qui ne fait pas d'obsolescence programmee.")
(ajouter_solution 'GESTION_ERREUR_INTERNE "~&Ouvrez l'utilitaire de gestion de probleme de votre systeme d'exploitation et lancez une analyse.~&Si cela ne donne rien, copiez le code d'erreur indique lors de l'erreur de mise a jour, et collez-le dans votre moteur de recherche pour connaitre la demarche a suivre.")
(ajouter_solution 'ANALYSE_ANTIVIRUS "~&Le probleme peut etre lie a des fichiers corrompus ou a un programme malveillant.~&Dans tous les cas, lancez une analyse avec les differents logiciels dont vous disposez et ceux mis en place par votre systeme d'exploitation.")
(ajouter_solution 'REDEMARRER_MEMOIRE "~&Redemarrez votre ordinateur.~&Un redemarrage de temps en temps permet de nettoyer la memoire et de fermer les processus qui tournent dans le vide")


;--------------------------
;     Ajout des regles    |
;--------------------------


(ajouter_regle '((eq Type materiel)) '(eq Description materiel))
(ajouter_regle '((eq Type logiciel)) '(eq Description logiciel))
(ajouter_regle '((eq Description materiel) (eq Nature fonctionnement)) '(eq Description Fonctionnement_materiel))
(ajouter_regle '((eq Description materiel) (eq Nature performance)) '(eq Description Performance_materiel))
(ajouter_regle '((eq Description Performance_materiel) (eq Sujet internet)) '(eq Solution CONTACT_OPERATEUR))
(ajouter_regle '((eq Description Performance_materiel) (eq Sujet systeme)) '(eq Solution REMPLACEMENT_REPARATEUR))
(ajouter_regle '((eq Description Fonctionnement_materiel) (eq Domaine element_critique)) '(eq Solution REMPLACEMENT_REPARATEUR))
(ajouter_regle '((eq Description Fonctionnement_materiel) (eq Domaine peripherique)) '(eq Description Fonctionnement_materiel_peripherique))
(ajouter_regle '((eq Description Fonctionnement_materiel) (eq Domaine affichage)) '(eq Description Fonctionnement_materiel_affichage))
(ajouter_regle '((eq Description Fonctionnement_materiel) (eq Domaine energie)) '(eq Description Fonctionnement_materiel_energie))
(ajouter_regle '((eq Description Fonctionnement_materiel_energie) (eq Test_chargeur NIL)) '(eq Solution REMPLACEMENT_BATTERIE))
(ajouter_regle '((eq Description Fonctionnement_materiel_energie) (eq Test_chargeur T)) '(eq Solution REMPLACEMENT_CHARGEUR))
(ajouter_regle '((eq Description Fonctionnement_materiel_affichage) (eq Test_ecran NIL)) '(eq Solution MAJ_REP_CARTE_GRAPHIQUE))
(ajouter_regle '((eq Description Fonctionnement_materiel_affichage) (eq Test_ecran T)) '(eq Solution REMPLACEMENT_ECRAN))
(ajouter_regle '((eq Description Fonctionnement_materiel_peripherique) (eq Test_peripherique NIL)) '(eq Solution REMPLACEMENT_PERIPH))
(ajouter_regle '((eq Description Fonctionnement_materiel_peripherique) (eq Test_peripherique T)) '(eq Description Peripherique_ordinateur_personnel))
(ajouter_regle '((eq Description Peripherique_ordinateur_personnel) (eq Test_port NIL)) '(eq Solution REMPLACEMENT_PORT))
(ajouter_regle '((eq Description Peripherique_ordinateur_personnel) (eq Test_port T)) '(eq Solution INSTALLATION_PILOTE))
(ajouter_regle '((eq Description logiciel) (eq Partie OS)) '(eq Description logiciel_os))
(ajouter_regle '((eq Description logiciel) (eq Partie logiciel)) '(eq Description logiciel_logiciel))
(ajouter_regle '((eq Description logiciel_logiciel) (eq Touche global)) '(eq Solution SIGNALER))
(ajouter_regle '((eq Description logiciel_logiciel) (eq Touche unique)) '(eq Description logiciel_logiciel_particulier))
(ajouter_regle '((eq Description logiciel_logiciel_particulier) (eq Compatibilite depasse)) '(eq Solution LOGICIEL_ALTERNATIF))
(ajouter_regle '((eq Description logiciel_logiciel_particulier) (eq Compatibilite a_jour)) '(eq Solution MAJ_REINSTALLATION))
(ajouter_regle '((eq Description logiciel_os) (eq Demarrage possible)) '(eq Description logiciel_os_fonctionnement))
(ajouter_regle '((eq Description logiciel_os) (eq Demarrage impossible)) '(eq Description logiciel_os_demarrage))
(ajouter_regle '((eq Description logiciel_os_demarrage) (eq Nb_os 0)) '(eq Solution INSTALLATION_OS))
(ajouter_regle '((eq Description logiciel_os_demarrage) (eq Nb_os 1)) '(eq Solution INSTALLATION_BOOTLOADER))
(ajouter_regle '((eq Description logiciel_os_demarrage) (>= Nb_os 2)) '(eq Solution INSTALLATION_BOOTLOADER_MULTIPLE))
(ajouter_regle '((eq Description logiciel_os_fonctionnement) (eq Est_a_jour NIL)) '(eq Description logiciel_os_fonctionnement_maj))
(ajouter_regle '((eq Description logiciel_os_fonctionnement_maj) (eq Compatibilite depasse)) '(eq Solution ACHETER_ORDI))
(ajouter_regle '((eq Description logiciel_os_fonctionnement_maj) (eq Compatibilite a_jour)) '(eq Solution GESTION_ERREUR_INTERNE))
(ajouter_regle '((eq Description logiciel_os_fonctionnement) (eq Est_a_jour T)) '(eq Description logiciel_os_fonctionnement_general))
(ajouter_regle '((eq Description logiciel_os_fonctionnement_general) (eq Theme performance)) '(eq Description logiciel_os_fonctionnement_general_performance))
(ajouter_regle '((eq Description logiciel_os_fonctionnement_general) (eq Theme integrite)) '(eq Solution ANALYSE_ANTIVIRUS))
(ajouter_regle '((eq Description logiciel_os_fonctionnement_general_performance) (eq Test_redemarrage T)) '(eq Solution REDEMARRER_MEMOIRE))
(ajouter_regle '((eq Description logiciel_os_fonctionnement_general_performance) (eq Test_redemarrage NIL)) '(eq Solution ANALYSE_ANTIVIRUS))


;----------------------------
;     Ajout des question    |
;----------------------------

(ajouter_question 'Type "~&Votre probleme est de type MATERIEL ou LOGICIEL ? " '(member valeur '(materiel logiciel)))
(ajouter_question 'Nature "~&Votre probleme materiel est de type PERFORMANCE ou FONCTIONNEMENT ? " '(member valeur '(performance fonctionnement)))
(ajouter_question 'Sujet "~&Ce probleme diminue les performances de votre SYSTEME ou de votre connexion INTERNET ? " '(member valeur '(systeme internet)))
(ajouter_question 'Domaine "~&Le probleme concerne quel partie de votre ordinateur?~&ELEMENT_CRITIQUE ; PERIPHERIQUE ; AFFICHAGE ; ENERGIE : " '(member valeur '(element_critique peripherique affichage energie)))
(ajouter_question 'Test_chargeur "~&Votre probleme disparait-il avec un nouveau chargeur ? (Y/N) : " '(member valeur '(y n)))
(ajouter_question 'Test_ecran "~&Votre probleme disparait-il avec un nouvel ecran ? (Y/N) : " '(member valeur '(y n)))
(ajouter_question 'Test_peripherique "~&Le peripherique fonctionne-t-il sur un autre appareil ? (Y/N) : " '(member valeur '(y n)))
(ajouter_question 'Test_port "~&Un autre peripherique fonctionne-t-il sur le meme port ? (Y/N) : " '(member valeur '(y n)))
(ajouter_question 'Partie "~&Quel logiciel est touche, votre OS ou un LOGICIEL specifique ? " '(member valeur '(os logiciel)))
(ajouter_question 'Touche "~&Votre probleme est GLOBAL ou, vous est propre, UNIQUE ? " '(member valeur '(global unique)))
(ajouter_question 'Compatibilite "~&Votre logiciel est-il A_JOUR ou DEPASSE ? " '(member valeur '(a_jour depasse)))
(ajouter_question 'Demarrage "~&Le demarrage de votre ordinateur est POSSIBLE ou IMPOSSIBLE ? " '(member valeur '(possible impossible)))
(ajouter_question 'Nb_os "~&Combien d'OS possedez-vous ? " '(and (integerp valeur) (>= valeur 0)))
(ajouter_question 'Est_a_jour "~&Arrivez-vous a faire les mises a jour systeme ? (Y/N) : " '(member valeur '(y n)))
(ajouter_question 'Theme "~&Le probleme affecte : INTEGRITE ou PERFORMANCE ? " '(member valeur '(integrite performance)))
(ajouter_question 'Test_redemarrage "~&Une fois votre ordinateur redemarre le probleme disparait-il ? (Y/N) " '(member valeur '(y n)))


(defun afficher-menu ()
  ;;; Le menu principal qui permet d'initialiser la base de fait et valeur
  ;;; Puis de lancer le chainage avant ou arriere

  (funcall 'init)
  (let ((choix NIL))
    (format t "~&Voulez-vous faire un chainage avant ou arriere ? (avant/arriere) : ")
    (loop while (not (member choix '(avant arriere))) do
      (setq choix (read))
        (if (not (member choix '(avant arriere)))
          (format t "~&Veuillez entrer avant ou arriere : ")))

    (if (eq choix 'avant)
      (let ((rep nil) (nb nil))
        (format t "~&Voulez-vous completer les problemes courants ?")
        (loop while (not (OR (eq rep 'y) (eq rep 'n))) do
          (format t "~&Votre choix (Y/N): ")
          (setq rep (READ)))

        (if (eq rep 'y)
          ;;; Si l'utilisateur souhaite completer les problemes courants
          (progn
           (format t "~&Combien de problemes voulez vous ajouter a la liste des problemes courant ?")
           (loop
             (format t "~&Votre choix : ")
             (setq nb (READ))
             (when (and (numberp nb) (<= 1 nb))
               (return))
             (format t "~&Entree non valide, veuillez entrer un nombre entier superieur ou egal a 1."))))

        (if (numberp nb)
         ;;; Si l'utilisateur souhaites ajouter des problemes, numberp n'est pas nil
         (dotimes (x nb)
           (ajouter-pb-courant)))

        (chainage-avant))

      (chainage-arriere))))

(afficher-menu)