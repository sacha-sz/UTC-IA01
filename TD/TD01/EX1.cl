;;; Exercice 1

23 ; nombre retourne sa valeur

(quote 23) ; retourne la valeur de l'element

'23 ; est equivalent au precedent

(set x 32) ; erreur car cherche a evaluer x

(setq x 32) ; retourne 32 et x vaut maintenant 32 (affectation)

(list x x 32) ; x vaut 32 donc retourne la liste (32 32 32)

(cadr (list x x 32)) ; retourne le deuxieme element de la liste (32 32 32) soit 32

(setq x 'y) ; x vaut maintenant le caractere y

(setq xx 5) ; xx vaut maintenant 5 car setq empeche l'evaluation de xx

(setq y '(+ xx xx 32)) ; affecte a y la liste (+ xx xx 32)

x ; retourne la valeur de x, donc y qui vaut (+ xx xx 32)

(eval x) ; x vaut la valeur contenue en y

(eval y) ; va executer les instructions (+ 5 5 32) et retourner 42

(cadr y) ; retourne le deuxieme element de y (+ xx xx 32) soit le premier xx

(eval (list '+ (eval y)(cadr y)))
; evalue la liste (+ 42 5) car eval y retourne 42 et cadr y retourne xx qui vaut 5

(setq z (+ (if x 2 0) (caddr y) (* y y)))
; erreur car :
;    (if x 2 0) : x etant defini retourne 2
;    (caddr y) : retourne xx
;    (* y y) : erreur y n'est pas un nombre on ne peut pas le multiplier

(setq y (list (setq z "Albert") x y))
; y vaut ("Albert" y (+ xx xx 32) et z vaut "Albert

z ; "Albert"

y ; ("Albert" Y ("Albert" Y (+ XX XX 32))) y etant evalue retourne la liste ("Albert" Y (+ XX XX 32))

(setq x (* x x)) ; erreur car (* x x) est evaluer en (* y y) meme erreur que plus haut

x ; Y