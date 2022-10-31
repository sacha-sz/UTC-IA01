;;; Exercice 2

(B A (D C E)) ; infixe
(A B (C D E)) ; prefixe
(B (D E C) A) ; postfixe

(defun prefixe (l)
  (if (atom l)
      (list l)
      (cons (prefixe (cadr l)) (prefixe (car l)) (prefixe (caddr l)))
  )
)