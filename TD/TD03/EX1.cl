;;; Exercice 1

(setq ll '( A 1 BB 2 CCC 3 DDD 4))

(defun afficher_1 (L)
    ;; (mapcar #'(lambda(x) (print x)) L)
    (mapcar 'print L)
)

(defun afficher_2 (L)
    (dolist (n L)
        (print n)
    )
)

(defun afficher_3 (L)
    (loop
        (if (equal (length L) 0)
            (return nil)
            ( progn
                (print (car L))
                (setq L (cdr L))
            )
        )
    )
)


(defun afficher_3_1 (L)
    (loop
        (if L
            (print (pop L))
            (return nil)
        )
    )
)

(defun afficher_4 (L)
    (if L
        (progn
          (print (car L))
          (afficher_4 (cdr L))
        )
        NIL
    )
)

(defun afficher_5 (L)
    (loop for x in L
        do (print x)
    )
)