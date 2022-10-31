;;; Exercice 1

(defun deriv_terme (term var)
    (if (equal term var)
        1 ; si plusieurs instruction (progn ....)
        0
      )
  )

(defun deriv (exp var)
    (if (atom exp)
        (deriv_terme exp var)

        (cond
            ((equal (car exp) '*)
                (deriv_mult exp var)
            )

            ((equal (car exp) '/)
                (deriv_div exp var)
            )

            ((OR (equal (car exp) '-) (equal (car exp) '+))
                (deriv_add_minus exp var)

            )

            ((equal (car exp) 'exp)
                (deriv_exp exp var)

            )

            ((equal (car exp) 'ln)
                (deriv_ln exp var)

            )

            ((equal (car exp) 'cos)
                (deriv_cos exp var)

            )

            ((equal (car exp) 'sin)
                (deriv_sin exp var)

            )

            ((equal (car exp) 'tan)
                (deriv_tan exp var)

            )
        )
    )
)

(defun deriv_add_minus (exp var)
    (list (car exp) (deriv (cadr exp) var) (deriv (caddr exp) var))
  )


(defun deriv_mult (exp var)
    (list '+ (list '* (deriv (cadr exp) var) (caddr exp)) (list '* (cadr exp) (deriv (caddr exp) var)))
)


(defun deriv_div (exp var)
  (list '/
      (list '- (list '* (deriv (cadr exp) var) (caddr exp)) (list '* (cadr exp) (deriv (caddr exp) var)))

        (list '* (caddr exp) (caddr exp))
  )
)

(defun deriv_exp (exp var)
    (list '* (deriv (cadr exp) var) exp)
)

(defun deriv_ln (exp var)
    (list '/ (deriv (cadr exp) var) (cadr exp))
)


(defun deriv_cos (exp var)
    (list '* (list '- 0 (deriv (cadr exp) var)) (list 'sin (cadr exp)))
)

(defun deriv_sin (exp var)
    (list '* (deriv (cadr exp) var) (list 'cos (cadr exp)))
)


(defun deriv_tan (exp var)
    (list '/ 1 (list '* (list 'cos (cadr exp)) (list 'cos (cadr exp))))
)

(defun simplify_add (exp)
    (if (AND (numberp (cadr exp)) (numberp (caddr exp)))
        (EVAL exp)
        NIL
    )
)
