(setq bdr '(
    (F (B D E) R1)
    (A (D G) R2)
    (A (C F) R3)
    (D (C) R4)
    (E (D) R5)
    (H (A) R6)
    (X (B) R7)
    (A (X C) R8)
    )
)


(defun ccl (regle)
    (car regle)
)

(defun premiere_regle (regle)
    (cadr regle)
)

(defun num_regle (regle)
    (caddr regle)
)


(defun regles_candidates (but bdr)
    (if bdr
        (if (equal (ccl (car bdr)) but)
            (cons (car bdr) (regles_candidates but (cdr bdr)))
            (regles_candidates but (cdr bdr))
        )
    )
)


;; Non fait en TD
(defun chainage_arriere  (but bdf bdr &optional(i 0))
    (if (member but bdf)
        (progn
            (format t "~v@t    But : ~A proof ~%" i but)
        T)

        (progn

            (let ((regles (regles_candidates but bdr)) (sol nil))

                (while (and regles (not sol))

                    (format t "~V@t VERIFIE_OU ~A Regles ~s : ~A ~%" i but (num_regle (car regles)) (car regles))

                    (let ((premisses (premiere_regle (car regles))))
                        (setq sol T)
                        (while (and premisses sol)
                            (format t "~V@t VERIFIE_ET ~A~%" (+ i 1) (car premisses))
                            (setq sol (chainage_arriere (pop premisses) bdf bdr (+ 9 i)))
                            (if sol
                                (push (num_regle (car regles)) sol)
                            )

                        )
                        (pop regles)

                    )
                sol

                )

            )

        )
    )
  )

;; (chainage_arriere 'H '(B C) bdr)