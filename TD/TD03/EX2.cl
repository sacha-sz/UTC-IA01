(setq *html*
    '(html
        (header
            (title "ma page")
        )

        (body
            (h1 "un titre")
            (p "Sorror et aemula Romae")
        )
    )
)


;; A savoir sur fomat :

(format t "~V@t<~s>" i x)
;; ~s -> string
;; ~a -> string sans les  ""
;; ~t -> tabulation
;; ~5@t -> 5 tabulation
;; ~V@t -> V tabulation
;; ~& -> retour à la ligne



; Algorithme de make_html :

;;(defun make_html (L, i)
;;    Si L est une liste
;;        Affichage de la balise ouvrante
;;
;;            Boucle qui appelle récursivement
;;            (make_html x (+i 3))
;;
;;        Affichage de la balise fermante
;;
;;    Sinon
;;
;;        Affichage L
;;)

(defun make_html (L i)
    (if (listp L)
        (progn
            (format t "~&~V@t<~s>~&" i (car L))

            (dolist (x (cdr L))
                (make_html x (+ i 3))
            )

            (format t "~V@t</~s>~&" i (car L))
        )

        (format t "~V@t~a~&" i L)
    )
)

(make_html *html* 0)

;;; Modification du make_html
(defun make_html_file (L i file)
  (if (listp L)
      (progn
          (format file "~&~V@t<~s>~&" i (car L))

          (dolist (x (cdr L))
                (make_html_file x (+ i 3) file)
          )

         (format file "~V@t</~s>~&" i (car L))
       )

       (format file "~V@t~a~&" i L)
  )
)



;;; Ecriture dans un fichier
(defun write_html_file (L file)
  (with-open-file (file file
                        :direction :output
                        :if-exists :overwrite
                        :if-does-not-exist :create)
    (make_html_file L 0 file)
  )
)

(write_html_file *html* "test.html")
