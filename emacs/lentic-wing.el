(require 'f)
(require 'lentic-latex-code)


(defun lentic-wing-clj-for-tex (filename)
  (f-join
   (f-dirname
    (f-dirname filename))
   "src" "take" "wing"
   (concat (f-no-ext
            (f-filename
             filename))
           ".clj")))

(defvar lentic-wing-file
  (locate-library "lentic-wing.el"))

(defun lentic-wing-init ()
  (lentic-uncommented-chunk-configuration
   "lb-commented tawny latex"
   :this-buffer (current-buffer)
   :lentic-file
   (lentic-wing-clj-for-tex (buffer-file-name))
   :comment ";; "
   :comment-start "\\\\end{\\(tawny\\\|tawnyhidden\\)}"
   :comment-stop "\\\\begin{\\(tawny\\\|tawnyhidden\\)}"))

(add-to-list 'lentic-init-functions
             'lentic-wing-init)

(mapcar
 (lambda (env)
   (if (boundp 'LaTeX-indent-environment-list)
       (add-to-list 'LaTeX-indent-environment-list
                    `(,env current-indentation)))

   (if (boundp 'LaTeX-verbatim-environments)
       (add-to-list 'LaTeX-verbatim-environments
                    "env")))
 '("tawny" "tawnyhidden" "tawnyexample"))


(provide 'lentic-wing)
