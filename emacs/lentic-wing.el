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
   :comment-start "\\\\end{tawny}"
   :comment-stop "\\\\begin{tawny}"))

(add-to-list 'lentic-init-functions
             'lentic-wing-init)

(if (boundp 'LaTeX-indent-environment-list)
    (add-to-list 'LaTeX-indent-environment-list
                 '("tawny" current-indentation)))

(if (boundp 'LaTeX-verbatim-environments)
    (add-to-list 'LaTeX-verbatim-environments
                 "tawny"))

(provide 'lentic-wing)
