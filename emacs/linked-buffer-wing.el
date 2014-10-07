(require 'f)
(require 'linked-buffer-org)


(defun linked-buffer-wing-clj-for-tex (filename)
  (f-join
   (f-dirname
    (f-dirname filename))
   "src" "take" "wing"
   (concat (f-no-ext
            (f-filename
             filename))
           ".clj")))

(defvar linked-buffer-wing-file
  (locate-library "linked-buffer-wing.el"))

(defun linked-buffer-wing-init ()
  (setq linked-buffer-config
        (linked-buffer-uncommented-block-configuration
         "lb-commented tawny latex"
         :this-buffer (current-buffer)
         :linked-file
         (linked-buffer-wing-clj-for-tex (buffer-file-name))
         :comment ";; "
         :comment-start "\\\\end{tawny}"
         :comment-stop "\\\\begin{tawny}")))

(add-to-list 'linked-buffer-init-functions
             'linked-buffer-wing-init)

(add-to-list 'LaTeX-indent-environment-list
             '("tawny" current-indentation))
(provide 'linked-buffer-wing)
