(require 'org-publish)

(defvar take-wing-project
  `("take-wing"
    :base-directory ,(concat linked-buffer-wing-file "../org")
    :publishing-directory ,(concat linked-buffer-wing-file "../exports")
    :exclude ".*org"
    :include ("take_wing.org")
    :publishing-function
    (org-html-publish-to-html
     org-latex-publish-to-latex
     ;;org-latex-publish-to-pdf
     )))

;;(setq org-publish-project-alist nil)
(add-to-list
 'org-publish-project-alist
 take-wing-project)

(provide 'wing-config)
