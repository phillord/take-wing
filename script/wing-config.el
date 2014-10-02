(require 'org-publish)

(defvar wing-config-dir
  (if (locate-library "wing-config.el") 
      (concat "../"
              (file-name-directory (locate-library "wing-config.el")))
    default-directory))

(defvar take-wing-project
  `("take-wing"
    :base-directory ,(concat wing-config-dir "/org")
    :publishing-directory ,(concat wing-config-dir "/exports")
    :exclude ".*org"
    :include ("take_wing.org")
    :publishing-function
    (org-html-publish-to-html
     org-latex-publish-to-latex)))

;;(setq org-publish-project-alist nil)
(add-to-list
 'org-publish-project-alist
 take-wing-project)

(provide 'wing-config)
