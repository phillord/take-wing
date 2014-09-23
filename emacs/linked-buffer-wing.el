(require 'f)
(require 'linked-buffer-org)


(defvar linked-buffer-wing-file
  (locate-library "linked-buffer-wing.el"))

(defun linked-buffer-wing-init()
  (setq linked-buffer-config
        (linked-buffer-org-to-clojure-new))
  ;; reset the linked-file to the right place
  (oset linked-buffer-config
        :linked-file (linked-buffer-wing-clj-for-org (buffer-file-name)))
  ;; add tawny as a source tag to treat as source
  linked-buffer-config)

(add-to-list 'linked-buffer-init-functions
             'linked-buffer-wing-init)

(provide 'linked-buffer-wing)
