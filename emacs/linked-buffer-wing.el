(require 'f)
(require 'linked-buffer-asciidoc)

(defun linked-buffer-wing-clj-for-asciidoc (filename)
  (f-join
   (f-dirname
    (f-dirname filename))
   "src" "take" "wing"
   (concat (f-no-ext
            (f-filename
             filename))
           ".clj")))

(defun linked-buffer-wing-init()
  (setq linked-buffer-config
        (linked-buffer-asciidoc-uncommented-new))
  ;; reset the linked-file to the right place
  (oset linked-buffer-config
        :linked-file (linked-buffer-wing-clj-for-asciidoc (buffer-file-name)))
  ;; add tawny as a source tag to treat as source
  (object-add-to-list
   linked-buffer-config
   :srctags "tawny")
  linked-buffer-config)

(add-to-list 'linked-buffer-init-functions
             'linked-buffer-wing-init)

(provide 'linked-buffer-wing)
