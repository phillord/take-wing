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
  (oset linked-buffer-config
        :linked-file (linked-buffer-wing-clj-for-asciidoc (buffer-file-name)))
  linked-buffer-config)

(provide 'linked-buffer-wing)
