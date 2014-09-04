(require 'linked-buffer)
(require 'linked-buffer-wing
         (concat default-directory "emacs/linked-buffer-wing.el"))
(require 'commander)

;; from linked-buffer -- I can't get cask link workign!
(defun linked-buffer-batch-clone-and-save-with-config (filename init)
  "Open FILENAME, set INIT function, then clone and save.

Returns the config object. This function does potentially evil
things if the file or the linked-buffer is open already."
  (let ((retn))
    (with-current-buffer
        (find-file-noselect filename)
      (setq linked-buffer-init init)
      (with-current-buffer
          (linked-buffer-init-create)
        (save-buffer)
        (kill-buffer))
      (setq retn linked-buffer-config)
      (kill-buffer))
    retn))

(defun gensource-and-report (file init)
  (message "Cloning %s..."
           file)
  (let ((config
         (linked-buffer-batch-clone-and-save-with-config
          file init)))
    (message "Cloning %s...done" file)
    (message "For %s generated %s."
             file
             (oref config :linked-file))))

(defun gensource-gen-if-necessary (file)
  (let* ((target
          (linked-buffer-wing-clj-for-asciidoc
           file))
         (locked
          (or (file-locked-p file)
              (file-locked-p target))))
    (if locked
        (message "Skiping %s due to lock %s" file locked)
      (when (file-newer-than-file-p file target)
        (gensource-and-report file 'linked-buffer-wing-init)))))

(defun gen/generate ()
  (mapc 'gensource-gen-if-necessary
        (directory-files "./asciidoc" t "^[a-z].*.adoc$")))

(commander
 (command "generate" "Generate Clojure from Asciidoc" gen/generate))
