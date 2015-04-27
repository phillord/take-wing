(require 'lentic)
(require 'lentic-wing
         (concat default-directory "emacs/lentic-wing.el"))
(require 'commander)


;; toggle to stack trace
(setq debug-on-error t)

;; from lentic -- I can't get cask link workign!
(defun gensource-and-report (file init)
  (message "Cloning %s..."
           file)
  (let ((config
         (car
          (lentic-batch-clone-and-save-with-config
           file init))))
    (message "Cloning %s...done" file)
    (message "For %s generated %s."
             file
             (oref config :lentic-file))))

(defun gensource-gen-if-necessary (file)
  (let* ((target
          (lentic-wing-clj-for-tex
           file))
         (locked
          (or (file-locked-p file)
              (file-locked-p target))))
    (if locked
        (message "Skiping %s due to lock %s" file locked)
      (when (file-newer-than-file-p file target)
        (gensource-and-report file 'lentic-wing-init)))))

(defun build/gen-src ()
  (mapc 'gensource-gen-if-necessary
        (directory-files "./latex" t "^[a-z].*.tex$")))

(commander
 (command "gen-src" "Generate Clojure from Org" build/gen-src))
