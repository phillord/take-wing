(require 'linked-buffer)
(require 'linked-buffer-wing
         (concat default-directory "emacs/linked-buffer-wing.el"))
(require 'commander)
(require 'wing-config
         (concat default-directory "script/wing-config.el"))
(require 'org-publish)
(require 'ox-latex)
(require 'clojure-mode)
(require 'htmlize)
(require 'ox-html)






;; toggle to stack trace
(setq debug-on-error t)

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

(defvar build-source-file "org/take_wing.org")


;;(setq org-publish-project-alist nil)
;; this is defined in linked-buffer-wing.el

(setq org-export-allow-BIND t)
(setq org-latex-listings t)
(setq org-latex-custom-lang-environments
      '((clojure "tawny")))



(defun init-faces ()
  (custom-set-faces
   '(default                      ((t (:foreground "#ffffff" :background "black"))))
   '(font-lock-builtin-face       ((t (:foreground "#ff0000"))))
   '(font-lock-comment-face       ((t (:bold t :foreground "#333300"))))
   '(font-lock-constant-face      ((t (:foreground "magenta"))))
   '(font-lock-function-name-face ((t (:bold t :foreground "Blue"))))
   '(font-lock-keyword-face       ((t (:foreground "yellow3"))))
   '(font-lock-string-face        ((t (:foreground "light blue"))))
   '(font-lock-type-face          ((t (:foreground "green"))))
   '(font-lock-variable-name-face ((t (:foreground "cyan" :bold t))))
   '(font-lock-warning-face       ((t (:foreground "red" :weight bold))))))

(defun build/publish ()
  (init-faces)
  (with-current-buffer
      (find-file-noselect build-source-file)
    (org-publish-project "take-wing" t)))

(defun build/latex ()
  (with-current-buffer
      (find-file-noselect build-source-file)
    (let ((proj
           ;; project is nearly a plist but not quite
           (cons
            "take-wing"
            (plist-put
             (cdr
              take-wing-project)
             :publishing-function '(org-latex-publish-to-latex)))))
      (org-publish-project proj t))))

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
          (linked-buffer-wing-clj-for-org
           file))
         (locked
          (or (file-locked-p file)
              (file-locked-p target))))
    (if locked
        (message "Skiping %s due to lock %s" file locked)
      (when (file-newer-than-file-p file target)
        (gensource-and-report file 'linked-buffer-wing-init)))))

(defun build/gen-src ()
  (mapc 'gensource-gen-if-necessary
        (directory-files "./org" t "^[a-z].*.org$")))

(commander
 (command "gen-src" "Generate Clojure from Org" build/gen-src)
 (command "latex" "Generate latex source" build/latex)
 (command "publish" "Generate all files" build/publish)
 (command "html" "Generate html" build/html)
 (command "pdf" "Generate pdf" build/pdf))
