;; -*- lexical-binding: t -*-
(require 'm-buffer)


(defun select-stop-for-start (starts stops)
  (-map
   (lambda (start)
     (setq stops
           (-drop-while
            (lambda (stop)
              (< stop start))
            stops))
     (let ((r
            (car stops)))
       (setq stops (cdr stops))
       r))
   starts))


(defun fix-entities (file-name)
  (find-file file-name)
  (let* ((starts
          (m-buffer-match-end
           (m-buffer-match (current-buffer)
                           (regexp-quote "<code class=\"language-tawny\">"))))
        (stops
         (m-buffer-match-begin
          (m-buffer-match (current-buffer)
                          (regexp-quote "</code>"))))
        (real-stops
         (select-stop-for-start starts stops)))
    (-map
     (lambda (replacements)
       (-map
        (lambda (pair)
          (m-buffer-replace-match
           (m-buffer-match
            (current-buffer)
            (nth 0 replacements)
            :begin (car pair)
            :end (cdr pair))
           (nth 1 replacements)))
        (-zip starts real-stops)))
     '(("<" "&lt;")
       (">" "&gt;")
       )))
  (save-buffer))

;; make this more parameterisable later?
(fix-entities "./latex/take_wing.html")
