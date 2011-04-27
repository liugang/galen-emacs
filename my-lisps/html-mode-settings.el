;; -*- Emacs-Lisp -*-

;; Time-stamp: <2011-03-20 12:47:35 Sunday by taoshanwen>

(eal-define-keys
 'html-mode-map
 `(("C-c C-w" w3m-browse-current-buffer)
   ("C-c M-a" add-target-to-link-sb)))

(defun html-mode-settings ()
  "settings for `html-mode'."
  (defun add-target-to-link ()
    "Add \"target=\"_blank\" to link."
    (interactive)
    (query-replace-regexp "<a\\s-+href=\\(\"[^\"#][^\"]*?\"\\)>\\(.*?\\)</a>" "<a href=\\1 target=\"_blank\">\\2</a>"))

  (defun w3m-browse-current-buffer ()
    "Use w3m browser current buffer."
    (interactive)
    (w3m-browse-buffer)))

(eval-after-load "sgml-mode"
  `(html-mode-settings))

(provide 'html-mode-settings)
