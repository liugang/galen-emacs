;;; config-common-hooks.el ---

;; Copyright (C) Galen
;;
;; Author: Galen <galen.gang.liu@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

;; common settings for different text & programming modes
(defun galen.gang.liu/common-hook ()
  (local-set-key "\C-c:" 'uncomment-region)
  (local-set-key "\C-c;" 'comment-region)
  (local-set-key "\C-c\C-c" 'comment-region)
  )

;; show FIXME/TODO/BUG keywords
(defun galen.gang.liu/show-prog-keywords ()
  ;; highlight additional keywords
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil '(("\\<\\(DONE\\):" 1 font-lock-doc-face t)))
  ;; highlight too long lines
  (font-lock-add-keywords nil '(("^[^\n]\\{120\\}\\(.*\\)$" 1 font-lock-warning-face t))))


;; clean trailing whitespaces automatically
(setq galen.gang.liu/trailing-whitespace-modes '(c++-mode c-mode haskell-mode emacs-lisp-mode
                                                   lisp-mode scheme-mode erlang-mode))

(defun galen.gang.liu/trailing-whitespace-hook ()
  (when (member major-mode galen.gang.liu/trailing-whitespace-modes)
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'galen.gang.liu/trailing-whitespace-hook)

;; untabify some modes
(setq galen.gang.liu/untabify-modes '(haskell-mode emacs-lisp-mode lisp-mode scheme-mode
                                            erlang-mode clojure-mode))
(defun galen.gang.liu/untabify-hook ()
  (when (member major-mode galen.gang.liu/untabify-modes)
    (untabify (point-min) (point-max))))
(add-hook 'before-save-hook 'galen.gang.liu/untabify-hook)


;;; config-common-hooks.el ends here
