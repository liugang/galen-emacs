;; for global
;(load-file "~/.emacs.d/plugins/gtags.el")

(require 'gtags)

(galen-func/add-hooks
 `(c-mode-common-hook
;   lisp-mode-hook
;   emacs-lisp-mode-hook
   java-mode-hook
;   lisp-interaction-mode-hook
;   sh-mode-hook
;   awk-mode-hook
;   ruby-mode-hook
   )
 'gtags-mode)


(defun galen-func/gtags-settings()
  "Setting for `gtags'."
  (setq gtags-mode-hook
        '(lambda ()
           (define-key gtags-mode-map "\C-csc" 'gtags-find-rtag)
           (define-key gtags-mode-map "\C-csd" 'gtags-find-tag)
           (define-key gtags-mode-map "\C-csf" 'gtags-find-file)
           (define-key gtags-mode-map "\C-csg" 'gtags-find-with-grep)
           (define-key gtags-mode-map "\C-css" 'gtags-find-symbol)
           (define-key gtags-mode-map "\C-cst" 'gtags-pop-stack)
           (define-key gtags-mode-map "\C-csb" 'gtags-display-browser)
           (define-key gtags-mode-map "\C-csh" 'gtags-find-tag-from-here)
           (define-key gtags-mode-map "\C-csr" 'gtags-visit-rootdir)
           ;; (setq gtags-pop-delete t)
           (setq gttags-path-style 'relative)
           ;; (setq hl-line-face 'underline)
           ;; (setq gtags-select-buffer-single t)
           )))

(eval-after-load "gtags"
  `(galen-func/gtags-settings))

(provide 'galen-gtags-settings)

