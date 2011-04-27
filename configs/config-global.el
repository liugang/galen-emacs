;;; config-highlight-symbol.el ---

;; Copyright (C) 2010 Galen
;;
;; Author: galen.gang.liu@gmail.com
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

;; for global
(load-file "~/.emacs.d/plugins/gtags.el")

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
         ))

