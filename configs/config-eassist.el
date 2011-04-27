;;; config-highlight-symbol.el ---

;; Copyright (C) 2010 Galen
;;
;; Author: galen.gang.liu@gmail.com
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(load-file "~/.emacs.d/plugins/cedet/contrib/eassist.el")

(defun my-c-mode-common-hook ()
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
  (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
  (define-key c-mode-base-map (kbd "M-p") 'eassist-list-methods))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-python-mode-hook ()
  (define-key python-mode-map (kbd "M-p") 'eassist-list-methods))
(add-hook 'python-mode-hook 'my-python-mode-hook)

(define-key lisp-mode-shared-map (kbd "M-p") 'eassist-list-methods)

