;;; config-lisp.el ---

;; Copyright (C) 2004 Galen
;;
;; Author: galen.gang.liu@gmail.com
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(defun galen.gang.liu/lisp-mode-hook ()
  ;;       (setq tab-width 4)
  (setq indent-tabs-mode t)
  (abbrev-mode 1)
  (auto-fill-mode 1)
  (turn-on-eldoc-mode)
  (paredit-mode 1)
  (local-set-key [return] 'newline-and-indent)
  (set (make-local-variable 'slime-lisp-implementations)
       (list (assoc 'sbcl slime-lisp-implementations)))
  )
(add-hook 'lisp-mode-hook 'galen.gang.liu/lisp-mode-hook)
(add-hook 'lisp-mode-hook 'galen.gang.liu/common-hook)
(add-hook 'lisp-mode-hook 'galen.gang.liu/show-prog-keywords)

(defun galen.gang.liu/lisp-interact-mode-hook ()
  (paredit-mode 1)
  )
(add-hook 'lisp-interaction-mode-hook 'galen.gang.liu/lisp-interact-mode-hook)

;; lookup information in hyperspec
(require 'info-look)
(info-lookup-add-help
 :mode 'lisp-mode
 :regexp "[^][()'\" \t\n]+"
 :ignore-case t
 :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))

;;; config-slime.el ends here
