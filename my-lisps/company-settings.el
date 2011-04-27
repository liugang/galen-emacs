;; -*- Emacs-Lisp -*-

;; Time-stamp: <2011-04-21 22:18:43 Thursday by liugang>

(require 'company)

(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)
;;(setq company-begin-commands '(self-insert-command))
(define-key company-mode-map (kbd "M-RET") 'company-expand-top)

(am-add-hooks
 `(c-mode-common-hook lisp-mode-hook emacs-lisp-mode-hook
                      java-mode-hook lisp-interaction-mode-hook sh-mode-hook
                      ,(if (not is-before-emacs-21) 'awk-mode-hook)
                      ruby-mode-hook)
 'company-mode)

(provide 'company-settings)
