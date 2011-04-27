(require 'company)

(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)


;; 按 alt + enter 补全
(setq company-begin-commands nil)
(define-key company-mode-map (kbd "M-RET") 'company-complete)

;; 按键后，马上弹出补全菜单
;;(setq company-begin-commands '(self-insert-command))

(galen-func/add-hooks
 `(c-mode-common-hook
   lisp-mode-hook
   emacs-lisp-mode-hook
   java-mode-hook
   lisp-interaction-mode-hook
   sh-mode-hook
   awk-mode-hook
   ruby-mode-hook)
 'company-mode)

(provide 'galen-company-settings)
