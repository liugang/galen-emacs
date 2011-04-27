;;; config-gdb.el --- gdb/gud settings

;; Copyright (C) Galen
;;
;; Author: Galen <galen.gang.liu@gmail.com>
;; Keywords: gdb
;; Status: not intended to be distributed yet

(setq gdb-many-windows t)
(setq gdb-show-main t)
(setq gud-chdir-before-run nil)
(setq gud-tooltip-mode t)

(add-hook 'gdb-mode-hook '(lambda ()
                            ; c-mode-base-map
                            (define-key c-mode-base-map [(f5)] 'gud-go)
                            (define-key c-mode-base-map [(f7)] 'gud-step)
                            (define-key c-mode-base-map [(f8)] 'gud-next)
                            (local-set-key [(f5)] 'gud-go)
                            (local-set-key [(f7)] 'gud-step)
                            (local-set-key [(f8)] 'gud-next)
                            ))
;(global-set-key [(f5)] 'gud-go)

;;; config-gdb.el ends here
