;;; config-ess.el ---

;; Copyright (C) Galen
;;
;; Author: Galen <galen.gang.liu@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(add-to-list 'load-path "~/emacs/ess")
(require 'ess-site)

;; R doc
(add-to-list 'auto-mode-alist '("\\.rd\\'" . Rd-mode))
(add-hook 'Rd-mode-hook (lambda ()
                          (abbrev-mode 1)
                          (font-lock-mode 1)))

;;; config-ess.el ends here
