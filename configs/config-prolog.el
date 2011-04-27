;;; config-prolog.el ---

;; Copyright (C) Galen
;;
;; Author: Galen <galen.gang.liu@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)

(add-hook 'prolog-mode-hook 'galen.gang.liu/common-hook)
(add-hook 'prolog-mode-hook 'galen.gang.liu/show-prog-keywords)

(add-to-list 'auto-mode-alist '("\\.plg$" . prolog-mode))

;;  (turn-on-eldoc-mode)

;;; config-prolog.el ends here
