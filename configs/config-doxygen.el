;;; config-doxygen.el ---

;; Copyright (C) 2010 Galen
;;
;; Author: galen.gang.liu@gmail.com
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

;; (require 'doc-mode)
;; (add-hook 'c-mode-common-hook 'doc-mode)

(require 'tempo)
(require 'doxymacs)
(add-hook 'c-mode-common-hook 'doxymacs-mode)

(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode)
       (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

(setq-default doxymacs-doxygen-style "JavaDoc")

;;; config-doxygen.el ends here
