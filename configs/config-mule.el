;;; config-mule.el ---

;; Copyright (C) 2005 Galen
;;
;; Author: galen.gang.liu@gmail.com
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

;; (set-language-environment 'Cyrillic-KOI8)
;; (set-selection-coding-system 'cyrillic-koi8)

(custom-set-variables
 '(current-language-environment "UTF-8")
 '(default-input-method "russian-computer")
 '(x-select-enable-clipboard t)
 '(interprogram-paste-function (quote x-cut-buffer-or-selection-value))
)

(set-input-method "russian-computer" nil)
(setenv "LANG" "ru_RU.UTF-8")

;;; config-mule.el ends here
