;;; config-tramp.el --- 

;; Copyright (C) 2010 Galen
;;
;; Author: galen.gang.liu@gmail.com
;; Keywords: 
;; Requirements: 
;; Status: not intended to be distributed yet

(require 'tramp)

(setq tramp-default-method "ssh")
(add-to-list 'tramp-default-method-alist
	     '("\\`localhost\\'" "\\`root\\'" "su"))
(add-to-list 'tramp-default-method-alist '("" "ott" "ssh"))
(add-to-list 'tramp-default-method-alist '("" "root" "ssh"))


(tramp-set-completion-function "ssh"
			       '((tramp-parse-sconfig "/etc/ssh_config")
				 (tramp-parse-sconfig "~/.ssh/config")))

;;; config-tramp.el ends here
