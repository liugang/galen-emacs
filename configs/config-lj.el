;;; config-lj.el ---

;; Copyright (C) 2010 Galen
;;
;; Author: galen.gang.liu@gmail.com
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(add-to-list 'load-path "~/emacs/ljupdate/")
(require 'ljupdate)

(add-hook 'lj-compose-common-hook
          (lambda ()
            (auto-fill-mode nil)
            ))


(custom-set-variables
 '(lj-cache-login-information t)
 '(lj-default-username "galen.gang.liu")
 )

;;; config-lj.el ends here
