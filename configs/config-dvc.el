;;; config-dvc.el ---

;; Copyright (C) 2007 Galen
;;
;; Author: galen.gang.liu@gmail.com
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(add-to-list 'load-path "~/emacs/dvc/")
(require 'dvc-autoloads)

(custom-set-variables
 '(dvc-prompt-active-dvc nil)
 '(dvc-select-priority '(xgit xhg bzr baz))
 '(dvc-tips-enabled nil)
 )

;;; config-dvc.el ends here
