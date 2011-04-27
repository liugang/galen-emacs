;;; config-jdee.el ---

;; Copyright (C) 2010 Galen
;;
;; Author: galen.gang.liu@gmail.com
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

;; For JDE
(add-to-list 'load-path "/home/ott/emacs/jde/lisp")
(require 'jde)
(add-to-list 'load-path "/home/ott/emacs/jde-docindex/lisp")
                                        ;(require 'jde-docindex)

(require 'generic-x)
(add-to-list 'auto-mode-alist
             '("\\.properties$" . java-properties-generic-mode))

;;; config-jdee.el ends here
