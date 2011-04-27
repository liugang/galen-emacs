;;; config-jump-dls.el ---

;; Copyright (C) 2010 Galen
;;
;; Author: galen.gang.liu@gmail.com
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet


;; http://www.emacswiki.org/emacs/jump-dls.el
(load-file "~/.emacs.d/plugins/jump-dls.el")
(require 'jump)
(global-set-key [f4] 'jump-symbol-at-point)
(global-set-key [(shift f4)] 'jump-back)
