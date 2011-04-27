;;; config-yasnippet.el ---

;; Copyright (C) Galen
;;
;; Author: Galen <galen.gang.liu@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(setq galen.gang.liu/yasnippet-dir "~/emacs/yasnippet/snippets")
(yas/load-directory galen.gang.liu/yasnippet-dir)

;; hook for automatic reloading of changed snippets
(defun galen.gang.liu/update-yasnippets-on-save ()
  (when (string-match "/yasnippet/snippets" buffer-file-name)
    (yas/load-directory galen.gang.liu/yasnippet-dir)))
(add-hook 'after-save-hook 'galen.gang.liu/update-yasnippets-on-save)

;;; config-yasnippet.el ends here
