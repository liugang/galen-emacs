;;; config-wikis.el ---

;; Copyright (C) Galen
;;
;; Author: Galen <galen.gang.liu@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

;; moin-moin editing
(require 'moinmoin-mode)
(add-hook 'outline-mode-hook 'turn-on-flyspell)
;;(defun galen.gang.liu/outline-mode-hook ()
;;   (longlines-mode +1)
;;  )
;;(add-hook 'outline-mode-hook 'galen.gang.liu/outline-mode-hook)

;; dokuwiki
(global-unset-key [(control v)])
(require 'dokuwiki)
(add-to-list 'auto-mode-alist '("\\.doku?$" . simple-dokuwiki-mode))
(add-hook 'simple-dokuwiki-mode-hook 'turn-on-flyspell)
(custom-set-faces
 '(simple-wiki-teletype-face ((((class color) (background light)) (:underline "darkgreen")))))

(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
;;(add-to-list 'auto-mode-alist '("\\.te?xt$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown?$" . markdown-mode))


;;; config-wikis.el ends here
