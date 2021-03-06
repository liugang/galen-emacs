;;; config-mailcrypt.el --- Settings for mailcrypt

;; Copyright (C) Galen
;;
;; Author: Galen <galen.gang.liu@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet


(add-to-list 'load-path "~/emacs/mailcrypt")
(require 'mailcrypt)
(mc-setversion "gpg")
(add-hook 'gnus-summary-mode-hook 'mc-install-read-mode)
(add-hook 'message-mode-hook 'mc-install-write-mode)
(add-hook 'news-reply-mode-hook 'mc-install-write-mode)
(setq mc-gpg-user-id "Galen <galen.gang.liu@gmail.com>")



;;; config-mailcrypt.el ends here
