;;; config-jabber.el ---

;; Copyright (C) 2004 Galen
;;
;; Author: galen.gang.liu@gmail.com
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(add-to-list 'load-path "~/emacs/emacs-jabber")
(require 'jabber)
(require 'jabber-bookmarks)

(defun galen.gang.liu/jabber-connect-hook (jc)
  (jabber-send-presence "" "I'm online" 10)
  (let* ((state-data (fsm-get-state-data jc))
         (server (plist-get state-data :server)))
    (message "%s" server)
    (if (string-equal server "gmail.com")
        (progn
          (jabber-groupchat-join jc "devil@conference.jabber.ru" "galen.gang.liu")
          (jabber-groupchat-join jc "haskell@conference.jabber.ru" "galen.gang.liu")
;;          (jabber-groupchat-join jc "lisp@conference.jabber.ru" "galen.gang.liu")
;;          (jabber-groupchat-join jc "emacs@conference.jabber.ru" "galen.gang.liu")
;;          (jabber-groupchat-join jc "icfpc@conference.jabber.ru" "galen.gang.liu")
;;          (jabber-groupchat-join jc "wax@conference.jabber.ru" "galen.gang.liu")
;;          (jabber-groupchat-join jc "erlang@conference.jabber.ru" "galen.gang.liu")
          ))))
(add-hook 'jabber-post-connect-hooks 'galen.gang.liu/jabber-connect-hook)

(defun galen.gang.liu/jabber-chat-hook ()
  (auto-fill-mode -1))
(add-hook 'jabber-chat-mode-hook 'galen.gang.liu/jabber-chat-hook)

(setq jabber-history-enabled t)
(setq jabber-use-global-history nil)

(require 'jabber-autoaway)
(add-hook 'jabber-post-connect-hook 'jabber-autoaway-start)

(setq jabber-alert-info-message-hooks (quote (jabber-info-echo)))
(setq jabber-alert-message-hooks (quote (jabber-message-beep jabber-message-scroll)))
(setq jabber-alert-presence-hooks (quote (jabber-presence-update-roster)))
(setq jabber-nickname "galen.gang.liu")
(setq jabber-resource (concat "at-"
                              (if (string-equal (system-name) "galen.gang.liu")
                                  "work"
                                "home")))

(setq jabber-chat-buffer-show-avatar nil)
;; (setq jabber-vcard-avatars-retrieve nil
;;       jabber-vcard-avatars-publish nil
;;       )

(custom-set-variables
 '(jabber-auto-reconnect t)
 '(jabber-groupchat-buffer-format "*-jg-%n-*")
 '(jabber-roster-buffer "*-jroster-*")
 '(jabber-roster-line-format " %c %-25n %u %-8s  %S")
 '(jabber-chat-buffer-format "*-jc-%n-*")
 '(jabber-muc-private-buffer-format "*-jmuc-priv-%g-%n-*")
 '(jabber-rare-time-format "%e %b %Y %H:00")
 )

(custom-set-faces
 '(jabber-chat-prompt-system ((t (:foreground "darkgreen" :weight bold))))
 )

(setq fsm-debug nil)

;;; config-jabber.el ends here
