;;; config-desktop.el --- Load desktop settings

;; Copyright (C) 2010 Galen
;;
;; Author: galen.gang.liu@gmail.com
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

;(setq-default desktop-missing-file-warning nil)
;(setq-default desktop-path (quote ("~")))
;(setq-default desktop-save t)
;(setq-default desktop-save-mode t)   ;; PID problem
;(setq-default save-place t)

;(add-to-list 'desktop-locals-to-save 'buffer-file-coding-system)
;(add-to-list 'desktop-locals-to-save 'tab-width)

;(defun galen.gang.liu/desktop-ignore-semantic (desktop-buffer-file-name)
;       "Function to ignore cedet minor modes during restore of buffers"
;       nil)
;(add-to-list 'desktop-minor-mode-handlers '(semantic-stickyfunc-mode . galen.gang.liu/desktop-ignore-semantic))
;(add-to-list 'desktop-minor-mode-handlers '(senator-minor-mode . galen.gang.liu/desktop-ignore-semantic))
;(add-to-list 'desktop-minor-mode-handlers '(semantic-idle-scheduler-mode . galen.gang.liu/desktop-ignore-semantic))
;(add-to-list 'desktop-minor-mode-handlers '(semantic-idle-summary-mode . galen.gang.liu/desktop-ignore-semantic))
;(add-to-list 'desktop-minor-mode-handlers '(semantic-idle-completions-mode . galen.gang.liu/desktop-ignore-semantic))
;(add-to-list 'desktop-minor-mode-handlers '(semantic-mru-bookmark-mode . galen.gang.liu/desktop-ignore-semantic))
;(add-to-list 'desktop-minor-mode-handlers '(semantic-decoration-mode . galen.gang.liu/desktop-ignore-semantic))
;(add-to-list 'desktop-minor-mode-handlers '(srecode-minor-mode . galen.gang.liu/desktop-ignore-semantic))
;(add-to-list 'desktop-minor-mode-handlers '(ede-minor-mode . galen.gang.liu/desktop-ignore-semantic))

;(desktop-read)


;; for desktop
(desktop-save-mode t)
;; http://www.emacswiki.org/emacs/DeskTop
;; You can specify buffers which should not be saved, by name or by mode, e.g.:

(setq desktop-buffers-not-to-save
       (concat "\\("
                        "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                        "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                        "\\)$"))
 (add-to-list 'desktop-modes-not-to-save 'dired-mode)
 (add-to-list 'desktop-modes-not-to-save 'Info-mode)
 (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
 (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

;; Auto-Saving the Desktop whenever emacs is idle
(add-hook 'auto-save-hook (lambda () (desktop-save-in-desktop-dir)))

;; Automatically Overriding Stale Locks
;;; desktop-override-stale-locks.el begins here

 (defun emacs-process-p (pid)
   "If pid is the process ID of an emacs process, return t, else nil.
   Also returns nil if pid is nil."
   (when pid
     (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
       (when (file-exists-p cmdline-file)
         (with-temp-buffer
           (insert-file-contents-literally cmdline-file)
           (goto-char (point-min))
           (search-forward "emacs" nil t)
           pid)))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
           "Don't allow dead emacsen to own the desktop file."
           (when (not (emacs-process-p ad-return-value))
             (setq ad-return-value nil)))

;;; config-desktop.el ends here
