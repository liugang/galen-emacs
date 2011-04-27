;;; config-sh-mode.el ---

;; Copyright (C) Galen
;;
;; Author: Galen <galen.gang.liu@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

;;

(defun galen.gang.liu/sh-check-finish-hook (buf msg)
  "Function, that is executed at the end of sh check"
  (when (not (string-match "finished" msg))
    (next-error 1 t)))

(define-compilation-mode sh-check-mode "SH"
  "Mode for check sh source code."
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-scroll-output) nil)
  (set (make-local-variable 'compilation-finish-functions)
       (list 'galen.gang.liu/sh-check-finish-hook))
  )

(defun galen.gang.liu/sh-check-syntax ()
  "Check syntax of current file"
  (interactive)
  (when (string-match "^\\(ba\\|z\\)sh" (symbol-name sh-shell))
    (save-some-buffers t)
    (compilation-start (concat (symbol-name sh-shell) " -n " (buffer-file-name))
                       'sh-check-mode))
  )

;; sh mode hook
(defun galen.gang.liu/sh-mode-hook ()
  (local-set-key "\C-cl" 'galen.gang.liu/sh-check-syntax)
  (local-set-key [return] 'newline-and-indent)
  )
(add-hook 'sh-mode-hook 'galen.gang.liu/sh-mode-hook)
(add-hook 'sh-mode-hook 'galen.gang.liu/common-hook)
(add-hook 'sh-mode-hook 'galen.gang.liu/show-prog-keywords)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; config-sh-mode.el ends here
