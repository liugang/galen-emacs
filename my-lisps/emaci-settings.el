;; -*- Emacs-Lisp -*-
;; Time-stamp: <2010-12-01 11:57:41 Wednesday by taoshanwen>

(require 'util)
(require 'emaci)

(apply-define-key
 global-map
 `(("M-s" emaci-mode-on)
   ("M-S" emaci-mode-off)))

(eal-define-keys
 'emaci-mode-map
 `(("/" describe-symbol-at-point)
   ("'" switch-to-other-buffer)
   ("L" count-brf-lines)
   ("t" sb-toggle-keep-buffer)
   ("]" goto-paren)
   ("c" go-to-char-forward-sb)
   ("C" go-to-char-backward-sb)))

(defun emaci-settings ()
  "settings for `emaci'."
  (setq emaci-brief-key-defs
        (append emaci-brief-key-defs
                `(("]" goto-paren))))
  (emaci-bind-brief-keys)
  
  ;;;###autoload
  (defun switch-major-mode-with-emaci ()
    "Run `switch-major-mode' with `emaci-mode'."
    (interactive)
    (let ((emaci emaci-mode))
      (call-interactively 'switch-major-mode)
      (emaci-mode (if emaci 1 -1))))

  (eal-define-keys-commonly
   global-map
   `(("C-x q" switch-major-mode-with-emaci))))

(eval-after-load "emaci"
  `(emaci-settings))

(provide 'emaci-settings)
