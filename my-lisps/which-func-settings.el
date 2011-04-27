(which-func-mode 1)

(defun which-func-settings ()
  "Settings for `which-func'."
  (setq which-func-unknown "unknown"))

(eval-after-load "which-func"
  `(which-func-settings))

(provide 'which-func-settings)
