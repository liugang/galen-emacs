(which-func-mode 1)

(defun galen-func/which-func-settings ()
  "Settings for `which-func'."
  (setq which-func-unknown "unknown"))

(eval-after-load "which-func"
  `(galen-func/which-func-settings))

(provide 'galen-which-func-settings)
