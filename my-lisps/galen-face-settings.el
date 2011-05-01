(defun galen-func/help-mode-face-settings ()
  "Face settings for `help-mode'."
  (set-face-foreground 'help-argument-name "blue"))

(eval-after-load "help-mode"
  `(galen-func/help-mode-face-settings))

(provide 'galen-face-settings)
