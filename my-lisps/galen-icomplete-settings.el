;(require 'my-icomplete+)

(icomplete-mode 1)

(defun galen-func/icomplete-settings ()
  "Settings for `icomplete'."
  (define-key minibuffer-local-completion-map (kbd "SPC") 'minibuffer-complete-word))

(eval-after-load "icomplete"
  `(galen-func/icomplete-settings))

(provide 'galen-icomplete-settings)
