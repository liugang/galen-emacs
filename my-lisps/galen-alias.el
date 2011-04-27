(require 'galen-eval-after-load)

(when (>= 21 emacs-major-version)
  (defalias 'galen-func/move-beginning-of-line 'beginning-of-line)
  (defalias 'galen-func/move-end-of-line       'end-of-line))

;;;###autoload
(defalias 'galen-func/apply-define-key 'eal-define-keys-commonly)

;;;###autoload
(defalias 'galen-func/define-key-list 'eal-define-keys-commonly)

(provide 'galen-alias)
