(ffap-bindings)

(defun galen-func/ffap-settings ()
  "Settings for `ffap'."
  (setq ffap-c-path (append ffap-c-path galen-const/system-head-file-dir galen-const/user-head-file-dir)))

(eval-after-load "ffap"
  `(galen-func/ffap-settings))

(provide 'galen-ffap-settings)
