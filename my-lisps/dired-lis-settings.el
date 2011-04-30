(require 'dired-lis)

;; (eal-define-keys
;;  'isearch-mode-map
;;  `(("C-h" dired-lis-isearch-up-directory)))

(defun dired-lis-settings ()
  "Settings for `dired-lis'.")

(eval-after-load "dired-lis"
  `(dired-lis-settings))

(provide 'dired-lis-settings)
