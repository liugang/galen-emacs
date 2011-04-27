;; for global
;(load-file "~/.emacs.d/plugins/gtags.el")

(require 'eassist)
(defun eassist-settings()
  "Setting for `eassist'."
  (setq eassist-mode-hook
	'(lambda ()
	   ;(define-key gtags-mode-map "\C-csr" 'gtags-visit-rootdir)
	   (define-key eassist-mode-map (kbd "C-9") 'eassist-escape)
	   )))

(eval-after-load "gtags"
  `(eassist-settings))

(provide 'eassist-settings)

