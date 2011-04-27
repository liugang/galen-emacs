;;; config-python.el ---

;; Copyright (C) Galen
;;
;; Author: Galen <galen.gang.liu@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(defun electric-pair ()
  "Insert character pair without sournding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

(defun galen.gang.liu/python-mode-hook ()
;;  (setq tab-width 2)
  (local-set-key [return] 'newline-and-indent)
  (setq indent-tabs-mode t)
  (auto-fill-mode 1)
  (turn-on-eldoc-mode)

  (define-key python-mode-map "\"" 'electric-pair)
  (define-key python-mode-map "\'" 'electric-pair)
  (define-key python-mode-map "(" 'electric-pair)
  (define-key python-mode-map "[" 'electric-pair)
  (define-key python-mode-map "{" 'electric-pair)
  )
(add-hook 'python-mode-hook 'galen.gang.liu/python-mode-hook)
(add-hook 'python-mode-hook 'galen.gang.liu/common-hook)
(add-hook 'python-mode-hook 'galen.gang.liu/show-prog-keywords)

(add-hook 'python-mode-hook 'flyspell-prog-mode)

;;; config-python.el ends here
