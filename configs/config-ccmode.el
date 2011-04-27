;;; config-ccmode.el ---

;; Copyright (C) 2010 Galen
;;
;; Author: galen.gang.liu@gmail.com
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(require 'cc-mode)

(load "~/.emacs.d/plugins/c-eldoc.el")
(setq c-eldoc-includes "-I/usr/include -I~/workspace/spider/include -I./ -I../ ")

;; customisation of cc-mode
(defun galen.gang.liu/c-mode-common-hook ()

  ;; style customization
  (setq tab-width 4)
  (setq default-tab-width 4)
  (setq c-default-style "k&r")
  (setq c-basic-offset tab-width)
  (setq indent-tabs-mode nil)
  (setq comment-style 'extra-line)
  (setq compilation-window-height 15)
  (setq compilation-auto-jump-to-first-error t)
  (setq compilation-scroll-output t)

  (c-toggle-auto-hungry-state 0)
  (c-toggle-hungry-state t)
  (c-set-offset 'member-init-intro '++)
  (c-set-offset 'substatement-open 0)
  (c-set-style "k&r")

  ;; minor modes
  (auto-fill-mode 1)
  (c-turn-on-eldoc-mode)
  (gtags-mode 1)
  (hs-minor-mode t)
  (which-function-mode t)

  ;; local keys
  (local-set-key [return] 'newline-and-indent)
  ;;        (local-set-key [delete]  'delete-char)
  )

(add-hook 'c-mode-common-hook 'galen.gang.liu/c-mode-common-hook)
(add-hook 'c-mode-common-hook 'galen.gang.liu/common-hook)
(add-hook 'c-mode-common-hook 'galen.gang.liu/show-prog-keywords)

;; ***********************************************************************
;; c/c++ programming setting

(defun customise-c-mode-settings ()  ;; C language
  (c-set-style "k&r")
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  )

(defun customise-cpp-mode-settings ()  ;; C++ language
  (c-set-style "stroustrup")
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; don't modify these lines
(add-hook
 'c-mode-hook
 '(lambda ()
    (customise-c-mode-settings)))

(add-hook
 'c++-mode-hook
 '(lambda ()
    (customise-cpp-mode-settings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; for ruby-mode
(add-hook 'ruby-mode-hook (lambda ()
;;  (local-set-key "\r" 'newline-and-indent)
    (gtags-mode 1)
    ))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

















;; (require 'info-look)
;; (info-lookup-add-help
;;  :mode 'c-mode
;;  :regexp "[^][()'\" \t\n]+"
;;  :ignore-case t
;;  :doc-spec '(("(libc)Symbol Index" nil nil nil)))

;; (defun fp-c-mode-routine ()
;;   (local-set-key "\M-q" 'rebox-comment))
;; (add-hook 'c-mode-hook 'fp-c-mode-routine)

;; (setq-default c-default-style (quote ((java-mode . "java") (other . "gnu"))))

;; (add-to-list 'auto-mode-alist '("\\.ipp?$" . c++-mode))

;;; config-cmode.el ends here
