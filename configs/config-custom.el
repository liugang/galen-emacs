(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(backward-delete-char-untabify-method (quote hungry))
 '(blink-cursor-mode nil)
; '(bookmark-bmenu-file-column 50)
; '(bookmark-bmenu-toggle-filenames nil)
 '(column-number-mode t)
 '(custom-buffer-done-kill t)
 '(default-directory "~/" t)
 '(default-fill-column 80 t)
 '(display-battery-mode t)

;; '(display-time-24hr-format t)
;; '(display-time-mode t)
;; '(display-time-use-mail-icon t)

; '(ecb-layout-window-sizes (quote (("left9" (ecb-methods-buffer-name 0.20512820512820512 . 0.9210526315789473)))))
; '(ecb-options-version "2.40")
 '(echo-keystrokes 0.1)
;; 可以递归的使用minibuffer
 '(enable-recursive-minibuffers t)
;;;;  '(frame-title-format "%b" t)
 '(global-font-lock-mode t)
 '(global-hl-line-mode nil)
; '(global-semantic-tag-folding-mode t nil (semantic-util-modes))
;;; '(gud-tooltip-mode t)
 '(indent-tabs-mode t)
 '(inhibit-startup-screen t)
; '(initial-scratch-message "")


 '(line-number-mode t)
 '(require-final-newline t)
 '(scalable-fonts-allowed t)
 '(scroll-bar-mode (quote right))
; '(semantic-idle-scheduler-idle-time 3)
; '(semantic-self-insert-show-completion-function (lambda nil (semantic-ia-complete-symbol-menu (point))))
; '(semanticdb-default-save-directory "~/.emacs.d/semanticdb")
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(tool-bar-mode t)
 '(tooltip-hide-delay 100)
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote forward))
 '(use-dialog-box nil)
 '(visible-bell t)
 '(tab-width 8)
 '(make-backup-files nil)
;; 个人信息
 '(user-mail-address "galen.gang.liu@gmail.com")
 '(user-full-name    "Galen")

;; Emacs找不到合适的模式时，缺省使用text-mode
; '(default-major-mode 'text-mode)

;; 防止页面滚动时跳动,scroll-margin 3可以在靠近屏幕边沿3行时就开始滚动,可以很好的看到上下文
; '(scroll-margin 3); info mode have problem
 '(scroll-conservatively 10000)

;; 没有提示音,也不闪屏
 '(ring-bell-function 'ignore)

;; 不保存连续的重复的kill
 '(kill-do-not-save-duplicates t)

;; 先格式化再补全
 '(tab-always-indent 'complete)

;; '(system-time-locale "C")
;; '(x-select-enable-clipboard t)  ;; default is t in emacs24
;; 支持emacs和外部程序的粘贴

 )






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; don't waste time on these, just use the default color, it's OK
;; there are many important things need we do.

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )


;; emacs lock
(autoload 'toggle-emacs-lock "emacs-lock" "Emacs lock" t)

;; 启用以下功能
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; 不要总是没完没了的问yes or no, 为什么不能用y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; 不要闪烁光标, 烦不烦啊
(blink-cursor-mode -1)

;; 光标靠近鼠标指针时，让鼠标指针自动让开，别挡住视线。
;;(mouse-avoidance-mode 'animate)

;; 可以保存你上次光标所在的位置
;(require 'saveplace)
;(setq-default save-place t)

;; 当你在shell、telnet、w3m等模式下时，必然碰到过要输入密码的情况,此时加密显出你的密码
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(add-hook 'before-save-hook 'galen-func/trailing-whitespace-hook)
(add-hook 'before-save-hook 'galen-func/untabify-hook)

;(setq-default default-directory "~")

;; 在fringe上显示一个小箭头指示当前buffer的边界
;;(setq-default indicate-buffer-boundaries 'left)

;; 不要滚动条
(customize-set-variable 'scroll-bar-mode nil)


;; ;; 编码设置
;; coding system setting

;; 设置默认编码
(when (not (and galen-const/is-after-emacs-23 window-system))
  (set-language-environment "UTF-8"))
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)



;; (if window-system
;;     (define-key global-map (kbd "C-2") 'set-mark-command))


(add-hook
 'after-save-hook
 (lambda ()
   (if (not (= (shell-command (concat "test -x " (buffer-file-name))) 0))
       (progn
         ;; This puts message in *Message* twice, but minibuffer
         ;; output looks better.
         (message (concat "Wrote " (buffer-file-name)))
         (save-excursion
           (goto-char (point-min))
           ;; Always checks every pattern even after
           ;; match.  Inefficient but easy.
           (dolist (first-line-pattern galen-var/script-firstline-patterns)
             (if (looking-at first-line-pattern)
                 (if (= (shell-command
                         (concat "chmod u+x " (buffer-file-name)))
                        0)
                     (message (concat
                               "Wrote and made executable "
                               (buffer-file-name))))))))
     ;; This puts message in *Message* twice, but minibuffer output
     ;; looks better.
     (message (concat "Wrote " (buffer-file-name))))))

;; 回车后indent
(eal-define-keys
 `(lisp-mode-map
   emacs-lisp-mode-map
   lisp-interaction-mode-map
   sh-mode-map
   awk-mode-map
   java-mode-map
   ruby-mode-map
   c-mode-base-map
   tcl-mode-map
   org-mode-map
   python-mode-map
   perl-mode-map)
 `(("RET" newline-and-indent)

   ))

;; 输入左大花扩号自动补齐右大花括号
(eal-define-keys
 `(c-mode-base-map awk-mode-map)
 `(("{" galen-func/skeleton-c-mode-left-brace)))

(eal-define-keys-commonly
 global-map
 `(("C-M-]" galen-func/ywb-indent-accoding-to-paren)
   ("\C-]" galen-func/goto-paren)))
