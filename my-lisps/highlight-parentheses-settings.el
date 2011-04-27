;; -*- Emacs-Lisp -*-

;; Time-stamp: <2011-03-15 01:54:37 Tuesday by liugang>

(require 'highlight-parentheses)

;; TODO: 最后一项不知道为啥不起作用
;;(setq hl-paren-colors '("red" "yellow" "cyan" "magenta" "green" "red"))
(setq hl-paren-colors '("red" "blue" "red" "red" "red" "blue"))

(am-add-hooks
 `(find-file-hook help-mode-hook Man-mode-hook log-view-mode-hook
                  compilation-mode-hook gdb-mode-hook lisp-interaction-mode-hook
                  browse-kill-ring-mode-hook completion-list-mode-hook hs-hide-hook
                  inferior-ruby-mode-hook custom-mode-hook Info-mode-hook svn-log-edit-mode-hook
                  package-menu-mode-hook dired-mode-hook apropos-mode-hook)
 'highlight-parentheses-mode)

(provide 'highlight-parentheses-settings)
