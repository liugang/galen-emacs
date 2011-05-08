;; sourcepair,可以在cpp与h文件之间切换
;; (require 'sourcepair-settings)

;; 改包中定义了C-j 为goto-line, 还设置了c-style

;; 然后把kde-emacs.tar.gz 解压到一个目录在.emacs中加上
;; (add-to-list 'load-path WHEREKDEEMACSIN)
(require 'kde-emacs)

(require 'kde-emacs-core)

(autoload 'agulbra-make-member "kde-emacs-utils"
  "make a skeleton member function in the .cpp or .cc file" t)

(eal-define-keys
 'c++-mode-map
 `(("C-c C-b" agulbra-make-member)))

(defun kde-emacs-settings ()
  "Settings for `kde-emacs'."
  (setq magic-keys-mode t)
  (setq kde-tab-behavior 'indent)

  (galen-func/add-hooks
   `(java-mode-hook)
   (lambda ()
     (c-set-style "kde-c++"))))

(eval-after-load "kde-emacs-core"
  `(kde-emacs-settings))

(provide 'kde-emacs-settings)
