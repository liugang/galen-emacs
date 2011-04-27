;; 语法高亮
(global-font-lock-mode t)

;; hs-minor-mode,折叠代码
;(require 'hs-minor-mode-settings)



;; 所有关于括号的配置
(require 'all-paren-settings)

;; 用来显示当前光标在哪个函数
;(require 'which-func-settings)

;; cedet 强大的开发工具, 包括代码浏览, 补全, 类图生成
;; 用CEDET浏览和编辑C++代码 http://emacser.com/cedet.htm
;; Emacs才是世界上最强大的IDE － cedet的安装 http://emacser.com/install-cedet.htm
(require 'cedet-settings)

;; ecb 代码浏览器
;(require 'ecb-settings)

;; doxygen
;(require 'doxymacs-settings)

;(require 'autoconf-mode-settings)

;; 像Eclipse那样高亮光标处单词
(require 'highlight-symbol-settings)

;; `gdb'
(require 'gud-settings)

;; subversion
(require 'svn-settings)

;; 版本控制
(require 'vc-settings)


;; 所有关于lisp方面的配置
(require 'all-lisp-settings)

;; 开发shell程序的mode配置
(require 'sh-mode-settings)

;; xml mode配置
;(require 'sgml-mode-settings)

;(require 'html-mode-settings)

;; ruby
(require 'ruby-settings)

;; rails
;(require 'rails-settings)

;(require 'php-mode)

;(require 'sql-settings)

;; 显示变量, 函数的声明
(require 'eldoc-settings)

;; 自动给你加上括号
(require 'autopair-settings)

;; 把Eclipse的功能带给Emacs
(require 'eclim-settings)

;; 方便开发c/c++的配置
(require 'c-settings)

;; 放在kde-emacs后面
(require 'compile-settings)


(defun copy-current-fun-name ()
  "Copy current function name."
  (interactive)
  (kill-new (which-function)))

(mapc 'require '(;; hide region
                 ;; hide-region-settings
                 ;; hide lines
                 ;; hide-lines
                 ;; 把imenu以tree的形式显示出来
                 ;;imenu-tree-settings
                 ;; 高亮引用的函数和变量
                 ;;zjl-hl-settings
                 ;; 把speedbar放到当前frame里面
                 ;;sr-speedbar-settings
                 ;;codepilot-settings  ;; this will make gdb-many-window bad
                 ;;perl-settings
                 ;; 实现程序变量的自动对齐
                 ;;align-settings
                 ;; 生成c程序调用图
                 ;; http://emacser.com/emacs-cflow.htm
                 cflow-mode-settings))

(provide 'dev-settings)
