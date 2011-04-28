
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(semantic-load-enable-excessive-code-helpers)
;(global-semantic-idle-completions-mode -1)
;(semantic-load-enable-semantic-debugging-helpers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 一般装插件的思路，都是先load然后enable某个minor mode。cedet基本上也遵循
;; 这个规则，不过有点区别是semantic定义了很多个mode，要是挨个去enable，用
;; 户可能就要骂娘了，所以 cedet的作者Eric定义了几个方便使用的函数，这些函
;; 数会自动帮你enable某些minor mode，大概有这么几个：
;; (semantic-load-enable-minimum-features)
;; (semantic-load-enable-code-helpers)
;; (semantic-load-enable-guady-code-helpers)
;; (semantic-load-enable-excessive-code-helpers)
;; (semantic-load-enable-semantic-debugging-helpers)

;; 简单介绍一下各个函数的功能：

;;  1. semantic-load-enable-minimum-features

(semantic-load-enable-minimum-features)

;; 这个函数开启了最基本的三个特性：

;; semantic-idle-scheduler-mode

;;       enable这个mode让cedet在emacs空闲的时候自动分析buffer内容，比如正
;;       在编辑的buffer内容改变后。这个mode一般应该是需要enable的，如果没
;;       有enable这个mode，那只有手工触发才会让cedet重新分析。

;; semanticdb-minor-mode

;;       semanticdb是semantic用来保存分析后的内容的，所以也是应该enable的。

;; semanticdb-load-ebrowse-caches

;;       这个feature我不是很确定，大概的意思好像是semantic可以利用ebrowse
;;       的结果。这个feature大概就是把ebrowse生成的文件load给semantic使用。
;; (要是谁了解这个feature具体意义请告诉我下)

;;;  (semantic-load-enable-code-helpers)

;; 这个函数除enable semantic-load-enable-minimum-features外，还包括：

;;     * imenu

;;  这个feature可以让imenu显示semantic分析出的类，函数等tags。


;;     * semantic-idle-summary-mode

;; 打开这个mode之后，光标停留在一个类/函数等tag上时，会在minibuffer显
;; 示出这个函数原型

;;     * senator-minor-mode

;; senator开启之后，会在emacs上增加一个senator的菜单，可以通过菜单在
;; 当前文件的各个tag之间前后移动，跳转；还可以在里面方便地打开/关闭某
;; 个feature；还有另外一些实用的功能


;;     * semantic-mru-bookmark-mode

;; cedet有tag跳转的功能，但是经常跳转完后还需要跳回刚才的位置，这时候
;; 就需要mru-bookmark-mode了。打开这个mode之后，每次跳转semantic都会
;; 把位置当作书签一样记录下来，以后可以通过M-x
;; semantic-mrub-switch-tags（绑定到按键C-x B上）来选择跳回以前的任意
;; 一个位置。

;;;(semantic-load-enable-gaudy-code-helpers)

;;;(global-semantic-idle-summary-mode -1)
;; 这个函数除enable semantic-load-enable-code-helpers之外，还包括：

;;(semantic-stickyfunc-mode 1)

;;       这个mode会根据光标位置把当前函数名显示在buffer顶上

;;(semantic-decoration-mode 1)
;;;(global-semantic-decoration-mode -1)

;; 打开这个mode后，semantic会在类/函数等tag上方加一条蓝色的线，源文件
;; 很大的时候用它可以提示出哪些是类和函数的头。

;;(semantic-idle-completions-mode)
;;;(global-semantic-idle-completions-mode -1)
;; 这个mode打开后，光标在某处停留一段时间后，semantic会自动提示此处可
;; 以补全的内容。 如果提示的函数不是需要的，按TAB键可以在各个可能的函
;; 数之间循环，按回车就可以确定了


;;;;;;;;;; semantic-load-enable-excessive-code-helpers

;; 这个函数除enable semantic-load-enable-gaudy-code-helpers之外，还包括：

;;     * semantic-highlight-func-mode

;;       打开这个mode的话，semantic会用灰的底色把光标所在函数名高亮显
;;       示.

;; (semantic-idle-tag-highlight-mode 1)

;; 用过XCode或eclipse的人应该会喜欢高亮光标处变量的功能：就是在函数内
;; 部，光标停留在一个变量上，整个函数内部用这个变量的地方都高亮了。在
;; emacs里只要打开semantic-idle-tag-highlight-mode，光标在变量处停留
;; 一会，就会把相同的变量全都高亮.
;; semantic的这个tag-highlight虽然智能，可是我感觉它显示得太慢了

;;       * semantic-decoration-on-*-members

;; 把private和protected的函数用颜色标识出来


;(which-func-mode 1)

;; 这个其实就是emacs自带的which-function-mode，把光标当前所在的函数名
;; 显示在mode-line上。

;(semantic-load-enable-semantic-debugging-helpers)

;; 这个函数会enable几个和调试semantic相关的特性：

;;(semantic-highlight-edits-mode -1)
;(global-semantic-highlight-edits-mode -1)

;;       打开这个mode后，emacs会把最近修改过的内容高亮出来
;;       隔一段时间后高亮会自动取消，不会一直高亮着让整个buffer看起来混乱。

;;(semantic-show-unmatched-syntax-mode -1)
;(global-semantic-show-unmatched-syntax-mode -1)
;; 这个mode会把semantic解析不了的内容用红色下划线标识出来

;; (semantic-show-parser-state-mode)
;(global-semantic-show-parser-state-mode -1)

;; 打开这个mode，semantic会在modeline上显示出当前解析状态, semantic
;; 会在空闲时自动解析，另外可以打开senator-minor-mode，按[C-c , ,]或
;; 者在senator菜单中选[Force Tag Refresh]强制它马上解析。



;; if you want to enable support for gnu global
(require 'semanticdb-global)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

(when window-system
  (global-semantic-tag-folding-mode 1))

(eal-define-keys-commonly
 global-map
 `(("C-x M-j" semantic-complete-jump)))

(defun galen-func/cedet-semantic-settings ()
  "Settings for `semantic'."
  (eal-define-keys
   `(c-mode-base-map makefile-gmake-mode-map python-mode-map perl-mode-map sh-mode-map)
   `(("C-c C-j" semantic-ia-fast-jump)
     ("C-c j"   semantic-complete-jump-local)
     ("C-c n"   senator-next-tag)
     ("C-c p"   senator-previous-tag)
     ("C-c M-m" galen-func/semantic-goto-local-main)
     ("C-c M-M" galen-func/semantic-goto-main)))

  (defun galen-func/semantic-goto-main ()
    "Jump to main function."
    (interactive)
    (galen-func/semantic-goto-tag 'main))

  (defun galen-func/semantic-goto-local-main ()
    "Jump to local main function."
    (interactive)
    (galen-func/semantic-goto-local-tag 'main))

  (defun galen-func/semantic-goto-local-tag (tag)
    "Jump to local tag."
    (interactive "STag goto: ")
    (let* ((semantic-completion-collector-engine (semantic-collector-buffer-deep "" :buffer (current-buffer)))
           (semantic-completion-display-engine (semantic-displayor-traditional-with-focus-highlight "simple"))
           (semantic-complete-active-default nil)
           (semantic-complete-current-matched-tag nil)
           (tag (semantic-complete-default-to-tag tag)))

      (when (semantic-tag-p tag)
        (push-mark)
        (goto-char (semantic-tag-start tag))
        (semantic-momentary-highlight-tag tag)
        (working-message "%S: %s " (semantic-tag-class tag) (semantic-tag-name  tag)))))

  (defun galen-func/semantic-goto-tag (tag)
    "Jump to tag."
    (interactive "STag goto: ")
    (let* ((semantic-completion-collector-engine
            (semantic-collector-project-brutish "" :buffer (current-buffer) :path (current-buffer)))
           (semantic-completion-display-engine (semantic-displayor-traditional-with-focus-highlight "simple"))
           (semantic-complete-active-default nil)
           (semantic-complete-current-matched-tag nil)
           (tag (semantic-complete-default-to-tag tag)))

      (when (semantic-tag-p tag)
      (push-mark)
      (semantic-go-to-tag tag)
      (switch-to-buffer (current-buffer))
      (semantic-momentary-highlight-tag tag)
      (working-message "%S: %s " (semantic-tag-class tag) (semantic-tag-name  tag)))))

  (defun cedet-semantic-settings-4-emaci ()
    "cedet `semantic' settings for `emaci'."
    (emaci-add-key-definition
     "." 'semantic-ia-fast-jump
     '(memq major-mode dev-modes))
    (emaci-add-key-definition
     "," 'recent-jump-backward
     '(memq major-mode dev-modes)))

  (eval-after-load "emaci"
    `(cedet-semantic-settings-4-emaci))

  (eal-define-keys
   'emaci-mode-map
   `(("." emaci-.)
     ("," emaci-\,)))

  ;; system include path
  ;; (if (or mswin cygwin)
  ;;     (dolist (mode '(c-mode c++-mode))
  ;;       (semantic-add-system-include "c:/cygwin/usr/include/" mode)))
)

(defun galen-func/semantic-decorate-include-settings ()
  "Settings of `semantic-decorate-include'."
  (eal-define-keys
   'semantic-decoration-on-include-map
   `(("." semantic-decoration-include-visit))))

(defun galen-func/cedet-semantic-idle-settings ()
  "Settings for `semantic-idle'."
  (defun galen-func/semantic-idle-tag-highlight-idle-command ()
    "Highlight the tag, and references of the symbol under point.
Call `semantic-analyze-current-context' to find the refer ence tag.
Call `semantic-symref-hits-in-region' to identify local references."
    (interactive)
    (semantic-idle-tag-highlight-idle-function))

  (defun galen-func/semantic-idle-summary-idle-command ()
    "Display a tag summary of the lexical token under the cursor.
Call `semantic-idle-summary-current-symbol-info' for getting the
current tag to display information."
    (interactive)
    (semantic-idle-summary-idle-function))

  (defun galen-func/semantic-refresh-tags ()
    "Execute `semantic-idle-scheduler-refresh-tags'"
    (interactive)
    (semantic-idle-scheduler-refresh-tags))

  (eal-define-keys
   `(c-mode-base-map)
   `(("C-c M-s" galen-func/semantic-idle-summary-idle-command))))

(defun semantic-decorate-mode-settings ()
  "Settings of `semantic-decorate-mode'."
  (defun semantic-decoration-decorate ()
    "`semantic-decorate-add-decorations' all `semantic-fetch-available-tags'."
    (interactive)
    (semantic-decorate-add-decorations (semantic-fetch-available-tags))))

(eal-define-keys
 `(semantic-symref-results-mode-map)
 `(("1" delete-other-windows)
   ("2" split-window-vertically)
   ("3" split-window-horizontally)
   ("q" delete-current-window)))

;; (eval-after-load "semantic-decorate-include"
;;   `(galen-func/semantic-decorate-include-settings))

;; (eval-after-load "semantic-decorate-mode"
;;   `(semantic-decorate-mode-settings))

(eval-after-load "semantic-idle"
  `(galen-func/cedet-semantic-idle-settings))

(eval-after-load "semantic"
  `(galen-func/cedet-semantic-settings))

(provide 'galen-cedet-semantic-settings)
