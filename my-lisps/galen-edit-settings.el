;; 在行首C-k时，同时删除该行
;;(setq-default kill-whole-line t)
;; (define-key minibuffer-local-completion-map (kbd "C-k") 'kill-line)

;; 缩进设置
;; 不用TAB字符来indent
(setq-default indent-tabs-mode nil)
(setq tab-width 8)
(setq tab-stop-list nil)
(loop for x downfrom 40 to 1 do
      (setq tab-stop-list (cons (* x tab-width) tab-stop-list)))

;; 自动的在文件末增加一新行
(setq require-final-newline t)

;; (autoload 'copy-region-as-kill-nomark "pc-select"
;;   "Save the region as if killed; but don't kill it; deactivate mark.
;; If `interprogram-cut-function' is non-nil, also save the text for a window
;; system cut and paste.

;; Deactivating mark is to avoid confusion with `delete-selection-mode'
;; and `transient-mark-mode'." t)

;; ;;;###autoload
;; (defmacro def-action-on-area-command (fun-name action mark-area doc)
;;   `(defun ,fun-name ()
;;      ,doc
;;      (interactive)
;;      (save-excursion
;;        (funcall ,mark-area)
;;        (call-interactively ,action))))

;; (apply-args-list-to-fun
;;  'def-action-on-area-command
;;   `((copy-function        'copy-region   'mark-function     "Copy function.")
;;     (kill-function        'kill-region   'mark-function     "Kill function.")
;;     (indent-function      'indent-region 'mark-function     "Indent function.")
;;     (indent-paragraph     'indent-region 'mark-paragraph    "Indent paragraph.")
;;     (copy-whole-buffer    'copy-region   'mark-whole-buffer "Copy whole buffer.")
;;     (kill-whole-buffer    'kill-region   'mark-whole-buffer "Kill whole buffer.")
;;     (indent-whole-buffer  'indent-region 'mark-whole-buffer "Indent whole buffer.")))

(eal-define-keys
 (append
  galen-var/makefile-mode-map-list
  `(c-mode-base-map
    emacs-lisp-mode-map
    lisp-interaction-mode-map
    conf-javaprop-mode-map
    html-mode-map
    tcl-mode-map
    autoconf-mode-map
    perl-mode-map
    nxml-mode-map
    python-mode-map))
 `(("C-c C-c" galen-func/comment)))

;; 定义redo命令
(galen-macro/def-redo-command redo 'redo 'undo)

(eal-define-keys-commonly
 global-map
 `(
   ;; ("M-k" kill-whole-paragraph)
   ("%" galen-func/match-parenthesis)
   ;; ("M-C-k" kill-paragraph)
   ;; ("M-C" copy-whole-paragraph)
   ;; ("C-x c" copy-whole-buffer)
   ;; ("C-x C" kill-whole-buffer)
   ;; ("M-W" which-copy)
   ;; ("M-w" smart-copy)
   ;; ("C-x M-w" insert-cur-line)
   ;; ("C-x M-W" insert-cur-sexp)
   ;; ("C-M-w" copy-sentence)
   ;; 删除整行
   ;; ("M-K" kill-line)
   ;; ("C-k" smart-kill)
   ;; ("C-\\" delete-indentation)
   ;; ("C-x M-M" mark-invisible-region)
   ;; ("M-U" del-to-begin)
   ;; ("C-^" case-trans)
   ;; ("C-6" case-trans)
   ;; ("C-w" backward-kill-word-or-kill-region)
   ;; ("C-x S" mark-whole-sexp)
   ;; ("C-x W" kill-whole-sexp)
   ;; ("C-x w" copy-sexp)
   ;; ("M-D" my-kill-word)
   ;; ("C-x TAB" indent-whole-buffer)
   ;; ("C-h" c-electric-backspace-kill)
   ;; ,(if window-system '("C-z" undo))
   ;; ("M-Y" redo)
   ;; ("M-m" beginning-of-line-text)
   ;; ("C-M-\\" smart-indent)
   ;; ("M-q" fill-paragraph-justify)
   ;; ("<escape> SPC" just-one-space)
   ))

;; (eal-define-keys
;;  'c-mode-base-map
;;   `(("C-c f" copy-function)
;;     ("C-c F" kill-function)
;;     ("C-c C" comment-function)
;;     ("C-M-h" mark-function)))

;; (eal-define-keys
;;  `(emacs-lisp-mode-map
;;    lisp-interaction-mode-map)
;;  `(("C-M-h" mark-function)
;;    ("C-c D"  edebug-defun)
;;    ("C-c C-d" eval-defun)
;;    ("C-c B"  eval-buffer)
;;    ("C-c f" copy-function)
;;    ("C-c F" kill-function)
;;    ("C-c C-q" indent-function)
;;    ("C-c C" comment-function)))

(provide 'galen-edit-settings)
