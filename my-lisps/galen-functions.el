(require 'galen-eval-after-load)
(require 'galen-variables)

(when (>= 21 emacs-major-version)
  (defalias 'galen-func/move-beginning-of-line 'beginning-of-line)
  (defalias 'galen-func/move-end-of-line       'end-of-line))

;;;###autoload
(defun galen-func/add-hooks (hooks function &optional append local)
  "Call `add-hook' on hook list HOOKS use arguments FUNCTION, APPEND, LOCAL.

HOOKS can be one list or just a hook."
  (if (listp hooks)
      (mapc
       `(lambda (hook)
          (add-hook hook ',function append local))
       hooks)
    (add-hook hooks function append local)))

;;;###autoload
(defun galen-func/intern (&rest strings)
  "`intern' use STRINGS."
  (intern
   (apply
    'concat
    (mapcar
     (lambda (element)
       (if (stringp element) element (symbol-name element)))
     strings))))

;;;###autoload
(defun galen-func/variable-is-t (symbol)
  "Return SYMBOL's value is t or not."
  (and (boundp symbol) (symbol-value symbol)))

;;;###autoload
(defun galen-func/forward-word-or-to-word ()
  "`forward-word' or `forward-to-word'.
If after excute `forward-to-word', current position
is at next line, then rollback and excute `forward-word'"
  (interactive)
  (let ((noo (line-number-at-pos)) no)
    (save-excursion
      (forward-to-word 1)
      (setq no (line-number-at-pos)))
    (if (> no noo)
        (forward-word)
      (forward-to-word 1))))

;;;###autoload
(defun galen-func/equal-ignore-case (str1 str2)
  "STR1 equal ignore case to STR2 or not."
  (string= (downcase str1) (downcase str2)))

;; copy from ahei-misc.el
;;;###autoload
(defun galen-func/execute-command-on-file (file command)
  "对FILE执行命令COMMAND"
  (interactive
    (list (read-file-name "File execute command on: ")
          (let* ((input ""))
            (while (string= input "")
                   (setq input (read-string "命令: ")))
            input)))
  (if file
    (when (yes-or-no-p (concat command " file `" file "'?"))
      (shell-command (concat command " \"" file "\"")))
    (message "Executing command `%s'..." command)
    (shell-command command)))

;;;###autoload
(defun galen-func/execute-command-on-current-file (command)
  "对当前buffer执行命令COMMAND, 如果该buffer对应文件的话, 再执行`revert-buffer-no-confirm'"
  (interactive
    (list (let* ((input ""))
            (while (string= input "")
                   (setq input (read-string "命令: ")))
            input)))
  (let* ((file (buffer-file-name)))
    (galen-func/execute-command-on-file file command)
    (if file
      (revert-buffer-no-confirm))))

;;;###autoload
(defun galen-func/execute-command-on-current-dir (command)
  "对当前目录执行命令COMMAND."
  (interactive
    (list (let* ((input ""))
            (while (string= input "")
                   (setq input (read-string "命令: ")))
            input)))
  (let* ((file (buffer-file-name)))
    (galen-func/execute-command-on-file default-directory command)
    (if file
      (revert-buffer-no-confirm))))

;;;###autoload
(defalias 'galen-func/apply-define-key 'eal-define-keys-commonly)

;;;###autoload
(defalias 'galen-func/define-key-list 'eal-define-keys-commonly)

;;;###autoload
(defun galen-func/apply-args-list-to-fun (fun-list args-list)
  "Apply args list to function FUN-LIST.
  FUN-LIST can be a symbol, also can be a list whose element is a symbol."
  (let ((is-list (and (listp fun-list) (not (functionp fun-list)))))
    (dolist (args args-list)
      (if is-list
        (dolist (fun fun-list)
          (apply-args-to-fun fun args))
        (apply-args-to-fun fun-list args)))))

;;;###autoload
(defun galen-func/apply-args-to-fun (fun args)
  "Apply args to function FUN."
  (if (listp args)
    (eval `(,fun ,@args))
    (eval `(,fun ,args))))

;;;###autoload
(defun galen-func/kill-buffer-when-shell-command-exit ()
  "Close current buffer when `shell-command' exit."
  (let ((process (ignore-errors (get-buffer-process (current-buffer)))))
    (when process
      (set-process-sentinel process
                            (lambda (proc change)
                              (when (string-match "\\(finished\\|exited\\)" change)
                                (kill-buffer (process-buffer proc))))))))

;;;###autoload
(defun galen-func/list-colors-display-htm (&optional list)
  "Create HTML page which lists all the defined colors."
  (interactive)
  (if (and (null list) window-system)
    (progn
      (setq list (x-defined-colors))
      ;; Delete duplicate colors.
      (let ((l list))
        (while (cdr l)
               (if (facemenu-color-equal (car l) (car (cdr l)))
                 (setcdr l (cdr (cdr l)))
                 (setq l (cdr l)))))))
  (with-output-to-temp-buffer "*Colors*"
                              (save-excursion
                                (set-buffer standard-output)
                                (insert "<html>\n"
                                        "<head>\n"
                                        "<meta http-equiv=\"Content-Style-Type\" content=\"text/css\">\n"
                                        "<title>Colors</title>\n"
                                        "</head>\n"
                                        "<body>\n"
                                        "<h1>Colors</h1>\n"
                                        "<p>\n"
                                        "<pre>\n")
                                (let (s)
                                  (while list
                                         (insert (format (concat "<span style=\"background-color:%s\">%-20s</span>"
                                                                 "  "
                                                                 "<span style=\"color:%s\">%s</span>"
                                                                 "\n")
                                                         (html-color (car list)) (car list)
                                                         (html-color (car list)) (car list)))
                                         (setq list (cdr list))))
                                (insert "</pre>"
                                        "</body>"
                                        "</html>"))))

;;;###autoload
(defun galen-func/html-color (string)
  "Convert colors names to rgb(n1,n2,n3) strings."
  (format "rgb(%d,%d,%d)"
          (/ (nth 0 (x-color-values string)) 256)
          (/ (nth 1 (x-color-values string)) 256)
          (/ (nth 2 (x-color-values string)) 256)))

;;;###autoload
(defun galen-func/delete-current-window (&optional frame)
  "Delete window which showing current buffer."
  (interactive
    (list (and current-prefix-arg
               (or (natnump (prefix-numeric-value current-prefix-arg))
                   'visible))))
  (if (one-window-p)
    (bury-buffer)
    (delete-windows-on (current-buffer) frame)))

;;;###autoload
(defun galen-func/unset-key (keymap key)
  "Remove binding of KEY in map KEYMAP.
  KEY is a string or vector representing a sequence of keystrokes."
  (define-key keymap key nil))

;;;###autoload
(defun galen-func/trailing-whitespace-hook ()
  (when (member major-mode galen-var/trailing-whitespace-modes)
    (delete-trailing-whitespace)))

;;;###autoload
(defun galen-func/untabify-hook ()
  (when (member major-mode galen-var/untabify-modes)
    (untabify (point-min) (point-max))))

;;;###autoload
(defun galen-func/tabify-hook ()
  (when (member major-mode galen-var/tabify-modes)
    (tabify (point-min) (point-max))))

;;;###autoload
(defun galen-func/revert-buffer-with-coding-system-no-confirm (coding-system)
  "Call `revert-buffer-with-coding-system', but when `revert-buffer' do not need confirm."
  (interactive "zCoding system for visited file (default nil): ")
  (let ((coding-system-for-read coding-system))
    (revert-buffer-no-confirm)))

;;;###autoload
(defun galen-func/revert-buffer-with-gbk ()
  "Call `revert-buffer-with-coding-system-no-confirm' with gbk."
  (interactive)
  (revert-buffer-with-coding-system-no-confirm 'gbk))

;;;###autoload
(defun galen-func/skeleton-c-mode-left-brace (arg)
  (interactive "P")
  (if  (c-in-literal (c-most-enclosing-brace (c-parse-state)))
      (self-insert-command 1)
    ;; auto insert complex things.
    (let* ((current-line (delete-and-extract-region (line-beginning-position) (line-end-position)))
           (lines (and arg (mark t) (delete-and-extract-region (mark t) (point))))
           (after-point (make-marker)))
       ;;; delete extra blank begin and after the LINES
      (setq lines (and lines
                       (with-temp-buffer
                         (insert lines)
                         (beginning-of-buffer)
                         (delete-blank-lines)
                         (delete-blank-lines)
                         (end-of-buffer)
                         (delete-blank-lines)
                         (delete-blank-lines)
                         (buffer-string))))
      (save-excursion
        (let* ((old-point (point)))
          (insert (if current-line current-line "")  "{\n")
          (and lines (insert lines))
          (move-marker after-point (point))
          (insert "\n}")
          (indent-region old-point (point) nil)))
      (goto-char after-point)
      (c-indent-line))))

;;;###autoload
(defun galen-func/generate-tag-table ()
  "Generate tag tables under current directory(Linux)."
  (interactive)
  (let ((exp "") (dir ""))
    (setq dir (read-from-minibuffer "generate tags in: " default-directory)
          exp (read-from-minibuffer "suffix: "))
    (with-temp-buffer
      (shell-command
       (concat "find " dir " -name \"" exp "\" | xargs etags ")
       (buffer-name)))))

;;;###autoload
(defun galen-func/ywb-indent-accoding-to-paren ()
  "按块([]{}())来格式化代码"
  (interactive)
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char)))
        (pos (point)))
    (save-excursion
      (cond ((string-match "[[{(<]" next-char)
             (indent-region pos (progn (forward-sexp 1) (point)) nil))
            ((string-match "[\]})>]" prev-char)
             (indent-region (progn (backward-sexp 1) (point)) pos nil))))))

;;;###autoload
(defun galen-func/goto-paren ()
  "跳到匹配的括号"
  (interactive)
  (cond
   ((looking-at "[ \t]*[[\"({]") (forward-sexp) (backward-char))
    ((or (looking-at "[]\")}]") (looking-back "[]\")}][ \t]*")) (if (< (point) (point-max)) (forward-char)) (backward-sexp))
   (t (message "找不到匹配的括号"))))

;;;###autoload
(defun galen-func/copy-current-fun-name ()
  "Copy current function name."
  (interactive)
  (kill-new (which-function)))

;;;###autoload
(defun galen-func/match-parenthesis (arg)
  "Go to the matching parenthesis if on a parenthesis; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
    ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
    (t (self-insert-command (or arg 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   edit function

;;;###autoload
(defun galen-func/backward-kill-word-or-kill-region ()
  "`mark-active'时, 剪切选择的区域, 平时向后删除word, 和bash下面一样."
  (interactive)
  (if (rm-mark-active)
      (call-interactively 'rm-kill-region)
    (if mark-active
        (if cua--rectangle
            (progn
              (cua-cut-rectangle t)
              (cua-cancel))
          (call-interactively 'kill-region))
      (call-interactively 'backward-kill-word))))

;;;###autoload
(defun galen-func/mark-whole-sexp (&optional not-whole)
  "Mark whole sexp.

If NOT-WHOLE is non-nil, do not mark whole sexp."
  (interactive "P")
  (if not-whole
      (mark-sexp)
    (let ((region (bounds-of-thing-at-point 'sexp)))
      (if (not region)
          (message "Can not found sexp.")
        (goto-char (car region))
        (call-interactively 'set-mark-command)
        (forward-sexp)))))

;;;###autoload
(defun galen-func/kill-whole-sexp (&optional not-whole)
  "Kill whole sexp.

If NOT-WHOLE is non-nil, do not kill whole sexp."
  (interactive)
  (mark-whole-sexp not-whole)
  (backward-kill-word-or-kill-region))

;;;###autoload
(defun galen-func/copy-sexp (&optional not-whole)
  "Copy whole sexp.

If NOT-WHOLE is non-nil, do not copy whole sexp."
  (interactive)
  (save-excursion
    (mark-whole-sexp not-whole)
    (if mark-active
        (copy-region-as-kill (region-beginning) (region-end)))))

;;;###autoload
(defun galen-func/my-kill-word ()
  "删除一个单词, 当光标处于单词中间时也删除整个单词, 这是与`kill-word'的区别"
  (interactive)
  (wcy-mark-some-thing-at-point)
  (backward-kill-word-or-kill-region))

;;;###autoload
(defun galen-func/mark-function ()
  "Mark function."
  (interactive)
  (cond
   ((or (equal major-mode 'c-mode) (equal major-mode 'c++-mode))
    (c-mark-function))
   ((or (equal major-mode 'emacs-lisp-mode) (equal major-mode 'lisp-mode) (equal major-mode 'lisp-interaction-mode))
    (lisp-mark-function))))

;;;###autoload
(defmacro galen-func/def-action-on-function-command (fun-name action action-str)
  `(defun ,fun-name ()
     ,(concat (capitalize action-str) " function.")
     (interactive)
     (save-excursion
       (mark-function)
       (call-interactively ,action))))

;;;###autoload
(defun galen-func/comment-function (&optional arg)
  "Comment function."
  (interactive "P")
  (save-excursion
    (mark-function)
    (comment-region (region-beginning) (region-end) arg)))

;;;###autoload
(defun galen-func/kill-whole-paragraph (&optional arg)
  "Kill whole paragraph."
  (interactive "P")
  (if arg
      (kill-paragraph nil)
    (call-interactively 'mark-paragraph)
    (call-interactively 'kill-region)))

;;;###autoload
(defun galen-func/copy-whole-paragraph (&optional arg)
  "Copy whole paragraph."
  (interactive "P")
  (save-excursion
    (if arg
        (progn
          (mark-command t)
          (forward-paragraph))
      (call-interactively 'mark-paragraph))
    (call-interactively 'copy-region-as-kill)))

;;;###autoload
(defun galen-func/copy-cur-line ()
  "拷贝当前行"
  (interactive)
  (let ((end (min (point-max) (1+ (line-end-position)))))
    (kill-ring-save (line-beginning-position) end)))

;;;###autoload
(defun galen-func/copy-lines (&optional number)
  "从当前行开始拷贝NUMBER行"
  (interactive "p")
  (if (null number)
      (copy-cur-line)
    (let ((lineNo))
      (save-excursion
        (if (< number 0)
            (next-line))
        (setq lineNo (line-number-at-pos nil))
        (move-beginning-of-line nil)
        (set-mark-command nil)
        (goto-line (+ number lineNo))
        (call-interactively 'kill-ring-save)))))

;;;###autoload
(defun galen-func/copy-line-left ()
  "拷贝当前行光标后面的文字"
  (interactive)
  (kill-ring-save (point) (min (1+ (line-end-position)) (point-max))))

;;;###autoload
(defun galen-func/smart-copy ()
  "智能拷贝, 如果`mark-active'的话, 则`copy-region-as-kill', 否则`copy-lines'"
  (interactive)
  (if mark-active (call-interactively 'copy-region-as-kill) (call-interactively 'copy-lines)))

;;;###autoload
(defun galen-func/copy-region-and-paste ()
  "拷贝region并且粘贴到region后"
  (interactive)
  (call-interactively 'copy-region-as-kill)
  (call-interactively 'yank))

;;;###autoload
(defun galen-func/which-copy ()
  "如果`mark-active'的话, 则`copy-region-and-paste', 否则`copy-line-left'"
  (interactive)
  (if mark-active (copy-region-and-paste) (copy-line-left)))

;;;###autoload
(defun galen-func/insert-cur-line ()
  "拷贝当前行并粘贴进当前buffer"
  (interactive)
  (copy-cur-line)
  (forward-line)
  (beginning-of-line)
  (call-interactively 'yank)
  (previous-line)
  (end-of-line))

;;;###autoload
(defun galen-func/insert-cur-sexp ()
  "拷贝当前sexp并粘贴进当前buffer"
  (interactive)
  (copy-sexp)
  (call-interactively 'yank))

;;;###autoload
(defun galen-func/copy-sentence ()
  "拷贝sentence"
  (interactive)
  (save-excursion
    (call-interactively 'mark-end-of-sentence)
    (call-interactively 'kill-ring-save)))

;; 删除当前光标到行首的字符
;;;###autoload
(defun galen-func/del-to-begin (&optional arg)
  "Delete characters to line beginning."
  (interactive "P")
  (if (not arg)
      (kill-line 0)
    (kill-ring-save (1+ (line-beginning-position)) (point))))

;;;###autoload
(defun galen-func/lisp-mark-function (&optional allow-extend)
  "`mark-defun'有时候会多mark一个空白行, 这个函数就是解决这个bug的"
  (interactive "p")
  (mark-defun allow-extend)
  (let (next-is-fun)
    (save-excursion (forward-line) (setq next-is-fun (looking-at "[ \t]*(defun")))
    (if (or (looking-at "$") (and next-is-fun (not (looking-at "[ \t]*(defun"))))
        (forward-line))))

;;;###autoload
(defun galen-func/case-trans ()
  "大小写转换当前字符"
  (interactive)
  (let* ((ochar (char-after (point))) (char ochar))
    (if (and (>= char ?a) (<= char ?z))
        (setq char (upcase char))
      (setq char (downcase char)))
    (if (/= ochar char)
        (save-excursion
          (delete-char 1)
          (insert-char char 1)))))

;;;###autoload
(defun galen-func/comment (&optional arg)
  "如果`mark-active'的话,就`comment-region',否则注释光标所在行"
  (interactive "P")
  (if mark-active
      (comment-region (region-beginning) (region-end) arg)
    (let (fun)
      (if arg (setq fun 'uncomment-region) (setq fun 'comment-region))
      (funcall fun (line-beginning-position) (line-end-position)))))

;;;###autoload
(defun galen-func/uncomment (&optional arg)
  "如果`mark-active'的话,就`uncomment-region',否则取消注释光标所在行"
  (interactive "P")
  (comment (not arg)))

;;;###autoload
(defun galen-func/mark-invisible-region ()
  "Mark invisible region."
  (interactive)
  (if (not (and last-region-beg last-region-end))
      (message "No previous region.")
    (goto-char last-region-beg)
    (if last-region-is-rect
        (if last-region-use-cua
            (call-interactively 'cua-set-rectangle-mark)
          (call-interactively 'rm-set-mark))
      (call-interactively 'set-mark-command))
    (goto-char last-region-end)
    (if (and last-region-is-rect last-region-use-cua)
        (cua--activate-rectangle))))

;;;###autoload
(defun galen-func/c-electric-backspace-kill ()
  "If `mark-active', run `kill-region', otherwise run `c-electric-backspace'."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'c-electric-backspace)))

;;;###autoload
(defun galen-func/delete-blank-lines-region (beg end)
  "Execute `delete-blank-lines' in region."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (let ((blank-line "^\\s-*$")
          (nonblank-line "^.*\\S-.*$")
          blank-beg blank-end)
      (while (and (< (point) end) (setq blank-beg (search-forward-regexp blank-line end t)))
        (save-excursion
          (setq blank-end (search-forward-regexp nonblank-line end t)))
        (if blank-end
            (setq end (- end (- blank-end blank-beg)))
          (setq end 0))
        (previous-line)
        (delete-blank-lines)))))

;;;###autoload
(defun galen-func/smart-delete-blank-lines (&optional no-region)
  "Smart `delete-blank-lines'.

If NO-REGION is non-nil, always execute `delete-blank-lines',
otherwise, if `mark-active', execute `delete-blank-lines-region',
and execute `delete-blank-lines' if there no mark."
  (interactive "P")
  (if (or no-region (not mark-active))
      (delete-blank-lines)
    (call-interactively 'delete-blank-lines-region)))

;;;###autoload
(defun galen-func/smart-home (&optional home)
  "Goto home.

If HOME is negative, call `beginning-of-line-text',
otherwise call `move-beginning-of-line'."
  (interactive "P")
  (if (not home)
      (let ((old (point)))
        (beginning-of-line-text)
        (if (= (point) old)
            (move-beginning-of-line 1)))
    (if (< (prefix-numeric-value home) 0)
        (beginning-of-line-text)
      (move-beginning-of-line 1))))

;;;###autoload
(defun galen-func/smart-kill ()
  "If `mark-active', call `kill-region', otherwise call `kill-line'."
  (interactive)
  (if mark-active
      (call-interactively 'kill-region)
    (call-interactively 'kill-line)))
    ;;(call-interactively 'kill-whole-line)))

;;;###autoload
(defun galen-func/smart-indent ()
  "If `mark-active', call `indent-region', otherwise indent all buffer."
  (interactive)
  (save-excursion
    (unless mark-active
      (call-interactively 'mark-whole-buffer))
    (call-interactively 'indent-region)))

;;;###autoload
(defun galen-func/fill-paragraph-justify (region)
  "Run `fill-paragraph' with argument justify t."
  (interactive (list t))
  (fill-paragraph 'full region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set default COPY,CUT whole line when use M-w, C-w
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2))
     )
   ))



(defun uniq-lines (beg end)
  "Unique lines in region.
Called from a program, there are two arguments:
BEG and END (region to sort)."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (kill-line 1)
        (yank)
        (let ((next-line (point)))
          (while
              (re-search-forward
               (format "^%s" (regexp-quote (car kill-ring))) nil t)
            (replace-match "" nil nil))
          (goto-char next-line))))))


(provide 'galen-functions)
