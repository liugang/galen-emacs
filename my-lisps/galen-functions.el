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






(provide 'galen-functions)
