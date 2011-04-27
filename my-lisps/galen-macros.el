(require 'galen-functions)

;;;###autoload
(defmacro galen-macro/def-active-fun (symbol &optional fun-name)
  "Make definition of function judge variable is active or not."
  `(defun ,(if fun-name fun-name symbol) ()
     ,(concat "`" (symbol-name symbol) "' is t or not.")
     (galen-func/variable-is-t ',symbol)))


;;;###autoload
(defmacro galen-macro/with-temp-mode (mode &rest body)
  "Create a temporary buffer with mode MODE, and evaluate BODY there like `progn'.
See also `with-temp-buffer'."
  `(with-temp-buffer
     (funcall ,mode)
     ,@body))

;;;###autoload
(defmacro galen-macro/def-execute-command-on-file-command (command)
  "Make definition of command which execute command on file."
  `(defun ,(intern (subst-char-in-string ?\ ?- command)) (file)
     ,(concat "Run command `" command "' on file FILE.")
     (interactive (list (read-file-name (concat "File to " ,command ": "))))
     (galen-func/execute-command-on-file file ,command)))

;;;###autoload
(defmacro galen-macro/def-execute-command-on-current-file-command (command)
  "Make definition of command which execute command on current file."
  `(defun ,(galen-func/intern (subst-char-in-string ?\ ?- command) "-current-file") ()
     ,(concat "Execute command `" command "' on current file.")
     (interactive)
     (galen-func/execute-command-on-current-file ,command)))

;;;###autoload
(defmacro galen-macro/def-execute-command-on-current-dir-command (command)
  "Make definition of command which execute command on current directory."
  `(defun ,(galen-func/intern (subst-char-in-string ?\ ?- command) "-current-dir") ()
     ,(concat "Execute command `" command "' on current directory.")
     (interactive)
     (galen-func/execute-command-on-current-dir ,command)))

;;;###autoload
(defmacro galen-macro/define-kbd     (keymap key def) `(define-key ,keymap (kbd ,key) ,def))

;;;###autoload
(defmacro galen-macro/local-set-kbd  (key command)    `(local-set-key (kbd ,key) ,command))

;;;###autoload
(defmacro galen-macro/global-set-kbd (key command)    `(global-set-key (kbd ,key) ,command))

;;;###autoload
(defmacro galen-macro/def-command-max-window (command)
  "Make definition of command which after execute command COMMAND execute `delete-other-windows'."
  `(defun ,(galen-func/intern command "-max-window") ()
     ,(concat "After run command `" command "' execute command `delete-other-windows'.")
     (interactive)
     (call-interactively ',(intern command))
     (delete-other-windows)))

;;;###autoload
(defmacro galen-macro/def-turn-on (command &optional is-on)
  "Make definition of command whose name is COMMAND-on when IS-ON is t
  and COMMAND-off when IS-ON is nil."
  (let ((on (if is-on "on" "off")))
    `(defun ,(galen-func/intern command "-" on) ()
       ,(concat "Turn " on " `" command "'.")
       (interactive)
       (funcall ',(intern command) ,(if is-on 1 -1)))))

(provide 'galen-macros)
