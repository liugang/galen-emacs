(require 'highlight-symbol)

(when window-system
  (galen-func/add-hooks
   '(emacs-lisp-mode-hook
     lisp-interaction-mode-hook
     java-mode-hook
     c-mode-common-hook
     text-mode-hook
     ruby-mode-hook
     html-mode-hook
     sh-mode-hook
     Info-mode-hook
     perl-mode-hook)
   'galen-func/highlight-symbol-mode-on))

(defun galen-func/highlight-symbol-settings ()
  "Settings for `highlight-symbol'."

  (setq highlight-symbol-idle-delay 0.5)

  (defun galen-func/highlight-symbol-mode-on ()
    "Turn on function `highlight-symbol-mode'."
    (if window-system
        (highlight-symbol-mode 1)))

  (defun galen-func/highlight-symbol-mode-off ()
    "Turn off function `highlight-symbol-mode'."
    (highlight-symbol-mode -1))

;;;###autoload
  (define-globalized-minor-mode global-highlight-symbol-mode highlight-symbol-mode galen-func/highlight-symbol-mode-on)

  ;; highlight-line 后看不到当前行的颜色
;;;###autoload
  (defun highlight-symbol-jump (dir)    ; 覆盖默认的定义
    "Jump to the next or previous occurence of the symbol at point.
DIR has to be 1 or -1."
    (let ((symbol (highlight-symbol-get-symbol)))
      (if symbol
          (let* ((case-fold-search nil)
                 (bounds (bounds-of-thing-at-point 'symbol))
                 (offset (- (point) (if (< 0 dir) (cdr bounds) (car bounds)))))
            (unless (eq last-command 'highlight-symbol-jump)
              (push-mark))
            (let ((target
                   (save-excursion
                     ;; move a little, so we don't find the same instance again
                     (goto-char (- (point) offset))
                     (re-search-forward symbol nil t dir))))
              (if target
                  (goto-char (+ target offset))
                (message (format "Reach %s" (if (> dir 0) "bottom" "top"))))
              (setq this-command 'highlight-symbol-jump)))
        (error "No symbol at point"))))

  ;; I bind "C-x w" to `copy-sexp'
  (eal-define-keys
   'hi-lock-map
   `(("C-x w" nil)))

  ;; (eal-define-keys
  ;;  `(emacs-lisp-mode-map lisp-interaction-mode-map java-mode-map
  ;;                        c-mode-base-map text-mode-map ruby-mode-map html-mode-map)
  ;;  `(("C-c M-H" highlight-symbol-at-point)
  ;;    ("C-c M-R" highlight-symbol-remove-all)
  ;;    ("C-c M-N" highlight-symbol-next)
  ;;    ("C-c M-P" highlight-symbol-prev)
  ;;    ("C-c r"   highlight-symbol-query-replace)
  ;;    ("C-c M-n" highlight-symbol-next-in-defun)
  ;;    ("C-c M-p" highlight-symbol-prev-in-defun)))

  )

;; highlight-symbol (only for gui mode )
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)

(eval-after-load "highlight-symbol"
  '(galen-func/highlight-symbol-settings))

(provide 'galen-highlight-symbol-settings)
