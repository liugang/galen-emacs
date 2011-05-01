;; ;; set mode line's face

;; (defun galen-func/mode-line-face-settings ()
;;   "Face settings for `mode-line'."
;;   (custom-set-faces
;;    '(mode-line-buffer-id
;;      ((((class grayscale) (background light)) (:foreground "LightGray" :background "yellow" :weight bold))
;;       (((class grayscale) (background dark)) (:foreground "DimGray" :background "yellow" :weight bold))
;;       (((class color) (min-colors 88) (background light)) (:foreground "Orchid" :background "yellow"))
;;       (((class color) (min-colors 88) (background dark)) (:foreground "yellow" :background "HotPink3"))
;;       (((class color) (min-colors 16) (background light)) (:foreground "Orchid" :background "yellow"))
;;       (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue" :background "yellow"))
;;       (((class color) (min-colors 8)) (:foreground "blue" :background "yellow" :weight bold))
;;       (t (:weight bold)))))

;;   (if window-system
;;       (progn
;;         (set-face-foreground 'mode-line "black")
;;         (set-face-background 'mode-line "lightgreen")
;;         (unless galen-const/is-before-emacs-21
;;           (set-face-foreground 'mode-line-inactive "black")
;;           (set-face-background 'mode-line-inactive "white")))
;;     (set-face-foreground 'mode-line "green")
;;     (set-face-background 'mode-line "black")

;;     (set-face-foreground 'mode-line-buffer-id "blue")
;;     (set-face-background 'mode-line-buffer-id "yellow")
;;     (set-face-foreground 'mode-line-inactive "white")
;;     (set-face-background 'mode-line-inactive "black"))

;;   (custom-set-faces
;;    '(header-line
;;      ((default
;;         :inherit mode-line)
;;       (((type tty))
;;        :foreground "black" :background "yellow" :inverse-video nil)
;;       (((class color grayscale) (background light))
;;        :background "grey90" :foreground "grey20" :box nil)
;;       (((class color grayscale) (background dark))
;;        :background "#D58EFFFFFC18" :foreground "blue")
;;       (((class mono) (background light))
;;        :background "white" :foreground "black"
;;        :inverse-video nil
;;        :box nil
;;        :underline t)
;;       (((class mono) (background dark))
;;        :background "black" :foreground "white"
;;        :inverse-video nil
;;        :box nil
;;        :underline t)))))

;; (eval-after-load "mode-line-settings"
;;   '(progn
;;      (defface mode-line-lines-face
;;        '((((type tty pc)) :background "red" :foreground "white")
;;          (t (:background "dark slate blue" :foreground "yellow")))
;;        "Face used to highlight lines on mode-line.")))

;; (eval-after-load "faces"
;;   `(galen-func/mode-line-face-settings))

(galen-macro/def-active-fun linum-mode linum-mode-active)

;; 在状态栏显示日期时间
;; (setq display-time-day-and-date t)

;; Enable display of time, load level, and mail flag in mode lines.
;; (display-time)

;; 在mode-line上用彩色显示当前buffer行数
;; (defun galen-func/get-lines-4-mode-line ()
;;   (let ((lines (count-lines (point-min) (point-max))))
;;     (concat (propertize
;;              (format "%dL" lines)
;;              'mouse-face 'mode-line-highlight
;;              ;; 加上颜色
;;              'face 'mode-line-lines-face
;;              'help-echo (format "%d lines" lines)) " ")))

(defun galen-func/get-size-indication-format ()
  (if (and transient-mark-mode mark-active)
      (format "%dLs %dCs" (count-lines (region-beginning) (region-end)) (abs (- (mark t) (point))))
    "%I"))

(when window-system
  (copy-face 'region 'region-invert)
  (invert-face 'region-invert))

(defun galen-func/get-mode-line-region-face ()
  (and transient-mark-mode mark-active
       (if window-system 'region 'region-invert)))

(size-indication-mode 1)
(setq-default mode-line-buffer-identification (propertized-buffer-identification "%b"))

;; (if galen-const/is-after-emacs-23
;;     (setq-default
;;      mode-line-position
;;      `((:eval (galen-func/get-lines-4-mode-line))
;;        (:propertize
;;         "%p"
;;         'local-map mode-line-column-line-number-mode-map
;;         'mouse-face 'mode-line-highlight
;;         'help-echo "Size indication mode\n\
;; mouse-1: Display Line and Column Mode Menu")
;;        ;; 当选中一块区域后, 会高亮显示这个区域有多少个字符, 没有选中区域的时候, 则显示当前buffer的大小
;;        (size-indication-mode
;;         (" "
;;          (:eval
;;           (propertize (galen-func/get-size-indication-format)
;;                       'face (and transient-mark-mode mark-active (galen-func/get-mode-line-region-face))
;;                       'local-map mode-line-column-line-number-mode-map
;;                       'mouse-face 'mode-line-highlight
;;                       'help-echo "Buffer position, mouse-1: Line/col menu"))))
;;        (:eval
;;         ;; 当显示行号已经打开时, 则不在mode-line上显示行号
;;                                         ;(if (and line-number-mode (not (linum-mode-active)))
;;         (if column-number-mode
;;             (propertize
;;              " (%l,%c)"
;;              'local-map mode-line-column-line-number-mode-map
;;              'mouse-face 'mode-line-highlight
;;              'help-echo "Line number and Column number\n\
;; mouse-1: Display Line and Column Mode Menu")
;;           (propertize
;;            " L%l"
;;            'local-map mode-line-column-line-number-mode-map
;;            'mouse-face 'mode-line-highlight
;;            'help-echo "Line Number\n\
;; mouse-1: Display Line and Column Mode Menu"))
;;                                         ;          (if column-number-mode
;;                                         ;              (propertize
;;                                         ;               " C%c"
;;                                         ;               'local-map mode-line-column-line-number-mode-map
;;                                         ;               'mouse-face 'mode-line-highlight
;;                                         ;               'help-echo "Column number\n\
;;                                         ;mouse-1: Display Line and Column Mode Menu"))
;;                                         ;        )
;;         )))
;;   (let* ((help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
;;     (setq-default
;;      mode-line-position
;;      `((:eval (galen-func/get-lines-4-mode-line))
;;        (:propertize "%p" 'help-echo ,help-echo)
;;        (size-indication-mode
;;         (" " (:eval (propertize
;;                      (galen-func/get-size-indication-format) 'help-echo ,help-echo
;;                      'face (and transient-mark-mode mark-active (galen-func/get-mode-line-region-face))))))
;;        (:eval
;;         (if (and line-number-mode (not (linum-mode-active)))
;;             (if column-number-mode
;;                 (propertize " (%l,%c)" 'help-echo ,help-echo)
;;               (propertize " L%l" 'help-echo ,help-echo))
;;           (if column-number-mode
;;               (propertize " C%c" 'help-echo ,help-echo))))))))

(let* ((help-echo
        "mouse-1: Select (drag to resize)\n\
mouse-2: Make current window occupy the whole frame\n\
mouse-3: Remove current window from display")
       (recursive-edit-help-echo "Recursive edit, type C-M-c to get out")
       (standard-mode-line-modes
        (list
         " "
         (propertize "%[" 'help-echo recursive-edit-help-echo)
         (propertize "(" 'help-echo help-echo)
         `(:propertize ("" mode-name)
                       help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                       mouse-face mode-line-highlight
                       local-map ,mode-line-major-mode-keymap)
         '("" mode-line-process)
         `(:propertize ("" minor-mode-alist)
                       mouse-face mode-line-highlight
                       help-echo "Minor mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
                       local-map ,mode-line-minor-mode-keymap)
         (propertize "%n" 'help-echo "mouse-2: Remove narrowing from the current buffer"
                     'mouse-face 'mode-line-highlight
                     'local-map (make-mode-line-mouse-map
                                 'mouse-1 #'mode-line-widen))
         (propertize ")" 'help-echo help-echo)
         (propertize "%]" 'help-echo recursive-edit-help-echo))))
  (setq-default mode-line-modes standard-mode-line-modes)
  (setq-default mode-line-format
                `("%e%t"
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  " "
                  mode-line-buffer-identification
                  ,(propertize " " 'help-echo help-echo)
                  mode-line-position
                  (vc-mode vc-mode)
                  mode-line-modes
                  (which-func-mode (" " which-func-format))
                  (working-mode-line-message (" " working-mode-line-message))
                  ,(propertize "-%-" 'help-echo help-echo))))

(setq mode-line-format-bak mode-line-format)
(setq mode-line t)

(defun galen-func/toggle-mode-line ()
  "Toggle mode-line."
  (interactive)
  (if mode-line
      (setq-default mode-line-format nil)
    (setq-default mode-line-format mode-line-format-bak))
  (setq mode-line (not mode-line)))

;; 在标题栏显示登陆名称和文件名
(setq frame-title-format
      '((:eval
         (let ((login-name (getenv-internal "LOGNAME")))
           (if login-name (concat login-name "@") "")))
        (:eval (system-name))
        ":"
        (:eval (or (buffer-file-name) (buffer-name)))))

(provide 'galen-mode-line-settings)
