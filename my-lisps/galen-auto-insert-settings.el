(auto-insert-mode 1)

(defun galen-func/auto-insert-settings ()
  "Settings for `auto-insert'."
  (setq auto-insert-query nil)
  (setq auto-insert-directory galen-const/templates-path)

  (define-auto-insert "build.properties" "build.properties.tpl")

  (defun galen-func/expand-template (template)
    "Expand template."
    (template-expand-template (concat galen-const/templates-path template)))

  ;; (define-auto-insert
  ;;   '("\\.\\([Hh]\\|hh\\|hxx\\|hpp\\)$" . "C/C++ header")
  ;;   (lambda ()
  ;;     (galen-func/expand-template "h.tpl")))
  ;; (define-auto-insert
  ;;   '("\\.c$" . "C")
  ;;   (lambda ()
  ;;     (galen-func/expand-template "c.tpl")))
  ;; (define-auto-insert
  ;;   '("\\.cpp$" . "Cpp")
  ;;   (lambda ()
  ;;     (galen-func/expand-template "cpp.tpl")))

  (defun galen-func/insert-headx-snippet ()
    "Insert headx snippet."
    (galen-func/insert-snippet "headx"))

  (defun galen-func/insert-abbrev (abbrev-name)
    "Insert abbrev ABBREV-NAME"
    (interactive "s")
    (insert abbrev-name)
    (expand-abbrev))

  (defun galen-func/insert-snippet (snippet)
    "Insert snippet SNIPPET."
    (interactive "s")
    (insert snippet)
    (yas/expand))

  (mapc
   (lambda (suffix)
     (define-auto-insert (concat "\\." suffix "$") 'galen-func/insert-headx-snippet))
   '("el" "sh" "org" "pl" "py" "htm\\(l\\)?")))

(eval-after-load "autoinsert"
  `(galen-func/auto-insert-settings))

(provide 'galen-auto-insert-settings)
