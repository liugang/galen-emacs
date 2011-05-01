(define-key global-map (kbd "C-x H") 'goto-help-buffer)

(defun galen-func/help-mode-settings ()
  "settings of `help-mode'."
  (eal-define-keys
   'help-mode-map
   `(("B"   help-go-back)
     ("F"   help-go-forward)
     ;; ("f"   am-forward-word-or-to-word)
     ("d"   scroll-up)
     ("w"   scroll-down)
     ("v"   set-mark-command)
     ;; ("C-h" help-go-back)
     ("C-;" help-go-forward)
     ("n"   forward-button)
     ("p"   backward-button)
     ("q"   delete-current-window)
     ("'"   switch-to-other-buffer)
     ("u"   View-scroll-half-page-backward)
     ("SPC" scroll-up)
     ("."   find-symbol-at-point)
     ("/"   describe-symbol-at-point)))

  (defun goto-help-buffer ()
    "Goto *Help* buffer."
    (interactive)
    (let ((buffer (get-buffer "*Help*")))
      (if buffer
          (switch-to-buffer buffer)
        (message "*Help* buffer dose not exist!"))))

  (galen-macro/def-turn-on "view-mode" nil)
  (galen-func/add-hooks 'help-mode-hook 'view-mode-off))

(eval-after-load "help-mode"
  `(galen-func/help-mode-settings))

(provide 'galen-help-mode-settings)
