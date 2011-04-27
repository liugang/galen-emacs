(eal-define-keys
 'c-mode-base-map
 `(("C-c g" gdb)
   ("C-c b" gud-break)
   ("<f7>"  gud-step)
   ("<f8>"  gud-next)
   ("C-c B" gud-remove)))

(defun gud-settings ()
  "Settings for `gud'."
  (eal-define-keys
   'gud-mode-map
   `(("C-c B" gud-remove)
     ("M-s"   view)
     ("M-m"   comint-previous-matching-input)
     ("M-M"   comint-next-matching-input)
     ("C-c r" gud-run)
     ("C-c f" gud-finish)
     ("M-j"   gud-next)
     ("M-k"   gud-step)
     ("<f7>"  gud-step)
     ("<f8>"  gud-next)
     ("M-c"   gud-cont)
     ("M-C"   capitalize-word)
     ("C-c m" make)))

  ;; 退出gdb的时候关闭gdb对应的buffer
  (add-hook 'gdb-mode-hook 'kill-buffer-when-shell-command-exit)

  ;; 显示gdb的鼠标提示

(setq gdb-many-windows t)
(setq gdb-show-main t)
(setq gud-tooltip-mode t)
;(setq gud-chdir-before-run nil)
)


(eval-after-load "gdb-mi"
  `(gud-settings))

(provide 'gud-settings)
