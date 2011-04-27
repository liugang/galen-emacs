;;; config-server.el --- working with server/client

;; Copyright (C) 2010 Galen
;;
;; Author: galen.gang.liu@gmail.com
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(unless (string-equal "root" (getenv "USER"))
  ;; Only start server mode if I'm not root
  (require 'server)
  (server-start))

(setq oldframe (selected-frame))
(defun galen.gang.liu/client-start-hook ()
  (let* (
        (wd (- (frame-parameter nil 'width) 10))
        (hg (- (frame-parameter nil 'height) 5))
        (fn (frame-parameter nil 'font))
        (newframe (make-frame
                   `((width . ,wd)
                     (height . ,hg)
                     (font . ,fn)))))
;    (setq oldframe (selected-frame))
    (select-frame newframe)
    (setq server-window newframe))
  )

(defun galen.gang.liu/client-done-hook ()
    (delete-frame (selected-frame))
    (select-frame oldframe)
    (setq server-window oldframe)
  )

(custom-set-variables
 '(server-done-hook (quote (delete-frame)))
 '(server-window (quote switch-to-buffer-other-frame))
)
;;; config-server.el ends here
