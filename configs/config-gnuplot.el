;;; config-gnuplot.el ---

;; Copyright (C) Galen
;;
;; Author: Galen <galen.gang.liu@gmail.com>
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(add-to-list 'load-path "~/emacs/gnuplot")
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)

(add-to-list 'auto-mode-alist '("\\.gp$" . gnuplot-mode))

;;; config-gnuplot.el ends here
