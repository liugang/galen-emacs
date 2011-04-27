;; Copyright (C) 2010 ahei

;; Author: ahei <ahei0802@gmail.com>
;; URL: http://code.google.com/p/dea/source/browse/trunk/my-lisps/cflow-mode-settings.el
;; Time-stamp: <2010-11-29 14:32:15 Monday by taoshanwen>

;; This  file is free  software; you  can redistribute  it and/or
;; modify it under the terms of the GNU General Public License as
;; published by  the Free Software Foundation;  either version 3,
;; or (at your option) any later version.

;; This file is  distributed in the hope that  it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR  A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You  should have  received a  copy of  the GNU  General Public
;; License along with  GNU Emacs; see the file  COPYING.  If not,
;; write  to  the Free  Software  Foundation,  Inc., 51  Franklin
;; Street, Fifth Floor, Boston, MA 02110-1301, USA.

(require 'cflow-mode)

(eal-define-keys
 'cflow-mode-map
 `(("SPC"     View-scroll-half-page-forward)
   ("u"       View-scroll-half-page-backward)
   ("d"       scroll-up)
   ("'"       switch-to-other-buffer)
   ("t"       sb-toggle-keep-buffer)
   ("m"       back-to-indentation)
   
   ("RET"     cflow-find-function)))

(defun cflow-mode-settings ()
  "Settings for `cflow-mode'."
  (defun cflow-function (function-name)
    "Get call graph of inputed function. "
    (interactive (list (car (senator-jump-interactive "Function name: " nil nil nil))))
    (let ((cmd (format "cflow -b -n -T --main=\"%s\" %s" function-name buffer-file-name))
          (cflow-buf-name (format "**cflow-%s:%s**"
                                  (file-name-nondirectory buffer-file-name)
                                  function-name))
          (cflow-buf (get-buffer-create cflow-buf-name)))
      (set-buffer cflow-buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (shell-command-to-string cmd))
      (pop-to-buffer cflow-buf)
      (goto-char (point-min))
      (cflow-mode))))

(eval-after-load "cflow-mode"
  `(cflow-mode-settings))

(provide 'cflow-mode-settings)
