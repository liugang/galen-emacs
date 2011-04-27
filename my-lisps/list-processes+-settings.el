;; Copyright (C) 2010 ahei

;; Author: ahei <ahei0802@gmail.com>
;; URL: http://code.google.com/p/dea/source/browse/trunk/my-lisps/list-processes+-settings.el
;; Time-stamp: <2010-12-02 00:17:42 Thursday by taoshanwen>

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

(autoload 'list-processes+ "list-processes+" "增强的`list-processes'命令" t)

(eal-define-keys
 'list-processes-mode-map
 `(("g" list-processes+)))

(defun list-processes+-settings ()
  "Settings for `list-processes+'.")

(eval-after-load "list-processes+"
  `(list-processes+-settings))

(provide 'list-processes+-settings)
