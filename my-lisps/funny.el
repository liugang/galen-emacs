;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-11-29 12:43:53 Monday by taoshanwen>

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

;; google-maps-el – Emacs中的谷歌地图
;; http://emacser.com/emacs-google-map.htm
(require 'google-maps-settings)

;; 非常强大的文本画图的工具
(require 'artist-settings)

;; 用渐变颜色显示你最近的修改
;; http://emacser.com/highlight-tail.htm
;; 与semantic冲突，启动了它后，打开大文件的时候，会发现buffer大范围的刷屏
;; (require 'highlight-tail-settings)

;; 用对应的颜色显示你的颜色字符串, i.e. red blue #96bf33
(require 'rainbow-mode)

(provide 'funny)
