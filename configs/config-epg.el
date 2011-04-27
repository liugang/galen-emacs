;;; config-epg.el ---

;; Copyright (C) 2007 Galen
;;
;; Author: galen.gang.liu@gmail.com
;; Version: $Id: config-epg.el,v 0.0 2007/09/10 13:04:59 ott Exp $
;; Keywords:
;; Requirements:
;; Status: not intended to be distributed yet

(add-to-list 'load-path "~/emacs/epg")
(require 'epa-setup)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;;; config-epg.el ends here

