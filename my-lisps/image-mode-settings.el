;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-11-30 10:01:05 Tuesday by taoshanwen>

(auto-image-file-mode 1)
(add-to-list 'auto-mode-alist '("\\.\\(bmp\\|BMP\\)$" . image-mode))

(eal-define-keys
 'image-mode-map
 `(("'" switch-to-other-buffer)))

(defun image-mode-settings ()
  "Settings for `image-mode'."
  (require 'ppm-gen)  

  (defadvice image-type (around image-type-bmp first (source &optional type data-p) activate)  
    (setq ad-return-value  
          (cond (data-p (if (string-match "^BM" source) 'bmp ad-do-it))  
                (t (if (string-match ".*\\.bmp" source) 'bmp ad-do-it)))))  

  (defadvice create-image (around create-image-bmp (file-or-data &optional type data-p &rest props) activate)  
    (setq ad-return-value  
          (cond ((eq 'bmp type) (let (ppm-obj ppm-data)  
                                  (setq ppm-obj (ppm-from-bmp file-or-data))  
                                  (setq ppm-data (with-temp-buffer  
                                                   (set-buffer-multibyte nil)  
                                                   (with-slots ((w width)  
                                                                (h height)  
                                                                pixels) ppm-obj  
                                                     (insert (format "P6 %d %d 255\r" w h))  
                                                     (insert (concat pixels)))  
                                                   (buffer-substring-no-properties (point-min) (point-max))))  
                                  (create-image ppm-data 'pbm t)))  
                (t ad-do-it)))))

(eval-after-load "image-mode"
  `(image-mode-settings))

(provide 'image-mode-settings)
