
(eal-define-keys
 `(c-mode-base-map)
 `(("C-8" sourcepair-load)))

(autoload 'sourcepair-load "sourcepair"
  "Load the corresponding C/C++ header or source file for the current buffer.

This function can be invoked by \\[sourcepair-load].  It will load the the
corresponding header or source file for the current buffer.  For example, if
you are looking at the file FooParser.cpp and press \\[sourcepair-load], the
file FooParser.h will be loaded.  It also works the other way as well.

There are six global variables that can be used to adjust how the function
works:

 `sourcepair-source-extensions'
 `sourcepair-header-extensions'
 `sourcepair-source-path'
 `sourcepair-header-path'
 `sourcepair-recurse-ignore'
 `sourcepair-private-header-suffixes'

See the documentation for these variables for more info.
" t)

(defun sourcepair-settings ()
  "Settings for `sourcepair'."
  (setq sourcepair-source-path '( "." "../src"))
  (setq sourcepair-header-path user-head-file-dir)
  (setq sourcepair-recurse-ignore '("CVS" "bin" "lib" "Obj" "Debug" "Release" ".svn")))

(eval-after-load "sourcepair"
  `(sourcepair-settings))

(provide 'sourcepair-settings)
