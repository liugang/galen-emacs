(require 'evernote-mode)

(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; option

(eal-define-keys-commonly
 global-map
 `(
   ("\C-cec" evernote-create-note)
   ("\C-ceo" evernote-open-note)
   ("\C-ces" evernote-search-notes)
   ("\C-ceS" evernote-do-saved-search)
   ("\C-cew" evernote-write-note)
   ("\C-cep" evernote-post-region)
   ("\C-ceb" evernote-browser)
   ))

(provide 'galen-evernote-settings)

