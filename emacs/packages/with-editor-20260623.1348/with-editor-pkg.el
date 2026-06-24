;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "with-editor" "20260623.1348"
  "Use the Emacsclient as $EDITOR."
  '((emacs    "28.1")
    (compat   "31.0")
    (cond-let "1.1")
    (llama    "1.0"))
  :url "https://github.com/magit/with-editor"
  :commit "c319ef4d3a9dab479b4077cdc089d1ffac97d7db"
  :revdesc "c319ef4d3a9d"
  :keywords '("processes" "terminals")
  :authors '(("Jonas Bernoulli" . "emacs.with-editor@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.with-editor@jonas.bernoulli.dev")))
