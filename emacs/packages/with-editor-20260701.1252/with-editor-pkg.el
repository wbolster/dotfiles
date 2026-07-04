;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "with-editor" "20260701.1252"
  "Use the Emacsclient as $EDITOR."
  '((emacs    "28.1")
    (compat   "31.0")
    (cond-let "1.1")
    (llama    "1.0"))
  :url "https://github.com/magit/with-editor"
  :commit "45bfc6084f03e3aa7f4f8db20836d559186c5957"
  :revdesc "45bfc6084f03"
  :keywords '("processes" "terminals")
  :authors '(("Jonas Bernoulli" . "emacs.with-editor@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.with-editor@jonas.bernoulli.dev")))
