;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "with-editor" "20260925.1502"
  "Use the Emacsclient as $EDITOR."
  '((emacs    "28.1")
    (compat   "31.0")
    (cond-let "1.1")
    (llama    "1.0"))
  :url "https://github.com/magit/with-editor"
  :commit "7bec41144ea197961c76c769cfc0acaf689ebac0"
  :revdesc "7bec41144ea1"
  :keywords '("processes" "terminals")
  :authors '(("Jonas Bernoulli" . "emacs.with-editor@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.with-editor@jonas.bernoulli.dev")))
