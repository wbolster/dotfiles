;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "with-editor" "20260729.1509"
  "Use the Emacsclient as $EDITOR."
  '((emacs    "28.1")
    (compat   "31.0")
    (cond-let "1.1")
    (llama    "1.0"))
  :url "https://github.com/magit/with-editor"
  :commit "249f872ffd68c8744a772e855d1186ae3f330571"
  :revdesc "249f872ffd68"
  :keywords '("processes" "terminals")
  :authors '(("Jonas Bernoulli" . "emacs.with-editor@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.with-editor@jonas.bernoulli.dev")))
