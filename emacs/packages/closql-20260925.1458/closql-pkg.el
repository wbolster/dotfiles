;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "closql" "20260925.1458"
  "Store EIEIO objects using EmacSQL."
  '((emacs    "28.1")
    (compat   "31.0")
    (cond-let "1.1")
    (emacsql  "4.4")
    (llama    "1.0"))
  :url "https://github.com/emacscollective/closql"
  :commit "f8df77d3e31f63faef8791ac8705786e996f00e3"
  :revdesc "f8df77d3e31f"
  :keywords '("extensions")
  :authors '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev")))
