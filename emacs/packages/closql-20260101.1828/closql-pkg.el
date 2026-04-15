;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "closql" "20260101.1828"
  "Store EIEIO objects using EmacSQL."
  '((emacs    "28.1")
    (compat   "30.1")
    (cond-let "0.2")
    (emacsql  "4.3"))
  :url "https://github.com/emacscollective/closql"
  :commit "947426d0c93e5ad5374c464b2f121c36cdaf2132"
  :revdesc "947426d0c93e"
  :keywords '("extensions")
  :authors '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev")))
