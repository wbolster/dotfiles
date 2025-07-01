;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "closql" "20250601.1010"
  "Store EIEIO objects using EmacSQL."
  '((emacs   "26.1")
    (compat  "30.1")
    (emacsql "4.3.1"))
  :url "https://github.com/emacscollective/closql"
  :commit "05a2b048fd4e5c90aa971479cb9e71cf9aeba2bf"
  :revdesc "05a2b048fd4e"
  :keywords '("extensions")
  :authors '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.closql@jonas.bernoulli.dev")))
