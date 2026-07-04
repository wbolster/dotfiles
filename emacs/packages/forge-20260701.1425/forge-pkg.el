;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "forge" "20260701.1425"
  "Access Git forges from Magit."
  '((emacs         "29.1")
    (compat        "31.0")
    (closql        "2.4")
    (cond-let      "1.1")
    (emacsql       "4.4")
    (ghub          "5.2.2")
    (llama         "1.0")
    (magit         "4.6")
    (markdown-mode "2.8")
    (transient     "0.13")
    (yaml          "1.2"))
  :url "https://github.com/magit/forge"
  :commit "9628f76740aec9270e9fb31457ff4cb38d9f3f16"
  :revdesc "9628f76740ae"
  :keywords '("git" "tools" "vc")
  :authors '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev")))
