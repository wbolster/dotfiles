;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "forge" "20250327.1806"
  "Access Git forges from Magit."
  '((emacs         "29.1")
    (compat        "30.0.2.0")
    (closql        "2.2.1")
    (emacsql       "4.2.0")
    (ghub          "4.2.2")
    (let-alist     "1.0.6")
    (llama         "0.6.1")
    (magit         "4.3.1")
    (markdown-mode "2.7")
    (seq           "2.24")
    (transient     "0.8.5")
    (yaml          "1.2.0"))
  :url "https://github.com/magit/forge"
  :commit "1a27d246b4b2b9c62214186e1976202253a4b6a5"
  :revdesc "1a27d246b4b2"
  :keywords '("git" "tools" "vc")
  :authors '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev")))
