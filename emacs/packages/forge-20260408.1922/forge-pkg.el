;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "forge" "20260408.1922"
  "Access Git forges from Magit."
  '((emacs         "29.1")
    (compat        "30.1")
    (closql        "2.4")
    (cond-let      "0.2")
    (emacsql       "4.3")
    (ghub          "5.1")
    (llama         "1.0")
    (magit         "4.5")
    (markdown-mode "2.8")
    (seq           "2.24")
    (transient     "0.12")
    (yaml          "1.2"))
  :url "https://github.com/magit/forge"
  :commit "69801d0da19d62b4b68b1f1756900e47ce7e8769"
  :revdesc "69801d0da19d"
  :keywords '("git" "tools" "vc")
  :authors '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev")))
