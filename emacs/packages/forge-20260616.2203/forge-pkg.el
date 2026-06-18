;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "forge" "20260616.2203"
  "Access Git forges from Magit."
  '((emacs         "29.1")
    (compat        "31.0")
    (closql        "2.4")
    (cond-let      "1.1")
    (emacsql       "4.4")
    (ghub          "5.2.1")
    (llama         "1.0")
    (magit         "4.5")
    (markdown-mode "2.8")
    (seq           "2.24")
    (transient     "0.13")
    (yaml          "1.2"))
  :url "https://github.com/magit/forge"
  :commit "883d5f1f4cd695bfc2ebefeff4cef06205438fc3"
  :revdesc "883d5f1f4cd6"
  :keywords '("git" "tools" "vc")
  :authors '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.forge@jonas.bernoulli.dev")))
