;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20260614.1700"
  "A Git porcelain inside Emacs."
  '((emacs         "28.1")
    (compat        "31.0")
    (cond-let      "0.2")
    (llama         "1.0")
    (magit-section "4.5")
    (seq           "2.24")
    (transient     "0.13")
    (with-editor   "3.4"))
  :url "https://github.com/magit/magit"
  :commit "e35d646c2cc9aa825e1f2007ec75dac872040b69"
  :revdesc "e35d646c2cc9"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
