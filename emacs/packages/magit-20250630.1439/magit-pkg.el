;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "magit" "20250630.1439"
  "A Git porcelain inside Emacs."
  '((emacs         "27.1")
    (compat        "30.1")
    (llama         "0.6.3")
    (magit-section "4.3.6")
    (seq           "2.24")
    (transient     "0.9.0")
    (with-editor   "3.4.4"))
  :url "https://github.com/magit/magit"
  :commit "1f5f15558f8899dac082dbd82e8ff15f4b58efe7"
  :revdesc "1f5f15558f88"
  :keywords '("git" "tools" "vc")
  :authors '(("Marius Vollmer" . "marius.vollmer@gmail.com")
             ("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev"))
  :maintainers '(("Jonas Bernoulli" . "emacs.magit@jonas.bernoulli.dev")
                 ("Kyle Meyer" . "kyle@kyleam.com")))
