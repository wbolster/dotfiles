;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "ucs-utils" "20230119.2237"
  "Utilities for Unicode characters."
  '((emacs           "24.3")
    (persistent-soft "0.8.10")
    (pcache          "0.5.1")
    (list-utils      "0.4.6"))
  :url "http://github.com/rolandwalker/ucs-utils"
  :commit "91b9e0207fff5883383fd39c45ad5522e9b90e65"
  :revdesc "91b9e0207fff"
  :keywords '("i18n" "extensions")
  :authors '(("Roland Walker" . "walker@pobox.com"))
  :maintainers '(("Roland Walker" . "walker@pobox.com")))
