;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "unicode-fonts" "20260202.1156"
  "Configure Unicode fonts."
  '((font-utils      "0.7.8")
    (ucs-utils       "0.8.2")
    (list-utils      "0.4.2")
    (persistent-soft "0.8.10")
    (pcache          "0.3.1"))
  :url "https://github.com/rolandwalker/unicode-fonts"
  :commit "d4a0648a2206d9a896433817110957b3b9e1c17a"
  :revdesc "d4a0648a2206"
  :keywords '("i18n" "faces" "frames" "wp" "interface")
  :authors '(("Roland Walker" . "walker@pobox.com"))
  :maintainers '(("Roland Walker" . "walker@pobox.com")))
