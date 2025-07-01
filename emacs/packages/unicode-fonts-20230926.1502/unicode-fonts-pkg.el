;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "unicode-fonts" "20230926.1502"
  "Configure Unicode fonts."
  '((font-utils      "0.7.8")
    (ucs-utils       "0.8.2")
    (list-utils      "0.4.2")
    (persistent-soft "0.8.10")
    (pcache          "0.3.1"))
  :url "http://github.com/rolandwalker/unicode-fonts"
  :commit "6245b97d8ddaeaf1de4dbe2cd85ca0f3b20ef81b"
  :revdesc "6245b97d8dda"
  :keywords '("i18n" "faces" "frames" "wp" "interface")
  :authors '(("Roland Walker" . "walker@pobox.com"))
  :maintainers '(("Roland Walker" . "walker@pobox.com")))
