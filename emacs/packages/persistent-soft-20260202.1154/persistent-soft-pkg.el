;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "persistent-soft" "20260202.1154"
  "Persistent storage, returning nil on failure."
  '((cl-lib     "0.7.1")
    (pcache     "0.3.1")
    (list-utils "0.4.2"))
  :url "https://github.com/rolandwalker/persistent-soft"
  :commit "c94b34332529df573bad8a97f70f5a35d5da7333"
  :revdesc "c94b34332529"
  :keywords '("data" "extensions")
  :authors '(("Roland Walker" . "walker@pobox.com"))
  :maintainers '(("Roland Walker" . "walker@pobox.com")))
