;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "python-pytest" "20260117.2218"
  "Helpers to run pytest."
  '((emacs     "24.4")
    (dash      "2.18.0")
    (transient "0.3.7")
    (s         "1.12.0"))
  :url "https://github.com/wbolster/emacs-python-pytest"
  :commit "78b5ea1d19c7e365ac00649d13c733954b11f822"
  :revdesc "78b5ea1d19c7"
  :keywords '("pytest" "test" "python" "languages" "processes" "tools")
  :authors '(("wouter bolsterlee" . "wouter@bolsterl.ee"))
  :maintainers '(("wouter bolsterlee" . "wouter@bolsterl.ee")))
