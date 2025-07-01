;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "python-pytest" "20240826.948"
  "Helpers to run pytest."
  '((emacs     "24.4")
    (dash      "2.18.0")
    (transient "0.3.7")
    (s         "1.12.0"))
  :url "https://github.com/wbolster/emacs-python-pytest"
  :commit "9390f9fc35f98884131e5b3ec572be04d5409b73"
  :revdesc "9390f9fc35f9"
  :keywords '("pytest" "test" "python" "languages" "processes" "tools")
  :authors '(("wouter bolsterlee" . "wouter@bolsterl.ee"))
  :maintainers '(("wouter bolsterlee" . "wouter@bolsterl.ee")))
