;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "python-pytest" "20250706.2057"
  "Helpers to run pytest."
  '((emacs     "24.4")
    (dash      "2.18.0")
    (transient "0.3.7")
    (s         "1.12.0"))
  :url "https://github.com/wbolster/emacs-python-pytest"
  :commit "02784b0701cf0698277e3252c2e394cf02fab055"
  :revdesc "02784b0701cf"
  :keywords '("pytest" "test" "python" "languages" "processes" "tools")
  :authors '(("wouter bolsterlee" . "wouter@bolsterl.ee"))
  :maintainers '(("wouter bolsterlee" . "wouter@bolsterl.ee")))
