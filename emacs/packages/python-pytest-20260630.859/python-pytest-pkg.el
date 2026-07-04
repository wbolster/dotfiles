;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "python-pytest" "20260630.859"
  "Helpers to run pytest."
  '((emacs     "28.1")
    (dash      "2.18.0")
    (transient "0.3.7")
    (s         "1.12.0"))
  :url "https://github.com/wbolster/emacs-python-pytest"
  :commit "8c0e048a771f355903a8c2ce78fa0974a077214e"
  :revdesc "8c0e048a771f"
  :keywords '("pytest" "test" "python" "languages" "processes" "tools")
  :authors '(("wouter bolsterlee" . "wouter@bolsterl.ee"))
  :maintainers '(("wouter bolsterlee" . "wouter@bolsterl.ee")))
