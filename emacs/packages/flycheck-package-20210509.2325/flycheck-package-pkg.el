;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "flycheck-package" "20210509.2325"
  "A Flycheck checker for elisp package authors."
  '((emacs        "24.1")
    (flycheck     "0.22")
    (package-lint "0.2"))
  :url "https://github.com/purcell/flycheck-package"
  :commit "ecd03f83790611888d693c684d719e033f69cb40"
  :revdesc "ecd03f837906"
  :keywords '("lisp")
  :authors '(("Steve Purcell" . "steve@sanityinc.com")
             ("Fanael Linithien" . "fanael4@gmail.com"))
  :maintainers '(("Steve Purcell" . "steve@sanityinc.com")
                 ("Fanael Linithien" . "fanael4@gmail.com")))
