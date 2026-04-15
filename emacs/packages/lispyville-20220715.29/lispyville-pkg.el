;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "lispyville" "20220715.29"
  "A minor mode for integrating evil with lispy."
  '((lispy  "0")
    (evil   "1.2.12")
    (cl-lib "0.5")
    (emacs  "24.4"))
  :url "https://github.com/noctuid/lispyville"
  :commit "14ee8711d58b649aeac03581d22b10ab077f06bd"
  :revdesc "14ee8711d58b"
  :keywords '("vim" "evil" "lispy" "lisp" "parentheses")
  :authors '(("Fox Kiester" . "noct@posteo.net"))
  :maintainers '(("Fox Kiester" . "noct@posteo.net")))
