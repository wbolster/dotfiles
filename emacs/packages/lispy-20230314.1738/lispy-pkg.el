;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "lispy" "20230314.1738"
  "Vi-like Paredit."
  '((emacs      "24.3")
    (ace-window "0.9.0")
    (iedit      "0.9.9")
    (swiper     "0.13.4")
    (hydra      "0.14.0")
    (zoutline   "0.2.0"))
  :url "https://github.com/abo-abo/lispy"
  :commit "fe44efd21573868638ca86fc8313241148fabbe3"
  :revdesc "fe44efd21573"
  :keywords '("lisp")
  :authors '(("Oleh Krehel" . "ohwoeowho@gmail.com"))
  :maintainers '(("Oleh Krehel" . "ohwoeowho@gmail.com")))
