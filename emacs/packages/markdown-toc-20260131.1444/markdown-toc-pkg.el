;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "markdown-toc" "20260131.1444"
  "A simple TOC generator for markdown file."
  '((emacs         "28.1")
    (markdown-mode "2.1")
    (dash          "2.11.0")
    (s             "1.9.0"))
  :url "https://github.com/ardumont/markdown-toc"
  :commit "d22633b654193bcab322ec51b6dd3bb98dd5f69f"
  :revdesc "d22633b65419"
  :keywords '("markdown" "toc" "tools")
  :authors '(("Antoine R. Dumont" . "(@ardumont)"))
  :maintainers '(("Antoine R. Dumont" . "(@ardumont)")
                 ("Jen-Chieh Shen" . "jcs090218@gmail.com")))
