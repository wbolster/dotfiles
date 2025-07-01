;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "vue-mode" "20240101.333"
  "Major mode for vue component based on mmm-mode."
  '((mmm-mode      "0.5.5")
    (vue-html-mode "0.2")
    (ssass-mode    "0.2")
    (edit-indirect "0.1.4"))
  :url "https://github.com/AdamNiederer/vue-mode"
  :commit "3a8056bc6ea6458265efb91067c7467860d2c118"
  :revdesc "3a8056bc6ea6"
  :keywords '("languages")
  :authors '(("codefalling" . "code.falling@gmail.com"))
  :maintainers '(("codefalling" . "code.falling@gmail.com")))
