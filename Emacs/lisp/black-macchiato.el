;;; black-macchiato.el --- reformat python using black-macchiato  -*- lexical-binding: t; -*-

(require 'evil)
(require 'reformatter)

(defgroup black-macchiato nil
  "Python formatting using black/black-macchiato"
  :group 'python
  :prefix "black-macchiato-")

(defcustom black-macchiato-command "black-macchiato"
  "Name of the black-macchiato executable."
  :group 'black-macchiato
  :type 'string)

(reformatter-define black-macchiato
  :program black-macchiato-command
  :args nil
  :lighter " ❤"
  :group 'black-macchiato)

(with-eval-after-load 'evil
  (evil-define-operator black-macchiato-evil (beg end _type)
    :type line
    :repeat nil
    (interactive "<R>")
    (black-macchiato-region beg end)))

(provide 'black-macchiato)
;;; black-macchiato.el ends here
