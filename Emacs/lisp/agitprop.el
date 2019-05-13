;;; agitprop.el --- Get rid of GNU/agitprop -*- lexical-binding: t; -*-

;;; Commentary:

;; Resist the GNU/extremists.

;;; Code:

(require 'help)

(defun agitprop--empty-message ()
  "Show an empty message."
  (message ""))

(defun agitprop-resist ()
  "Get rid of GNU/agitprop."
  ;; The 'inhibit-startup-echo-area-message' variable
  ;; requires hard-coding a user name for it to work,
  ;; Annoying. Instead, turn the function that actually
  ;; shows the propaganda message into a no-op. Bye bye.
  (defalias 'display-startup-echo-area-message 'agitprop--empty-message)

  ;; Unbind useless shortcuts to GPL, etc.
  (define-key help-map (kbd "g") nil)  ;; describe-gnu-project
  (define-key help-map (kbd "C-c") nil)  ;; describe-copying
  (define-key help-map (kbd "C-m") nil)  ;; view-order-manuals
  (define-key help-map (kbd "C-o") nil)  ;; describe-distributions
  (define-key help-map (kbd "C-w") nil))  ;; describe-no-warranty

(provide 'agitprop)
;;; agitprop.el ends here
