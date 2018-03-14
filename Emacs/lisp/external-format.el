;;; external-format.el --- reformat text using an external program  -*- lexical-binding: t; -*-

;; Author: wouter bolsterlee <wouter@bolsterl.ee>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience, languages, processes, tools, formatting, external format
;; URL: https://github.com/wbolster/emacs-external-format
;;
;; This file is not part of GNU Emacs.

;;; License:

;; 3-clause "new bsd"; see readme for details.

;;; Commentary:

;; todo
;; - external file + generate patch + apply patch? see py-autopep8.el
;;   which took it from go-mode gofmt support.
;; - make evil dependency optional
;; - allow dynamic external-format-shell-command via an optional
;;   external-format-shell-command-function variable that
;;   can dynamically create a command line, e.g. to include
;;   line widths in a flag or things like that.

;;; Code:

(require 'thingatpt)

(defgroup external-format nil
  "external format"
  :group 'editing
  :prefix "external-format-")

(defcustom external-format-shell-command "cat"
  "Shell command line to act as a format filter.

The default is 'cat', which does not change its input.

This is a buffer-local variable, typically set from a major mode hook."
  :group 'external-format
  :type 'string)
(make-variable-buffer-local 'external-format-shell-command)

(defcustom external-format-buffer-name " *external-format*"
  "Name of the output buffer (hidden by default)."
  :group 'external-format
  :type 'string)

(defcustom external-format-error-buffer-name "*external-format errors*"
  "Name of the error output buffer."
  :group 'external-format
  :type 'string)

;;;###autoload
(defun external-format (beg end)
  "Format BEG til END using an external formatting program.

The number of leading and trailing newlines (if any) will be kept
the same, to avoid bad interaction with surrounding text.

This also tries to retain the cursor position using a heuristic
that assumes that the number of occurrences of the symbol at
point stays the same after piping through the external program."
  (interactive "r")
  (unless external-format-shell-command
    (user-error "No external program configured (external-format-shell-command)"))
  (let ((sym)
        (sym-regexp)
        (nth-occurrence)
        (offset-in-sym))
    (save-restriction
      (narrow-to-region beg end)
      (setq sym (thing-at-point 'symbol))
      (unless sym
        ;; point not at symbol, try finding one at the left
        (forward-symbol -1)
        (forward-symbol 1)
        (backward-char)
        (setq sym (thing-at-point 'symbol)))
      (when sym
        ;; keep track of the position
        (setq sym-regexp (regexp-quote (downcase sym))
              offset-in-sym (- (point)
                               (car (bounds-of-thing-at-point 'symbol))))
        (beginning-of-thing 'symbol)
        (setq nth-occurrence (how-many sym-regexp)))
      ;; reformat
      (external-format--call (point-min) (point-max) external-format-shell-command)
      (when sym
        ;; try to restore the position
        (goto-char (point-max))
        (when (re-search-backward sym-regexp nil t nth-occurrence)
          (forward-char offset-in-sym))))))

;;;###autoload
(defalias 'external-format-region 'external-format)

;;;###autoload
(defun external-format-buffer ()
  "Format the buffer using an external formatting program."
  (interactive)
  (external-format (point-min) (point-max)))


;; evil integration

(require 'evil)

;;;###autoload (autoload 'evil-external-format "external-format" nil t)
(evil-define-operator evil-external-format (beg end _type)
  "Evil operator to run external formatting."
  (interactive "<R>")
  (external-format beg end))


;; internal helpers

(defun external-format--call (beg end command)
  "Pipe BEG til END through COMMAND."
  (let ((out-buffer (get-buffer-create external-format-buffer-name))
        (err-buffer (get-buffer-create external-format-error-buffer-name))
        (leading-newlines 0)
        (trailing-newlines 0))
    (with-current-buffer err-buffer
      (erase-buffer))
    (copy-to-buffer out-buffer beg end)
    (with-current-buffer out-buffer
      (goto-char (point-min))
      (when (looking-at "\n\+")
        (setq leading-newlines (- (match-end 0) (point-min))))
      (goto-char (point-max))
      (when (looking-back "\n\+" nil t)
        (setq trailing-newlines (- (point-max) (match-beginning 0))))
      (shell-command-on-region
       (point-min) (point-max)
       command
       nil t err-buffer t)
      (goto-char (point-min))
      (skip-chars-forward "\n")
      (delete-region (point-min) (point))
      (dotimes (_ leading-newlines)
        (insert "\n"))
      (goto-char (point-max))
      (skip-chars-backward "\n")
      (delete-region (point) (point-max))
      (dotimes (_ trailing-newlines)
        (insert "\n")))
    (delete-region beg end)
    (goto-char beg)
    (insert-buffer-substring out-buffer)
    (if (zerop (buffer-size err-buffer))
        (kill-buffer err-buffer)
      (display-buffer err-buffer))))

(provide 'external-format)
;;; external-format.el ends here
