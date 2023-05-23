;;; reformatter-dwim.el --- dwim reformatter helpers -*- lexical-binding: t; -*-

;;; Commentary:

;; Several do-what-i-mean helpers for reformatter.el based formatters.

;;; Code:

(require 'subr-x)
(require 'evil)
(require 'reformatter)

(defgroup reformatter-dwim nil
  "reformatter dwim helpers"
  :group 'reformatter
  :prefix "reformatter-dwim-")

(defcustom reformatter-dwim-reformatter nil
  "The currently active reformatter.

This should be a symbol, which is used to find the corresponding
reformatting commands. For example, setting this variable to 'foo
will cause the dwim function to use 'foo-region and 'foo-buffer.

Typically, a major mode hook should set this buffer-local variable."
  :group 'reformatter-dwim
  :type 'symbol)

(make-variable-buffer-local 'reformatter-dwim-reformatter)

;;;###autoload
(defun reformatter-dwim-region (beg end &optional display-errors)
  "Reformat the region from BEG to END.

When called interactively, or with prefix argument
DISPLAY-ERRORS, shows a buffer if the formatting fails."
  (interactive "rp")
  (let ((fun (reformatter-dwim--command 'region)))
    (funcall-interactively fun beg end display-errors)))

;;;###autoload
(defun reformatter-dwim-buffer (&optional display-errors)
  "Reformat the current buffer.

When called interactively, or with prefix argument
DISPLAY-ERRORS, shows a buffer if the formatting fails."
  (interactive "p")
  (let ((fun (reformatter-dwim--command 'buffer)))
    (funcall-interactively fun display-errors)))

;;;###autoload
(define-minor-mode reformatter-dwim-on-save-mode
  "Toggle automatic reformatting when saving."
  :lighter " 🧽"
  (let ((fun (reformatter-dwim--command 'on-save-mode)))
    (funcall fun 'toggle)))

;;;###autoload
(evil-define-operator reformatter-dwim-evil (beg end type)
  "Evil operator to reformat a region."
  :type line
  :repeat nil
  (interactive "<R>")
  (reformatter-dwim-region beg end))

(defun reformatter-dwim-select (formatter)
  "Use the FORMATTER (a symbol) for ‘dwim’ formatting."
  (setq reformatter-dwim-reformatter formatter))

(defun reformatter-dwim--command (suffix)
  "Get the currently configured reformatter function.

The symbol SUFFIX is used to construct the complete name."
  (unless reformatter-dwim-reformatter
    (user-error "No reformatter configured for current buffer (reformatter-dwim-reformatter not set)"))
  (if-let* ((name (format "%s-%s" (symbol-name reformatter-dwim-reformatter) (symbol-name suffix)))
            (fun (intern name))
            (fun-is-a-function (functionp fun)))
      fun
    (user-error
     "Reformatter %s not found. Is the package defining the reformatter loaded?"
     reformatter-dwim-reformatter)))

(provide 'reformatter-dwim)
;;; reformatter-dwim.el ends here
