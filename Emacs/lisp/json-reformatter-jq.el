;;; json-reformatter-jq.el --- reformat json using jq  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  wouter bolsterlee

;; Author: wouter bolsterlee <wouter@bolsterl.ee>
;; Keywords: languages
;; URL: https://github.com/wbolster/emacs-json-reformatter-jq
;; Package-Requires: ((emacs "24") (reformatter "0.3"))
;; Version: 0

;;; Commentary:

;; Commands for easy JSON (and jsonlines) reformatting using jq.

;;; Code:

(require 'reformatter)

(defgroup json-reformatter-jq nil
  "JSON reformatting using jq."
  :group 'json-reformat)

(defcustom json-reformatter-jq-command "jq"
  "Name of the jq executable."
  :group 'json-reformatter-jq
  :type 'string)

(defcustom json-reformatter-jq-sort-keys t
  "Whether to sort keys."
  :group 'json-reformatter-jq
  :type 'string)

(defcustom json-reformatter-jq-extra-args nil
  "Extra arguments to pass to jq."
  :group 'json-reformatter-jq
  :type '(repeat string))

;;;###autoload (autoload 'json-reformatter-jq-buffer "json-reformatter-jq" nil t)
;;;###autoload (autoload 'json-reformatter-jq-region "json-reformatter-jq" nil t)
;;;###autoload (autoload 'json-reformatter-jq-on-save-mode "json-reformatter-jq" nil t)
(reformatter-define json-reformatter-jq
  :program json-reformatter-jq-command
  :args (json-reformatter-jq--make-args)
  :lighter " JSONFmt"
  :group 'json-reformatter-jq)

;;;###autoload (autoload 'jsonlines-reformatter-jq-buffer "json-reformatter-jq" nil t)
;;;###autoload (autoload 'jsonlines-reformatter-jq-region "json-reformatter-jq" nil t)
;;;###autoload (autoload 'jsonlines-reformatter-jq-on-save-mode "json-reformatter-jq" nil t)
(reformatter-define jsonlines-reformatter-jq
  :program json-reformatter-jq-command
  :args (append '("--compact-output") (json-reformatter-jq--make-args))
  :lighter " JSONLFmt"
  :group 'json-reformatter-jq)

(defun json-reformatter-jq--make-args ()
  "Helper to build the argument list for jq."
  (append
   (when json-reformatter-jq-sort-keys '("--sort-keys"))
   json-reformatter-jq-extra-args
   '("." "-")))

(provide 'json-reformatter-jq)
;;; json-reformatter-jq.el ends here
