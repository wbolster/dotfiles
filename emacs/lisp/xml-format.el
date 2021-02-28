;;; xml-format.el --- XML reformatter using xmllint -*- lexical-binding: t; -*-

;; Author: wouter bolsterlee <wouter@bolsterl.ee>
;; Keywords: languages
;; URL: https://github.com/wbolster
;; Package-Requires: ((emacs "25") (reformatter "0.3"))
;; Version: 0.1.0

;; Copyright 2019 wouter bolsterlee. Licensed under the 3-Clause BSD License.

;;; Commentary:

;; Reformatter for XML using xmllint.

;;; Code:

(require 'reformatter)

(defgroup xml-format nil "XML refomatting using xmllint"
  :group 'xml)

(defcustom xml-format-xmllint-executable "xmllint"
  "Name of the xmllint executable."
  :group 'xmllint
  :type 'string)

(defcustom xml-format-xmllint-args '("--format" "-")
  "Arguments to pass to xmllint."
  :group 'xmllint
  :type '(repeat string))

;;;###autoload (autoload 'xml-format-buffer "xml-format" nil t)
;;;###autoload (autoload 'xml-format-region "xml-format" nil t)
;;;###autoload (autoload 'xml-format-on-save-mode "xml-format" nil t)
(reformatter-define xml-format
  :group 'xml-format
  :program xml-format-xmllint-executable
  :args xml-format-xmllint-args
  :mode "XMLFmt")

(provide 'xml-format)
;;; xml-format.el ends here
