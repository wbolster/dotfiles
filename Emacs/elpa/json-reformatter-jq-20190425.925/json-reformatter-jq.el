;;; json-reformatter-jq.el --- reformat json using jq  -*- lexical-binding: t; -*-

;; Copyright © 2019 wouter bolsterlee

;; Author: wouter bolsterlee <wouter@bolsterl.ee>
;; Keywords: languages
;; Package-Version: 20190425.925
;; URL: https://github.com/wbolster/emacs-json-reformatter-jq
;; Package-Requires: ((emacs "24") (reformatter "0.3"))
;; Version: 1

;; (this is the osi approved 3-clause "new bsd license".)

;; all rights reserved.

;; redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;; * redistributions of source code must retain the above copyright
;;   notice, this list of conditions and the following disclaimer.

;; * redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in
;;   the documentation and/or other materials provided with the
;;   distribution.

;; * neither the name of the author nor the names of the contributors
;;   may be used to endorse or promote products derived from this
;;   software without specific prior written permission.

;; this software is provided by the copyright holders and contributors
;; "as is" and any express or implied warranties, including, but not
;; limited to, the implied warranties of merchantability and fitness
;; for a particular purpose are disclaimed. in no event shall the
;; copyright holder or contributors be liable for any direct,
;; indirect, incidental, special, exemplary, or consequential damages
;; (including, but not limited to, procurement of substitute goods or
;; services; loss of use, data, or profits; or business interruption)
;; however caused and on any theory of liability, whether in contract,
;; strict liability, or tort (including negligence or otherwise)
;; arising in any way out of the use of this software, even if advised
;; of the possibility of such damage.

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
  :type 'boolean)

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
