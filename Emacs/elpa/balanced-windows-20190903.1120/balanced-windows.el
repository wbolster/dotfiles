;;; balanced-windows.el --- Keep windows balanced -*- lexical-binding: t; -*-

;; Author: wouter bolsterlee <wouter@bolsterl.ee>
;; Keywords: convenience
;; Package-Version: 20190903.1120
;; Package-Commit: 1da5354ad8a9235d13928e2ee0863f3642ccdd13
;; URL: https://github.com/wbolster/emacs-balanced-windows
;; Package-Requires: ((emacs "25"))
;; Version: 1.0.0

;; Copyright 2019 wouter bolsterlee. Licensed under the 3-Clause BSD License.

;;; Commentary:

;; Automatically keep windows balanced.

;;; Code:

(defvar evil-auto-balance-windows)

(defgroup balanced-windows nil
  "Keep windows balanced."
  :group 'windows
  :prefix "balanced-windows-")

(defcustom balanced-windows-functions
  '(delete-window quit-window split-window)
  "Functions needing advice to keep windows balanced."
  :group 'balanced-windows
  :type '(repeat function))

(defun balanced-windows--advice (&rest _ignored)
  "Balance windows (intended as :after advice); any args are ignored."
  (balance-windows))

;;;###autoload
(define-minor-mode balanced-windows-mode
  "Global minor mode to keep windows balanced at all times."
  :global t
  (dolist (fn balanced-windows-functions)
    (if balanced-windows-mode
        (advice-add fn :after #'balanced-windows--advice)
      (advice-remove fn #'balanced-windows--advice)))
  (when balanced-windows-mode
    (balance-windows))
  (when (featurep 'evil)
    (setq evil-auto-balance-windows balanced-windows-mode)))

(provide 'balanced-windows)
;;; balanced-windows.el ends here
