;;; evil-numbers.el --- Increment/decrement numbers like in VIM -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2011 by Michael Markert
;;               2020 by Julia Path
;; Author: Michael Markert <markert.michael@googlemail.com>
;; Maintainer: Julia Path <julia@jpath.de>
;; Contributors: Matthew Fidler <matthew.fidler@gmail.com>
;;               Michael Markert <markert.michael@gmail.com>
;;               Julia Path <julia@jpath.de>
;;               Campbell Barton <ideasman42@gmail.com>
;; URL: http://github.com/juliapath/evil-numbers
;; Git-Repository: git://github.com/juliapath/evil-numbers.git
;; Created: 2011-09-02
;; Package-Version: 20260103.850
;; Package-Revision: 616aff9e5cee
;; Package-Requires: ((emacs "24.1") (evil "1.2.0") (shift-number "0.2"))
;; Keywords: convenience tools

;;; Commentary:

;; Increment/Decrement binary, octal, decimal and hex literals.
;;
;; Works like C-a/C-x in VIM, i.e. searches for a number up to EOL and
;; then increments or decrements, maintaining zero padding.
;;
;; Known Bugs:
;; See http://github.com/juliapath/evil-numbers/issues
;;
;; Install:
;;
;; (require 'evil-numbers)
;;
;; and bind, for example:
;;
;; (global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
;; (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)
;; (global-set-key (kbd "C-c C-+") 'evil-numbers/inc-at-pt-incremental)
;; (global-set-key (kbd "C-c C--") 'evil-numbers/dec-at-pt-incremental)
;;
;; or only in evil's normal and visual state:
;;
;; (evil-define-key '(normal visual) 'global (kbd "C-c +")
;;                  'evil-numbers/inc-at-pt)
;; (evil-define-key '(normal visual) 'global (kbd "C-c -")
;;                  'evil-numbers/dec-at-pt)
;; (evil-define-key '(normal visual) 'global (kbd "C-c C-+")
;;                  'evil-numbers/inc-at-pt-incremental)
;; (evil-define-key '(normal visual) 'global (kbd "C-c C--")
;;                  'evil-numbers/dec-at-pt-incremental)
;;
;; Usage:
;; Go and play with your numbers!

;;; Code:

(require 'evil)
(require 'shift-number)

;; ---------------------------------------------------------------------------
;; Custom Variables

(defgroup evil-numbers nil
  "Support number increment/decrement."
  :group 'convenience)

(define-obsolete-variable-alias
  'evil-numbers/padDefault 'evil-numbers-pad-default "evil-numbers v0.6")

(defcustom evil-numbers-pad-default nil
  "Whether numbers are padded by default."
  :type 'boolean)

(defcustom evil-numbers-separator-chars nil
  "Separator characters allowed in numeric literals for visual grouping.

This value is a string containing separator characters,
typically \"_\" or \",\" which are allowed in numeric literals in some systems.

Set to nil to disable this functionality."
  :type '(choice (const nil) string))

(defcustom evil-numbers-case nil
  "Case to use for hexadecimal numbers."
  :type
  '(choice
    (const :tag "Current Case" nil)
    (const :tag "Upper Case" upcase)
    (const :tag "Lower Case" downcase)))

(defcustom evil-numbers-use-cursor-at-end-of-number nil
  "When non-nil, recognize numbers directly before the cursor."
  :type 'boolean)

(defcustom evil-numbers-negative t
  "When non-nil, recognize and preserve negative numbers.
When nil, the minus sign before a number is ignored, treating -5 as 5."
  :type 'boolean)


;; ---------------------------------------------------------------------------
;; Public Functions

;;;###autoload (autoload 'evil-numbers/inc-at-pt "evil-numbers" nil t)
(evil-define-operator
 evil-numbers/inc-at-pt (amount beg end type &optional incremental padded)
 "Increment the number at point or after point before `end-of-line' by AMOUNT.
When region is selected, increment all numbers in the region by AMOUNT.

INCREMENTAL causes the first number to be increased by 1*AMOUNT,
the second by 2*AMOUNT and so on.

PADDED is whether numbers should be padded (e.g. decrementing 10 -> 09).
-    nil: is default behavior set by `evil-numbers-pad-default',
-      t: is the opposite of `evil-numbers-pad-default',
- `'(t)': enables padding and `'(nil)' disables padding.

Numbers with a leading zero are always padded."
 :motion nil (interactive "*<c><R>")

 (setq amount (or amount 1))
 (setq padded
       (cond
        ((consp padded)
         (car padded))
        (padded
         (not evil-numbers-pad-default))
        (t
         evil-numbers-pad-default)))

 (let ((do-increment
        (lambda (amt range &optional range-check-fn)
          (shift-number-increment-at-point-with-search
           :amount amt
           :range range
           :pad-default padded
           :negative evil-numbers-negative
           :separator-chars evil-numbers-separator-chars
           :case evil-numbers-case
           :motion t
           :range-check-fn range-check-fn))))
   (cond
    ;; Handle selection (block or line).
    ;; Increment each number in the selection.
    ((and beg end type)
     (let* ((count 1)
            (process-region
             (lambda (beg end)
               (evil-with-restriction
                beg end (goto-char beg)
                (while (funcall do-increment
                                (* amount count)
                                (cons (point) (point-max)))
                  (when incremental
                    (incf count)))))))
       (save-excursion
         (if (eq type 'block)
             (evil-apply-on-block process-region beg end nil)
           (funcall process-region beg end)))))

    ;; Handle the simple case, either the cursor is over a number,
    ;; or a number exists between the cursor and `end-of-line'.
    (t
     (let ((point-next
            (save-excursion
              (when (funcall
                     do-increment
                     amount
                     (cons (line-beginning-position) (line-end-position))
                     ;; Ignore numbers directly before the cursor unless
                     ;; `evil-numbers-use-cursor-at-end-of-number' is enabled.
                     (unless evil-numbers-use-cursor-at-end-of-number
                       (let ((point-init (point)))
                         (lambda (_beg end) (< point-init end)))))
                (point)))))
       (cond
        ((null point-next)
         (message "No number at point or until end of line")
         nil)
        (t
         ;; Moves point one position back, see `evil-adjust-cursor'.
         (goto-char (1- point-next))
         t)))))))

;;;###autoload (autoload 'evil-numbers/dec-at-pt "evil-numbers" nil t)
(evil-define-operator
 evil-numbers/dec-at-pt
 (amount beg end type &optional incremental padded)
 "Decrement the number at point or after point before `end-of-line' by AMOUNT.

If a region is active, decrement all the numbers in the region by AMOUNT."
 :motion nil

 (interactive "*<c><R>")
 (evil-numbers/inc-at-pt (- (or amount 1)) beg end type incremental padded))

;;;###autoload (autoload 'evil-numbers/inc-at-pt-incremental "evil-numbers" nil t)
(evil-define-operator
 evil-numbers/inc-at-pt-incremental
 (amount beg end type padded)
 "Increment the number by AMOUNT.

When there is no active region, use the number at point
or between the point and the `end-of-line'.
When a region is active, increment all the numbers in the region by AMOUNT*n, where
n is the index of the number among the numbers in the region, starting at 1.
That is, increment the first number by AMOUNT, the second by 2*AMOUNT,
and so on."
 :motion nil

 (interactive "*<c><R>")
 (evil-numbers/inc-at-pt amount beg end type 'incremental padded))

;;;###autoload (autoload 'evil-numbers/dec-at-pt-incremental "evil-numbers" nil t)
(evil-define-operator
 evil-numbers/dec-at-pt-incremental
 (amount beg end type padded)
 "Like `evil-numbers/inc-at-pt-incremental' but with negated argument AMOUNT."
 :motion nil

 (interactive "*<c><R>")
 (evil-numbers/inc-at-pt (- (or amount 1)) beg end type 'incremental padded))

(provide 'evil-numbers)

;; Local Variables:
;; fill-column: 80
;; elisp-autofmt-load-packages-local: ("evil-macros")
;; End:

;;; evil-numbers.el ends here
