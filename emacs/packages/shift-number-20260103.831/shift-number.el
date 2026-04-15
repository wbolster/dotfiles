;;; shift-number.el --- Increase/decrease the number at point -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (c) 2016–2017 Alex Kost

;; Author: Alex Kost <alezost@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-shift-number
;; Keywords: convenience
;; Created: 12 Apr 2016
;; Package-Version: 20260103.831
;; Package-Revision: a71f5c3c77af
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Increase or decrease the number at point with `shift-number-up' and
;; `shift-number-down' commands.

;; To install the package manually, add the following to your init file:
;;
;; (add-to-list 'load-path "/path/to/shift-number-dir")
;; (autoload 'shift-number-up "shift-number" nil t)
;; (autoload 'shift-number-down "shift-number" nil t)

;; For more verbose description and a gif demonstration, see
;; <https://codeberg.org/ideasman42/emacs-shift-number>.

;;; Code:

(eval-when-compile
  ;; For `pcase-dolist'.
  (require 'pcase))

;; ---------------------------------------------------------------------------
;; Compatibility

(eval-when-compile
  (when (version< emacs-version "31.1")
    (defmacro incf (place &optional delta)
      "Increment PLACE by DELTA or 1."
      (declare (debug (gv-place &optional form)))
      (gv-letplace (getter setter) place
        (funcall setter `(+ ,getter ,(or delta 1)))))
    (defmacro decf (place &optional delta)
      "Decrement PLACE by DELTA or 1."
      (declare (debug (gv-place &optional form)))
      (gv-letplace (getter setter) place
        (funcall setter `(- ,getter ,(or delta 1)))))))

(when (version< emacs-version "29.1")
  (defsubst pos-bol (&optional n)
    "Return the position at the line beginning.
N specifies which line (default 1, the current line)."
    (line-beginning-position n))
  (defsubst pos-eol (&optional n)
    "Return the position at the line end.
N specifies which line (default 1, the current line)."
    (line-end-position n)))


;; ---------------------------------------------------------------------------
;; Custom Variables

(defgroup shift-number nil
  "Increase or decrease the number at point."
  :group 'convenience)

(defcustom shift-number-negative t
  "If non-nil, support negative numbers."
  :type 'boolean)

(defcustom shift-number-motion nil
  "Control cursor movement after modifying a number.
- nil: cursor stays at beginning of number, mark unchanged.
- t: cursor moves to end of number, mark unchanged.
- `mark': cursor moves to end of number, mark set to beginning."
  :type
  '(choice
    (const :tag "No motion" nil) (const :tag "Motion" t) (const :tag "Motion with mark" mark)))

(defcustom shift-number-pad-default nil
  "Whether to preserve number width when it shrinks.
When non-nil, decrementing 10 gives 09 instead of 9."
  :type 'boolean)

(defcustom shift-number-separator-chars nil
  "Separator characters allowed in numeric literals for visual grouping.

This value is a string containing separator characters,
typically \"_\" or \",\" which are allowed in numeric literals in some systems.

Set to nil to disable this functionality."
  :type '(choice (const nil) string))

(defcustom shift-number-case nil
  "Case to use for hexadecimal numbers."
  :type
  '(choice
    (const :tag "Current Case" nil)
    (const :tag "Upper Case" upcase)
    (const :tag "Lower Case" downcase)))

(defcustom shift-number-incremental-direction-from-region t
  "When non-nil, reverse incremental direction when point is before mark.
With point before mark, `shift-number-up-incremental' on \"0 0 0\"
produces \"3 2 1\" instead of \"1 2 3\"."
  :type 'boolean)

(declare-function apply-on-rectangle "rect")


;; ---------------------------------------------------------------------------
;; Internal Constants

(defconst shift-number--chars-superscript "⁰¹²³⁴⁵⁶⁷⁸⁹"
  "String containing superscript digit characters 0-9.")
(defconst shift-number--chars-subscript "₀₁₂₃₄₅₆₇₈₉"
  "String containing subscript digit characters 0-9.")

;; Helper for building script alists below.
(defun shift-number--build-script-alist (minus-char plus-char digit-chars)
  "Build an alist mapping ASCII characters to script equivalents.
MINUS-CHAR is the script minus sign.
PLUS-CHAR is the script plus sign.
DIGIT-CHARS is a 10-character string of script digits 0-9."
  (cons
   (cons ?- minus-char)
   (cons
    (cons ?+ plus-char)
    (mapcar
     (lambda (i)
       (cons (string-to-char (number-to-string i)) (aref digit-chars i)))
     (number-sequence 0 9)))))

(defconst shift-number--superscript-alist
  (shift-number--build-script-alist ?⁻ ?⁺ shift-number--chars-superscript)
  "Alist mapping regular characters to superscript equivalents.")

(defconst shift-number--subscript-alist
  (shift-number--build-script-alist ?₋ ?₊ shift-number--chars-subscript)
  "Alist mapping regular characters to subscript equivalents.")

(defconst shift-number--superscript-alist-decode
  (mapcar (lambda (x) (cons (cdr x) (car x))) shift-number--superscript-alist)
  "Alist mapping superscript characters to regular equivalents.")

(defconst shift-number--subscript-alist-decode
  (mapcar (lambda (x) (cons (cdr x) (car x))) shift-number--subscript-alist)
  "Alist mapping subscript characters to regular equivalents.")


;; ---------------------------------------------------------------------------
;; Internal String Separator Utilities

(defun shift-number--strip-chars (str sep-chars)
  "Remove SEP-CHARS from STR."
  (dotimes (i (length sep-chars))
    (let ((ch (char-to-string (aref sep-chars i))))
      (setq str (replace-regexp-in-string (regexp-quote ch) "" str t t))))
  str)

(defun shift-number--strip-chars-apply (str-src str-dst sep-chars)
  "Restore SEP-CHARS positions from STR-SRC into STR-DST."
  (let ((sep-chars-list (append sep-chars nil))
        ;; Convert strings to lists.
        (str-src-rev (nreverse (append str-src nil)))
        (str-dst-rev (nreverse (append str-dst nil)))
        (result (list)))
    (while str-dst-rev
      (let ((ch-src (pop str-src-rev)))
        (cond
         ((and ch-src (memq ch-src sep-chars-list))
          (push ch-src result))
         (t
          (push (pop str-dst-rev) result)))))
    (apply #'string result)))


;; ---------------------------------------------------------------------------
;; Internal Utilities

(defun shift-number--case-category (str default)
  "Categorize the case of STR.
Return DEFAULT if STR has no case (e.g., digits only), otherwise:
-   1: Upper case.
-  -1: Lower case.
- nil: Mixed case."
  (let ((str-dn (downcase str))
        (str-up (upcase str)))
    (cond
     ((string-equal str str-dn)
      (cond
       ((string-equal str str-up)
        default)
       (t
        -1)))
     (t
      (cond
       ((string-equal str str-up)
        1)
       (t
        nil))))))

(defun shift-number--format-binary (number &optional width fillchar)
  "Format NUMBER as binary.
Fill up to WIDTH with FILLCHAR (defaults to ?0) if binary
representation of NUMBER is smaller than WIDTH."
  (let ((nums (list))
        (fillchar (or fillchar ?0)))
    (while (> number 0)
      (push (number-to-string (% number 2)) nums)
      (setq number (truncate number 2)))
    (let ((len (length nums)))
      (apply #'concat
             (cond
              ((and width (< len width))
               (make-string (- width len) fillchar))
              (t
               ""))
             nums))))

(defun shift-number--format (num width base)
  "Format NUM with at least WIDTH digits in BASE."
  (cond
   ((= base 2)
    (shift-number--format-binary num width))
   ((= base 8)
    (format (format "%%0%do" width) num))
   ((= base 16)
    (format (format "%%0%dX" width) num))
   ((= base 10)
    (format (format "%%0%dd" width) num))
   (t
    "")))

(defun shift-number--skip-chars-impl (ch-skip ch-sep-optional dir ch-num limit)
  "Wrapper for `skip-chars-forward' and `skip-chars-backward'.

CH-SKIP: Characters to skip.
CH-SEP-OPTIONAL: Separator characters (single instances are stepped over).
DIR: Direction to step in (1 or -1).
CH-NUM: Number of characters to step.
LIMIT: Point which will not be stepped past."
  (let* ((is-forward (< 0 dir))
         (skip-chars-fn
          (cond
           (is-forward
            #'skip-chars-forward)
           (t
            #'skip-chars-backward)))
         (clamp-fn
          (cond
           (is-forward
            #'min)
           (t
            #'max)))
         (skipped
          (abs
           (funcall skip-chars-fn
                    ch-skip
                    ;; Limit.
                    (funcall clamp-fn (+ (point) (* ch-num dir)) limit)))))

    ;; Step over single separators, as long as there is a number after them.
    ;; Allow '100,123' and '16_777_216' to be handled as single numbers.
    (when ch-sep-optional
      (let ((point-next nil)
            (skipped-next 0))
        (setq ch-num (- ch-num skipped))
        (while (and (not (zerop ch-num))
                    (save-excursion
                      (and (eq 1 (shift-number--skip-chars-impl ch-sep-optional nil dir 1 limit))
                           (progn
                             ;; Not counted towards 'skipped'
                             ;; as this character is to be ignored entirely.
                             (setq skipped-next
                                   (shift-number--skip-chars-impl ch-skip nil dir ch-num limit))
                             (unless (zerop skipped-next)
                               (setq point-next (point))
                               ;; Found (apply `point-next').
                               t)))))
          ;; Step over the separator and contents found afterwards.
          (when point-next
            (goto-char point-next)
            (setq skipped (+ skipped skipped-next))
            (setq ch-num (- ch-num skipped-next))
            t))))

    skipped))

(defun shift-number--match-from-skip-chars (match-chars dir limit do-check do-match)
  "Match MATCH-CHARS in DIR (-1 or 1), until LIMIT.

When DO-CHECK is non-nil, any failure to match returns nil.
When DO-MATCH is non-nil, match data is set.

Each item in MATCH-CHARS is a list of (CH-SKIP CH-NUM CH-SEP-OPTIONAL).
- CH-SKIP is the argument to pass to
  `skip-chars-forward' or `skip-chars-backward'.
- CH-NUM specifies how many characters to match.
  Valid values:
  - Symbol `+' one or more.
  - Symbol `*' zero or more.
  - An integer: match exactly this many.
- CH-SEP-OPTIONAL specifies optional separator characters."
  (catch 'result
    (let* ((is-forward (< 0 dir))
           (point-init (point))
           ;; Fill when `do-match' is set.
           (match-list (list)))

      ;; Sanity check.
      (when (cond
             (is-forward
              (> (point) limit))
             (t
              (< (point) limit)))
        (error "Limit is on wrong side of point (internal error)"))

      (unless is-forward
        (setq match-chars (reverse match-chars)))

      (pcase-dolist (`(,ch-skip ,ch-num ,ch-sep-optional) match-chars)
        ;; Beginning of the match.
        (when do-match
          (push (point) match-list))

        (cond
         ((integerp ch-num)
          (let ((skipped (shift-number--skip-chars-impl ch-skip ch-sep-optional dir ch-num limit)))
            (when do-check
              (unless (eq skipped ch-num)
                (throw 'result nil)))))
         ((eq ch-num '+)
          (let ((skipped
                 (shift-number--skip-chars-impl
                  ch-skip ch-sep-optional dir most-positive-fixnum limit)))
            (when do-check
              (unless (>= skipped 1)
                (throw 'result nil)))))

         ;; No length checking needed as zero is acceptable.
         ;; Skip these characters if they exist.
         ((eq ch-num '*)
          (shift-number--skip-chars-impl ch-skip ch-sep-optional dir most-positive-fixnum limit))
         ((eq ch-num '\?)
          (shift-number--skip-chars-impl ch-skip ch-sep-optional dir 1 limit))
         (t
          (error "Unknown type %S (internal error)" ch-num)))

        ;; End of the match.
        (when do-match
          (push (point) match-list)))

      ;; Add match group 0 (full match) at the beginning of the list.
      (when do-match
        (setq match-list
              (cond ; `point-init' `point' `match-list'.
               (is-forward
                (cons point-init (cons (point) (nreverse match-list))))
               (t ; `point' `point-init' `match-list'.
                (cons (point) (cons point-init match-list)))))

        (set-match-data match-list)))
    t))


;; ---------------------------------------------------------------------------
;; Internal Script Translation

(defun shift-number--translate-with-alist (alist string)
  "Translate every character in STRING using ALIST."
  (funcall (cond
            ((stringp string)
             #'concat)
            (t
             #'identity))
           (mapcar (lambda (c) (cdr (assoc c alist))) string)))

(defun shift-number--encode-super (x)
  "Convert string X to superscript."
  (shift-number--translate-with-alist shift-number--superscript-alist x))
(defun shift-number--decode-super (x)
  "Convert string X from superscript to regular characters."
  (shift-number--translate-with-alist shift-number--superscript-alist-decode x))

(defun shift-number--encode-sub (x)
  "Convert string X to subscript."
  (shift-number--translate-with-alist shift-number--subscript-alist x))
(defun shift-number--decode-sub (x)
  "Convert string X from subscript to regular characters."
  (shift-number--translate-with-alist shift-number--subscript-alist-decode x))


;; ---------------------------------------------------------------------------
;; Internal Implementation

(defun shift-number--inc-at-pt-impl-with-match-chars
    (match-chars
     ;; Numeric & other options.
     sign-group num-group base beg end padded do-case
     ;; Callbacks.
     range-check-fn number-xform-fn decode-fn encode-fn)
  "Perform the increment/decrement within BEG and END.

For MATCH-CHARS docs see `shift-number--match-from-skip-chars'.
NUM-GROUP is the match group used to evaluate the number.
SIGN-GROUP is the match group used for the sign ('-' or '+').

When PADDED is non-nil,
the number keeps its current width (with leading zeroes).
When PADDED is the symbol `auto', detect leading zeros and preserve width.

When RANGE-CHECK-FN is non-nil, it's called with the match beginning & end.
A nil result causes this function to skip this match.

When DO-CASE is non-nil, apply case handling for hexadecimal numbers.

DECODE-FN converts the matched string to regular characters for parsing.
ENCODE-FN converts the result back for replacement.

When all characters are found in sequence, evaluate the number in BASE,
replacing it by the result of NUMBER-XFORM-FN.
Return (OLD-BEG . OLD-END) on success, nil on failure."
  (save-match-data
    (when (and (save-excursion
                 ;; Skip backwards (as needed), there may be no
                 ;; characters to skip back, so don't check the result.
                 (shift-number--match-from-skip-chars match-chars -1 beg nil nil)
                 ;; Skip forwards from the beginning, setting match data.
                 (shift-number--match-from-skip-chars match-chars 1 end t t))

               ;; Either there is no range checking or the range must
               ;; be accepted by the caller.
               (or (null range-check-fn)
                   (funcall range-check-fn (match-beginning 0) (match-end 0))))

      ;; Capture old bounds before any modification.
      (let* ((old-beg (match-beginning 0))
             (old-end (match-end 0))
             (sep-char (nth 2 (nth (1- num-group) match-chars)))
             (num-str-raw (match-string num-group))
             (sign-str (match-string sign-group))
             ;; Check if sign is preceded by a decimal digit (e.g., "123-456").
             ;; If so, ignore the sign to avoid treating it as a negative number.
             ;; Only check for 0-9 to allow "1e-10" scientific notation to work.
             (sign-preceded-by-digit
              (and sign-str
                   (not (string-empty-p sign-str)) (> (match-beginning sign-group) beg)
                   (let ((ch-before (char-before (match-beginning sign-group))))
                     (and ch-before (>= ch-before ?0) (<= ch-before ?9)))))
             (use-sign (and shift-number-negative (not sign-preceded-by-digit)))
             (str-prev
              (funcall decode-fn
                       (concat
                        (cond
                         (use-sign
                          sign-str)
                         (t
                          ""))
                        num-str-raw)))

             (str-prev-strip
              (cond
               (sep-char
                (shift-number--strip-chars str-prev sep-char))
               (t
                str-prev)))

             ;; Auto-detect leading zeros: if number starts with 0 and has more digits.
             (has-leading-zeros (and (> (length num-str-raw) 1) (eq (aref num-str-raw 0) ?0)))
             (use-padding
              (cond
               ((eq padded 'auto)
                has-leading-zeros)
               (t
                padded)))

             (num-prev (string-to-number str-prev-strip base))
             (num-next
              (let ((result (funcall number-xform-fn num-prev)))
                ;; When negative numbers are disabled, clamp to 0.
                (cond
                 (shift-number-negative
                  result)
                 (t
                  (max 0 result)))))
             (str-next
              (shift-number--format
               (abs num-next)
               (cond
                (use-padding
                 (- (match-end num-group) (match-beginning num-group)))
                (t
                 1))
               base)))

        ;; Maintain case.
        (when do-case
          ;; Upper case (already set), no need to handle here.
          (cond
           ;; Keep current case.
           ((null shift-number-case)
            (when (eq -1 (or (shift-number--case-category str-prev -1) -1))
              (setq str-next (downcase str-next))))
           ((eq shift-number-case 'downcase)
            (setq str-next (downcase str-next)))))

        (when sep-char
          ;; This is a relatively expensive operation,
          ;; only apply separators back if any were found to begin with.
          (unless (string-equal str-prev str-prev-strip)
            (setq str-next (shift-number--strip-chars-apply str-prev str-next sep-char))))

        ;; Replace number then sign to work around Emacs bug #74666.
        ;; Order doesn't affect correctness, but this order avoids the bug.

        ;; Replace the number.
        (replace-match (funcall encode-fn str-next) t t nil num-group)

        ;; Replace the sign (as needed).
        (when use-sign
          (cond
           ;; From negative to positive.
           ((and (< num-prev 0) (not (< num-next 0)))
            (replace-match "" t t nil sign-group))
           ;; From positive to negative.
           ((and (not (< num-prev 0)) (< num-next 0))
            (replace-match (funcall encode-fn "-") t t nil sign-group))))

        (goto-char (match-end num-group))

        ;; Return old bounds for cursor positioning.
        (cons old-beg old-end)))))

(defun shift-number--inc-at-pt-impl (beg end padded range-check-fn number-xform-fn)
  "Increment/decrement the number at point, limited by BEG and END.

Keep padding when PADDED is non-nil.  Use `auto' for automatic detection.

See `shift-number--inc-at-pt-impl-with-match-chars' for details on
RANGE-CHECK-FN and NUMBER-XFORM-FN.

Return (OLD-BEG . OLD-END) on success, nil on failure.
Point is left at the end of the modified number."
  (or
   ;; Find binary literals:
   ;; 0[bB][01]+, e.g. 0b101 or 0B0.
   (shift-number--inc-at-pt-impl-with-match-chars
    `(("+-" \?) ("0" 1) ("bB" 1) ("01" + ,shift-number-separator-chars))
    ;; Sign, number groups & base.
    1 4 2
    ;; Other arguments.
    beg end padded nil range-check-fn number-xform-fn
    ;; Decode & encode callbacks.
    #'identity #'identity)

   ;; Find octal literals:
   ;; 0[oO][0-7]+, e.g. 0o42 or 0O5.
   (shift-number--inc-at-pt-impl-with-match-chars
    `(("+-" \?) ("0" 1) ("oO" 1) ("0-7" + ,shift-number-separator-chars))
    ;; Sign, number groups & base.
    1 4 8
    ;; Other arguments.
    beg end padded nil range-check-fn number-xform-fn
    ;; Decode & encode callbacks.
    #'identity #'identity)

   ;; Find hex literals:
   ;; 0[xX][0-9a-fA-F]+, e.g. 0xBEEF or 0Xcafe.
   (shift-number--inc-at-pt-impl-with-match-chars
    `(("+-" \?) ("0" 1) ("xX" 1) ("[:xdigit:]" + ,shift-number-separator-chars))
    ;; Sign, number groups & base.
    1 4 16
    ;; Other arguments.
    beg end padded t range-check-fn number-xform-fn
    ;; Decode & encode callbacks.
    #'identity #'identity)

   ;; Find decimal literals:
   ;; [0-9]+, e.g. 42 or 23.
   (shift-number--inc-at-pt-impl-with-match-chars
    `(("+-" \?) ("0123456789" + ,shift-number-separator-chars))
    ;; Sign, number groups & base.
    1 2 10
    ;; Other arguments.
    beg end padded nil range-check-fn number-xform-fn
    ;; Decode & encode callbacks.
    #'identity #'identity)

   ;; Find decimal literals (superscript).
   (shift-number--inc-at-pt-impl-with-match-chars
    `(("⁺⁻" \?) (,shift-number--chars-superscript + nil))
    ;; Sign, number groups & base.
    1 2 10
    ;; Other arguments.
    beg end padded nil range-check-fn number-xform-fn
    ;; Decode & encode callbacks.
    #'shift-number--decode-super #'shift-number--encode-super)

   ;; Find decimal literals (subscript).
   (shift-number--inc-at-pt-impl-with-match-chars
    `(("₊₋" \?) (,shift-number--chars-subscript + nil))
    ;; Sign, number groups & base.
    1 2 10
    ;; Other arguments.
    beg end padded nil range-check-fn number-xform-fn
    ;; Decode & encode callbacks.
    #'shift-number--decode-sub #'shift-number--encode-sub)))

(defconst shift-number--number-chars-regexp
  (concat "[" "[:xdigit:]" shift-number--chars-superscript shift-number--chars-subscript "]")
  "Regexp matching characters that may be part of a number.")

(defun shift-number--inc-at-pt-with-search (amount beg end padded range-check-fn dir)
  "Change the number at point by AMOUNT, limited by BEG and END.

Keep padding when PADDED is non-nil.  Use `auto' for automatic detection.

DIR specifies search direction: 1 for forward, -1 for backward.

See `shift-number--inc-at-pt-impl-with-match-chars' for details on
RANGE-CHECK-FN.

Return (OLD-BEG . OLD-END) on success, nil on failure.
Point is left at the end of the modified number."
  (let ((result nil))
    (save-match-data
      ;; Search for any text that might be part of a number,
      ;; if `shift-number--inc-at-pt-impl' cannot parse it - that's fine,
      ;; keep searching until the limit.
      ;; This avoids doubling up on number parsing logic.
      ;;
      ;; Note that the while body is empty.
      (while (and
              ;; Found item, exit the loop.
              (null
               (when (setq result
                           (shift-number--inc-at-pt-impl
                            ;; Clamp limits to line bounds.
                            ;; The caller may use a range that spans lines to
                            ;; allow searching and finding items across
                            ;; multiple lines (currently used for selection).
                            (max beg (pos-bol))
                            (min end (pos-eol))
                            padded
                            range-check-fn
                            (lambda (n) (+ n amount))))
                 t))

              ;; No more matches found, exit the loop.
              (cond
               ((< dir 0)
                (re-search-backward shift-number--number-chars-regexp beg t))
               (t
                (re-search-forward shift-number--number-chars-regexp end t))))))
    result))


;; ---------------------------------------------------------------------------
;; Private Functions

(defmacro shift-number--swap-vars (i j)
  "Swap the value of I & J."
  `(setq ,i
         (prog1 ,j
           (setq ,j ,i))))

(defun shift-number--impl (n pos limit-beg limit-end dir)
  "Change the number at point by N.
If there is no number at point, search in the direction DIR
and change the first number found.

DIR specifies direction: 1 for forward, -1 for backward.
Search is limited by LIMIT-BEG and LIMIT-END.

Return (OLD-BOUNDS . NEW-BOUNDS) on success, nil on failure."
  (save-excursion
    (goto-char pos)
    (let ((old-bounds
           (shift-number--inc-at-pt-with-search
            n limit-beg limit-end (or shift-number-pad-default 'auto) nil dir)))
      (when old-bounds
        (let* ((num-chars
                (concat
                 "[:xdigit:]oOxXbB" shift-number--chars-superscript shift-number--chars-subscript))
               (old-beg (car old-bounds))
               (old-end (cdr old-bounds))
               (new-beg
                (save-excursion
                  (skip-chars-backward num-chars limit-beg)
                  (when (and shift-number-negative (memq (char-before) '(?- ?+ ?⁻ ?⁺ ?₋ ?₊)))
                    (backward-char))
                  (point)))
               (new-end
                (cond
                 ((< dir 0)
                  (save-excursion
                    (skip-chars-forward num-chars limit-end)
                    (point)))
                 (t
                  ;; Forward search leaves point at end of number.
                  (point)))))
          (cons old-bounds (cons new-beg new-end)))))))

(defun shift-number--on-line (n)
  "Adjust the number N on the current line."
  (let* ((old-pos (point))
         (bounds-pair (shift-number--impl n old-pos (pos-bol) (pos-eol) 1)))

    (unless bounds-pair
      (error "No number on the current line"))

    (let* ((old-bounds (car bounds-pair))
           (new-bounds (cdr bounds-pair))
           (old-end (cdr old-bounds))
           (new-beg (car new-bounds))
           (new-end (cdr new-bounds)))

      (cond
       ;; If the point was exactly at the end, keep it there.
       ((eq old-pos old-end)
        (setq old-pos new-end))
       ;; Prevent the change causing the cursor to "leave" the number,
       ;; allowing for further adjustments.
       (t
        (setq old-pos (min new-end old-pos))))

      (goto-char old-pos)

      (when shift-number-motion
        (when (eq shift-number-motion 'mark)
          (set-mark new-beg))
        (goto-char new-end))

      new-end)))

(defun shift-number--on-region-impl (n region-beg region-end incremental dir start-count)
  "Shift the numbers N in the region defined.
REGION-BEG & REGION-END define the region.
When INCREMENTAL is non-nil, each successive number is changed by an
additional N (first by 1*N, second by 2*N, etc.).
DIR specifies the direction: 1 for forward, -1 for backward.
START-COUNT specifies the initial count for incremental mode.
Returns the final count value."
  (let* ((pos-beg-old (mark))
         (pos-beg-new nil)
         (pos-end-old (point))
         (pos-end-new nil)
         (point-is-first nil)
         (count start-count))

    ;; Ensure order, mark then point.
    (when (< pos-end-old pos-beg-old)
      (setq point-is-first t)
      (shift-number--swap-vars pos-end-old pos-beg-old))

    (save-excursion
      (let ((bounds-pair nil)
            (search-pos
             (cond
              ((< dir 0)
               region-end)
              (t
               region-beg))))
        (goto-char search-pos)
        (while (and (setq bounds-pair
                          (shift-number--impl (* n count) search-pos region-beg region-end dir)))
          (let* ((old-bounds (car bounds-pair))
                 (new-bounds (cdr bounds-pair))
                 (old-beg (car old-bounds))
                 (old-end (cdr old-bounds))
                 (new-beg (car new-bounds))
                 (new-end (cdr new-bounds))
                 (delta (- new-end old-end)))

            ;; Always update pos-end-new to track the last modified number.
            (setq pos-end-new
                  (cond
                   (shift-number-motion
                    new-end)
                   (t
                    new-beg)))

            (cond
             ((< dir 0)
              ;; For backward mode, we process from end to beginning.
              ;; Positions before the modified number are unaffected.
              ;; Continue searching from before the modified number.
              (setq search-pos new-beg)
              (setq region-end new-beg))
             (t
              ;; For forward mode, adjust cursor if after the modified number.
              (cond
               ((< pos-end-old old-beg)) ; NOP - cursor is before the number.
               ((> pos-end-old old-end)
                (incf pos-end-old delta)))

              ;; Track mark position when motion with mark is enabled.
              (when (eq shift-number-motion 'mark)
                (cond
                 ((< pos-beg-old old-beg)) ; NOP - mark is before the number.
                 ((> pos-beg-old old-end)
                  (incf pos-beg-old delta))
                 (t
                  (setq pos-beg-new new-beg))))

              ;; Continue from after the modified number.
              (setq search-pos new-end)
              (setq region-beg new-end)
              (incf region-end delta)))

            ;; Increment count for incremental mode.
            (when incremental
              (incf count))))))

    ;; Always position cursor at the last modified number.
    ;; Only swap mark position (for motion mode) based on selection direction.
    (when pos-end-new
      (goto-char pos-end-new))
    (when pos-beg-new
      (when point-is-first
        (shift-number--swap-vars pos-end-new pos-beg-new))
      (set-mark pos-beg-new))

    ;; Return final count value (for chaining in rectangle mode).
    count))

(defun shift-number--on-region (n incremental)
  "Shift the numbers N on the current region.
When INCREMENTAL is non-nil, use progressive amounts."
  (shift-number--on-region-impl n (region-beginning) (region-end) incremental 1 1))

(defun shift-number--on-rectangle (n incremental dir)
  "Shift the numbers N in the current rectangle selection.
When INCREMENTAL is non-nil, use progressive amounts across all cells.
DIR specifies the direction: 1 for forward, -1 for backward."
  (let ((final-point nil)
        (final-mark nil)
        (count 1))
    ;; For backward mode, we need to process lines in reverse order.
    ;; Collect line info first, then process.
    (cond
     ((< dir 0)
      (let ((line-info nil))
        ;; Collect line positions and column info.
        (apply-on-rectangle
         (lambda (col-beg col-end)
           (let ((beg nil)
                 (end nil))
             (save-excursion
               (move-to-column col-beg)
               (setq beg (point))
               (move-to-column col-end)
               (setq end (point)))
             (push (list beg end) line-info)))
         (region-beginning) (region-end))
        ;; Process in reverse order (line-info is already reversed from push).
        (dolist (info line-info)
          (let ((beg (car info))
                (end (cadr info)))
            (goto-char end)
            (setq count (shift-number--on-region-impl n beg end incremental dir count))
            (setq final-point (point))
            (when (mark)
              (setq final-mark (mark)))))))
     (t
      (apply-on-rectangle
       (lambda (col-beg col-end)
         (let ((beg nil)
               (end nil))
           (save-excursion
             (move-to-column col-beg)
             (setq beg (point))
             (move-to-column col-end)
             (setq end (point)))
           ;; Move point to region start so cursor tracking works correctly.
           (goto-char beg)
           ;; Don't use save-excursion here - we want to capture final position.
           ;; Chain count across lines for incremental mode.
           (setq count (shift-number--on-region-impl n beg end incremental dir count))
           ;; Capture cursor/mark from last line processed.
           (setq final-point (point))
           (when (mark)
             (setq final-mark (mark)))))
       (region-beginning) (region-end))))
    ;; Apply final cursor position.
    (when final-point
      (goto-char final-point))
    (when final-mark
      (set-mark final-mark))))

(defun shift-number--on-context (n)
  "Manipulate numbers in the current region or line by N."
  (cond
   ((bound-and-true-p rectangle-mark-mode)
    (shift-number--on-rectangle n nil 1))
   ((region-active-p)
    (shift-number--on-region n nil))
   (t
    (shift-number--on-line n))))

(defun shift-number--on-context-incremental (n)
  "Incrementally change numbers between point and mark by N.
The first number is changed by N, the second by 2*N, etc.
Works with rectangle selection when `rectangle-mark-mode' is active."
  (unless (mark)
    (user-error "The mark is not set"))
  (let ((dir
         (cond
          ((and shift-number-incremental-direction-from-region (< (point) (mark)))
           -1)
          (t
           1))))
    (cond
     ((bound-and-true-p rectangle-mark-mode)
      (shift-number--on-rectangle n t dir))
     (t
      (shift-number--on-region-impl n (region-beginning) (region-end) t dir 1)))))


;; ---------------------------------------------------------------------------
;; Public Functions

;;;###autoload
(defun shift-number-up (&optional arg)
  "Increase the number at point (or on the current line) by ARG."
  (interactive "*p")
  (shift-number--on-context arg))

;;;###autoload
(defun shift-number-down (&optional arg)
  "Decrease the number at point (or on the current line) by ARG."
  (interactive "*p")
  (shift-number--on-context (- arg)))

;;;###autoload
(defun shift-number-up-incremental (&optional arg)
  "Incrementally increase numbers between point and mark by ARG.
The first number is increased by ARG, the second by 2*ARG, etc.
Works with rectangle selection when `rectangle-mark-mode' is active."
  (interactive "*p")
  (shift-number--on-context-incremental arg))

;;;###autoload
(defun shift-number-down-incremental (&optional arg)
  "Incrementally decrease numbers between point and mark by ARG.
The first number is decreased by ARG, the second by 2*ARG, etc.
Works with rectangle selection when `rectangle-mark-mode' is active."
  (interactive "*p")
  (shift-number--on-context-incremental (- arg)))

;;;###autoload
(defun shift-number-increment-at-point-with-search (&rest args)
  "Change the number at point, with keyword arguments in ARGS.

Required keywords:
  :amount       - The amount to increment.
  :range        - A cons cell (BEG . END) specifying search bounds.

Optional keywords:
  :dir          - Search direction: 1 for forward, -1 for backward (default 1).
  :range-check-fn - See `shift-number--inc-at-pt-impl-with-match-chars'.

Optional keywords to override customization variables:
  :pad-default    - Override `shift-number-pad-default'.
  :negative       - Override `shift-number-negative'.
  :separator-chars - Override `shift-number-separator-chars'.
  :case           - Override `shift-number-case'.
  :motion         - Override `shift-number-motion'.

Return (OLD-BEG . OLD-END) on success, nil on failure.
Point is left at the end of the modified number.

This function is only exposed for use by `evil-numbers'."
  (let ((amount nil)
        (range nil)
        (dir 1)
        (range-check-fn nil)
        (shift-number-pad-default shift-number-pad-default)
        (shift-number-negative shift-number-negative)
        (shift-number-separator-chars shift-number-separator-chars)
        (shift-number-case shift-number-case)
        (shift-number-motion shift-number-motion))
    (while args
      (let ((key (pop args))
            (value (pop args)))
        (pcase key
          (:amount (setq amount value))
          (:range (setq range value))
          (:dir (setq dir value))
          (:range-check-fn (setq range-check-fn value))
          (:pad-default (setq shift-number-pad-default value))
          (:negative (setq shift-number-negative value))
          (:separator-chars (setq shift-number-separator-chars value))
          (:case (setq shift-number-case value))
          (:motion (setq shift-number-motion value))
          (_ (error "Unknown keyword: %S" key)))))
    (unless amount
      (error "Missing required keyword: :amount"))
    (unless range
      (error "Missing required keyword: :range"))
    (shift-number--inc-at-pt-with-search
     amount (car range) (cdr range) (or shift-number-pad-default 'auto) range-check-fn dir)))

(provide 'shift-number)
;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; shift-number.el ends here
