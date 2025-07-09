;;; flyover.el --- Display Flycheck and Flymake errors with overlays -*- lexical-binding: t -*-

;; Author: Mikael Konradsson <mikael.konradsson@outlook.com>
;; Package-Version: 20250708.1611
;; Package-Revision: dbbc13f67c42
;; Package-Requires: ((emacs "27.1") (flymake "1.0"))
;; Keywords: convenience, tools, flycheck, flymake
;; URL: https://github.com/konrad1977/flyover
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; This package provides a way to display Flycheck and Flymake errors using overlays.
;; It offers customization options for icons, colors, and display behavior.
;; 
;; Flycheck is optional - if not available, only Flymake support will be enabled.

;;; Code:

(define-advice flyovers-in (:override (beg end) disable-sorting)
  "Get flycheck overlays between BEG and END without sorting."
  (seq-filter (lambda (ov) (overlay-get ov 'flyover))
              (overlays-in beg end)))

(require 'flymake)
(require 'cl-lib)

;; Optional flycheck support
(defun flyover--ensure-flycheck ()
  "Ensure flycheck is available if needed."
  (when (and (memq 'flycheck flyover-checkers)
             (not (featurep 'flycheck)))
    (condition-case nil
        (require 'flycheck)
      (error
       (message "Flycheck not available, removing from flyover-checkers")
       (setq flyover-checkers (delq 'flycheck flyover-checkers))))))

(defgroup flyover nil
  "Display Flycheck/Flymake errors using overlays."
  :prefix "flyover-"
  :group 'tools)

(defcustom flyover-debug nil
  "Enable debug messages for flyover."
  :type 'boolean
  :group 'flyover)

(defcustom flyover-checkers '(flycheck flymake)
  "Checkers to use for displaying errors.
Supported values are `flycheck` and `flymake`."
  :type '(set (const :tag "Flycheck" flycheck)
              (const :tag "Flymake" flymake))
  :group 'flyover)

(defcustom flyover-levels '(error warning info)
  "Error levels to display overlays for.
Only errors with these levels will be shown as overlays.
You can customize which levels are displayed by modifying this list.

Example configurations:
- Show only errors: \='(error)
- Show errors and warnings: \='(error warning)
- Show all levels: \='(error warning info)"

  :type '(set (const :tag "Errors" error)
              (const :tag "Warnings" warning)
              (const :tag "Info" info))
  :group 'flyover)

(defcustom flyover-base-height 0.8
  "Base height for all overlay faces.
This affects the font size of error, warning and info messages."
  :type 'number
  :group 'flyover)

(defface flyover-error
  `((t :inherit error
       :height ,flyover-base-height
       :weight normal))
  "Face used for error overlays.
Inherits from the theme's error face."
  :group 'flyover)

(defface flyover-warning
  `((t :inherit warning
       :height ,flyover-base-height
       :weight normal))
  "Face used for warning overlays.
Inherits from the theme's warning face."
  :group 'flyover)

(defface flyover-info
  `((t :inherit success
       :height ,flyover-base-height
       :weight normal))
  "Face used for info overlays.
Inherits from the theme's success face."
  :group 'flyover)

(defface flyover-marker
  `((t :inherit link
       :height ,flyover-base-height
       :weight bold))
  "Face used for info overlays."
  :group 'flyover)

(defcustom flyover-text-tint 'lighter
  "Tint type for text.  Possible values: nil, \='lighter, \='darker."
  :type '(choice (const :tag "No tinting" nil)
                 (const :tag "Lighter than base color" lighter)
                 (const :tag "Darker than base color" darker))
  :group 'flyover)

(defcustom flyover-text-tint-percent 50
  "Percentage to lighten or darken the text when tinting is enabled."
  :type 'integer
  :group 'flyover)

(defcustom flyover-info-icon " "
  "Icon used for information."
  :type 'string
  :group 'flyover)

(defcustom flyover-warning-icon " "
  "Icon used for warnings."
  :type 'string
  :group 'flyover)

(defcustom flyover-error-icon " "
  "Icon used for warnings."
  :type 'string
  :group 'flyover)

(defcustom flyover-virtual-line-icon nil
  "Icon used for the virtual line."
  :type 'string
  :group 'flyover)

(defcustom flyover-percent-darker 50
  "Icon background percent darker.
Based on foreground color"
  :type 'integer
  :group 'flyover)

(defcustom flyover-use-theme-colors t
  "Use theme colors for error, warning, and info faces.
When non-nil, adapts colors from the current theme."
  :type 'boolean
  :group 'flyover)

(defcustom flyover-background-lightness 75
  "Background lightness percentage for overlay faces.
Lower values make backgrounds darker."
  :type 'integer
  :group 'flyover)

(defcustom flyover-show-at-eol nil
  "Show error messages at the end of the line."
  :type 'boolean
  :group 'flyover)

(defcustom flyover-hide-when-cursor-is-on-same-line t
  "Hide error messages when the cursor is on the same line."
  :type 'boolean
  :group 'flyover)

(defcustom flyover-hide-when-cursor-is-at-same-line nil
  "Hide error messages when cursor is at same line as error.
Unlike `flyover-hide-when-cursor-is-on-same-line', this
only hides when cursor is at the exact line position of the error."
  :type 'boolean
  :group 'flyover)

(defcustom flyover-hide-checker-name t
  "Hide the checker name in the error message."
  :type 'boolean
  :group 'flyover)

(defcustom flyover-show-virtual-line t
  "Show a virtual line to highlight the error a bit more."
  :type 'boolean
  :group 'flyover)

(defcustom flyover-icon-left-padding 0.9
  "Padding to the left of the icon."
  :type 'number
  :group 'flyover)

(defcustom flyover-icon-right-padding 0.5
  "Padding to the right of the icon."
  :type 'number
  :group 'flyover)

(defcustom flyover-debounce-interval 0.2
  "Time in seconds to wait before checking and displaying errors after a change."
  :type 'number
  :group 'flyover)

(defcustom flyover-virtual-line-type 'curved-dotted-arrow
  "Arrow used to point to the error.
Provides various line styles including
straight, curved, bold, and dotted variations,
with and without arrow terminators."
  :type '(choice
          ;; Basic styles (no arrow)
          (const :tag "No indicator" nil)
          (const :tag "Straight line" line-no-arrow)
          (const :tag "Curved line" curved-line-no-arrow)
          (const :tag "Double line" double-line-no-arrow)
          (const :tag "Bold line" bold-line-no-arrow)
          (const :tag "Dotted line" dotted-line-no-arrow)
          
          ;; Straight variants with arrow
          (const :tag "Straight line + arrow" straight-arrow)
          (const :tag "Double line + arrow" double-line-arrow)
          (const :tag "Bold line + arrow" bold-arrow)
          (const :tag "Dotted line + arrow" dotted-arrow)
          
          ;; Curved variants with arrow
          (const :tag "Curved line + arrow" curved-arrow)
          (const :tag "Curved bold + arrow" curved-bold-arrow)
          (const :tag "Curved double + arrow" curved-double-arrow)
          (const :tag "Curved dotted + arrow" curved-dotted-arrow)

          (const :tag "arrow" arrow)
          (const :tag "line" line))
  :group 'flyover)

(defcustom flyover-line-position-offset 1
  "Number of lines below the error line to display the overlay.
A value of 1 means the overlay appears on the next line after the error.
A value of 0 means the overlay appears on the same line as the error."
  :type 'integer
  :group 'flyover)

(defcustom flyover-wrap-messages t
  "Whether to wrap long error messages across multiple lines.
When non-nil, long messages will be displayed on multiple lines.
When nil, messages will be truncated to fit on a single line."
  :type 'boolean
  :group 'flyover)

(defcustom flyover-max-line-length 80
  "Maximum length of each line when wrapping messages.
Only used when `flyover-wrap-messages' is non-nil."
  :type 'integer
  :group 'flyover)

(defvar-local flyover--overlays nil
  "List of overlays used in the current buffer.")

(defvar flyover--debounce-timer nil
  "Timer used for debouncing error checks.")

;; Color cache for performance optimization
(defvar flyover--color-cache (make-hash-table :test 'equal)
  "Cache for computed colors to avoid repeated calculations.")

;; Priority constants
(defconst flyover--error-priority 3000
  "Priority value for error overlays.")

(defconst flyover--warning-priority 2000
  "Priority value for warning overlays.")

(defconst flyover--info-priority 1000
  "Priority value for info overlays.")

;; Error handling utilities
(defun flyover--handle-error (error-symbol error-data context &optional operation)
  "Centralized error handling for flyover.
ERROR-SYMBOL is the error type, ERROR-DATA contains error details,
CONTEXT provides operation context, and OPERATION is optional operation name."
  (let ((error-msg (format "flyover error in %s: %s - %s"
                          (or operation context)
                          error-symbol
                          (if (stringp error-data) error-data (format "%S" error-data)))))
    (when flyover-debug
      (message "Debug: %s" error-msg))
    ;; Return nil to indicate failure
    nil))

(defvar flyover-regex-mark-quotes  "\\('[^']+'\\|\"[^\"]+\"\\|\{[^\}]+\}\\)"
  "Regex to match quoted strings or everything after a colon.")

(defvar flyover-regex-mark-parens "\\(\([^\)]+\)\\)"
  "Regex used to mark parentheses.")

(defvar flyover-checker-regex "^[^\"'(]*?:\\(.*\\)"
  "Regex used to match the checker name at the start of the error message.")

(defun flyover-get-arrow-type ()
  "Return the arrow character based on the selected style."
  (pcase flyover-virtual-line-type
    ;; Basic styles (no arrow)
    ('line-no-arrow "└──")
    ('curved-line-no-arrow "╰──")
    ('double-line-no-arrow "╚══")
    ('bold-line-no-arrow "┗━━")
    ('dotted-line-no-arrow "└┈┈")
    
    ;; Straight variants with arrow
    ('straight-arrow "└──►")
    ('double-line-arrow "╚══►")
    ('bold-arrow "┗━━►")
    ('dotted-arrow "└┈┈►")
    
    ;; Curved variants with arrow
    ('curved-arrow "╰──►")
    ('curved-bold-arrow "╰━━►")
    ('curved-double-arrow "╰══►")
    ('curved-dotted-arrow "╰┈┈►")

    ('arrow "──►")
    ('line "──")
    
    ;; Default/fallback
    ('no-arrow "")
    (_ "→")))

(defun flyover-get-arrow ()
  "Return the arrow character based on the selected style."
  (if (not flyover-show-virtual-line)
      ""
    (if (not flyover-virtual-line-icon)
        (flyover-get-arrow-type)
      flyover-virtual-line-icon)))

(defun flyover--get-flymake-diagnostics ()
  "Get all current Flymake diagnostics for this buffer."
  (when (and (memq 'flymake flyover-checkers)
             (bound-and-true-p flymake-mode))
    (flymake-diagnostics)))

(defun flyover--convert-flymake-diagnostic (diag)
  "Convert a Flymake DIAG to Flycheck error format.
Only converts diagnostics whose level is in `flyover-levels'."
  (let* ((beg (flymake-diagnostic-beg diag))
         (type (flymake-diagnostic-type diag))
         (text (flymake-diagnostic-text diag))
         (level (flyover--flymake-type-to-level type)))
    ;; Only convert if the level is enabled
    (when (memq level flyover-levels)
      (flycheck-error-new-at
       (line-number-at-pos beg)
       (save-excursion
         (goto-char beg)
         (current-column))
       level
       text))))

(defun flyover--flymake-type-to-level (type)
  "Convert Flymake diagnostic TYPE to Flycheck level.
Handles various forms that Flymake types can take."
  (let ((type-str (cond
                   ((symbolp type) (symbol-name type))
                   ((stringp type) type)
                   (t (format "%s" type)))))
    (cond
     ;; Handle keyword symbols
     ((eq type :error) 'error)
     ((eq type :warning) 'warning)
     ((eq type :note) 'info)
     ;; Handle regular symbols
     ((eq type 'flymake-error) 'error)
     ((eq type 'flymake-warning) 'warning)
     ((eq type 'flymake-note) 'info)
     ((eq type 'error) 'error)
     ((eq type 'warning) 'warning)
     ((eq type 'note) 'info)
     ((eq type 'info) 'info)
     ;; Handle string forms (case-insensitive)
     ((string-match-p "error" (downcase type-str)) 'error)
     ((string-match-p "warn" (downcase type-str)) 'warning)
     ((string-match-p "note\\|info" (downcase type-str)) 'info)
     ;; Default fallback
     (t 'warning))))

(defun flyover--get-all-errors ()
  "Get all errors from enabled checkers."
  (let (all-errors)
    (when (memq 'flycheck flyover-checkers)
      (setq all-errors (append all-errors flycheck-current-errors)))
    (when (memq 'flymake flyover-checkers)
      (let ((flymake-diagnostics (flyover--get-flymake-diagnostics)))
        (when flymake-diagnostics
          (setq all-errors
                (append all-errors
                        (delq nil (mapcar #'flyover--convert-flymake-diagnostic
                                          flymake-diagnostics)))))))
    all-errors))

;; Debug helper function
(defun flyover--debug-flymake-types ()
  "Debug function to see what types Flymake is actually returning."
  (interactive)
  (when (and (bound-and-true-p flymake-mode)
             (fboundp 'flymake-diagnostics))
    (let ((diagnostics (flymake-diagnostics)))
      (message "Flymake diagnostic types found:")
      (dolist (diag diagnostics)
        (let ((type (flymake-diagnostic-type diag)))
          (message "Type: %S (class: %s)" type (type-of type)))))))
(defun flyover--error-position-< (err1 err2)
  "Compare two errors ERR1 and ERR2 by position."
  (let ((line1 (flycheck-error-line err1))
        (line2 (flycheck-error-line err2))
        (col1 (flycheck-error-column err1))
        (col2 (flycheck-error-column err2)))
    (or (< line1 line2)
        (and (= line1 line2)
             (< (or col1 0) (or col2 0))))))

(defun flyover--is-valid-error (err)
  "Check if ERR is a valid flycheck error with proper positioning."
  (and err
       (not (eq err t))
       (flycheck-error-p err)
       (let ((line (flycheck-error-line err))
             (column (flycheck-error-column err))
             (level (flycheck-error-level err)))
         (and (numberp line)
              (>= line 0)
              (or (not column)
                  (and (numberp column) (>= column 0)))
              (memq level flyover-levels)))))

(defun flyover--filter-errors (errors)
  "Filter out invalid ERRORS.
This function ensures all errors are valid and have proper positions."
  (condition-case filter-err
      (when (and errors (listp errors))
        (let ((valid-errors (seq-filter #'flyover--is-valid-error errors)))
          (when flyover-debug
            (message "Debug: Valid errors: %S" valid-errors))
          valid-errors))
    (error
     (flyover--handle-error 'filter-error filter-err "error filtering" "filter-errors"))))

(defun flyover--get-error-region (err)
  "Get the start and end position for ERR."
  (condition-case region-err
      (save-excursion
        (save-restriction
          (widen)
          (let* ((line (flycheck-error-line err))
                 (start-pos (progn
                              (goto-char (point-min))
                              (forward-line (1- line))  ; line is 1-based
                              (point))))  ; Always get position, even for empty lines
            (when start-pos
              (goto-char start-pos)
              (let ((end-pos (line-end-position)))
                ;; For empty lines, ensure we still have a valid region
                (when (= start-pos end-pos)
                  (setq end-pos (1+ start-pos)))
                (when (and (numberp start-pos) (numberp end-pos)
                           (> start-pos 0) (> end-pos 0)
                           (<= start-pos (point-max))
                           (<= end-pos (point-max)))
                  (cons start-pos end-pos)))))))
    (error
     (flyover--handle-error 'region-error region-err "get-error-region" "get-error-region"))))

(defun flyover--create-overlay (region level msg &optional error)
  "Create an overlay at REGION with LEVEL and message MSG.
REGION should be a cons cell (BEG . END) of buffer positions.
LEVEL is the error level (error, warning, or info).
ERROR is the optional original flycheck error object."
  (let ((overlay nil))
    (condition-case ov-err
        (let* ((beg (car region))
               (end (cdr region)))
          (save-excursion
            (goto-char (min end (point-max)))
            (let* ((next-line-beg (if flyover-show-at-eol
                                      end
                                    (progn
                                      (forward-line flyover-line-position-offset)
                                      (line-beginning-position))))
                   (face (flyover--get-face level)))
              (when (and (numberp beg)
                         (numberp end)
                         (numberp next-line-beg)
                         (> beg 0)
                         (> end 0)
                         (> next-line-beg 0)
                         (<= beg (point-max))
                         (<= end (point-max))
                         (<= next-line-beg (point-max)))
                (setq overlay (make-overlay beg next-line-beg))
                (when (overlayp overlay)
                  (flyover--configure-overlay overlay face msg beg error))))))
      (error
       (flyover--handle-error 'overlay-creation ov-err "create-overlay"
                              (format "region=%S level=%S" region level))))
    overlay))

(defun flyover--get-face (type)
  "Return the face corresponding to the error TYPE."
  (pcase type
    ('error 'flyover-error)
    ('warning 'flyover-warning)
    ('info 'flyover-info)
    ;; Handle string type names (from flymake conversion)
    ("error" 'flyover-error)
    ("warning" 'flyover-warning)
    ("info" 'flyover-info)
    ;; Default to warning for any other type
    (_ 'flyover-warning)))

(defun flyover--get-indicator (type color)
  "Return the indicator string corresponding to the error TYPE COLOR."
  (let* ((props (pcase type
                  ('flyover-error
                   (cons flyover-error-icon 'flyover-error))
                  ('flyover-warning
                   (cons flyover-warning-icon 'flyover-warning))
                  ('flyover-info
                   (cons flyover-info-icon 'flyover-info))
                  (_
                   (cons flyover-warning-icon 'flyover-warning))))
         (icon (car props))
         (face-name (cdr props))
         (height (face-attribute face-name :height))
         (bg (if flyover-use-theme-colors
                 (pcase face-name
                   ('flyover-error
                    (flyover--get-theme-face-color 'error :foreground))
                   ('flyover-warning
                    (flyover--get-theme-face-color 'warning :foreground))
                   ('flyover-info
                    (flyover--get-theme-face-color 'success :foreground))
                   (_ (face-attribute face-name :foreground)))
               (face-attribute face-name :foreground)))
         (bg-color (flyover--darken-color bg flyover-percent-darker)))
    
    (concat
     ;; Left padding
     (propertize " "
                 'face `(:background ,bg-color, :height ,height)
                 'display '(space :width flyover-icon-left-padding))
     ;; Icon
     (propertize icon
                 'face `(:foreground ,color :background ,bg-color, :height ,height)
                 'display '(raise -0.02))
     ;; Right padding
     (propertize " "
                 'face `(:background ,bg-color, :height ,height)
                 'display '(space :width flyover-icon-right-padding)))))


(defun flyover--calculate-overlay-priority (error)
  "Calculate overlay priority based on ERROR level and column position."
  (let* ((col-pos (when (flycheck-error-p error)
                    (or (flycheck-error-column error) 0)))
         (level-priority (pcase (flycheck-error-level error)
                           ('error flyover--error-priority)
                           ('warning flyover--warning-priority)
                           ('info flyover--info-priority)
                           (_ flyover--warning-priority))))
    (- level-priority (or col-pos 0))))

(defun flyover--setup-basic-overlay-properties (overlay error)
  "Set up basic properties for OVERLAY with ERROR."
  (overlay-put overlay 'flyover t)
  (overlay-put overlay 'evaporate t)
  (overlay-put overlay 'modification-hooks
               '(flyover--clear-overlay-on-modification))
  (overlay-put overlay 'priority (flyover--calculate-overlay-priority error))
  (when (flycheck-error-p error)
    (overlay-put overlay 'flycheck-error error)))

(defun flyover--create-overlay-display-components (face error msg)
  "Create display components for overlay with FACE, ERROR, and MSG.
Returns a plist with :fg-color, :bg-color, :tinted-fg, :face-with-colors,
:indicator, :virtual-line, and :marked-string."
  (let* ((colors (flyover--get-face-colors (flycheck-error-level error)))
         (fg-color (car colors))
         (bg-color (cdr colors))
         (tinted-fg (if flyover-text-tint
                        (flyover--tint-color
                         fg-color
                         flyover-text-tint
                         flyover-text-tint-percent)
                      fg-color))
         (face-with-colors `(:inherit ,face
                                      :foreground ,tinted-fg
                                      :background ,bg-color))
         (indicator (flyover--get-indicator face tinted-fg))
         (virtual-line (when flyover-show-virtual-line
                         (propertize (flyover-get-arrow)
                                     'face `(:foreground ,fg-color))))
         (display-msg (concat " " msg " "))
         (marked-string (flyover--mark-all-symbols
                         :input (propertize display-msg
                                            'face face-with-colors
                                            'cursor-sensor-functions nil
                                            'rear-nonsticky t)
                         :regex flyover-regex-mark-quotes
                         :property `(:inherit flyover-marker
                                              :background ,bg-color))))
    (list :fg-color fg-color
          :bg-color bg-color
          :tinted-fg tinted-fg
          :face-with-colors face-with-colors
          :indicator indicator
          :virtual-line virtual-line
          :marked-string marked-string)))

(defun flyover--build-final-overlay-string (components error msg)
  "Build the final overlay string from display COMPONENTS, ERROR, and MSG."
  (let* ((col-pos (when (flycheck-error-p error)
                    (or (flycheck-error-column error) 0)))
         (line-content (string-trim
                        (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
         (is-empty-line (string-empty-p line-content))
         (indicator (plist-get components :indicator))
         (virtual-line (plist-get components :virtual-line))
         (face-with-colors (plist-get components :face-with-colors))
         (bg-color (plist-get components :bg-color))
         (wrapped-lines (flyover--wrap-message msg flyover-max-line-length))
         (overlay-string (if flyover-show-at-eol
                             (concat " " virtual-line indicator
                                     (flyover--mark-all-symbols
                                      :input (propertize (concat " " (car wrapped-lines) " ")
                                                         'face face-with-colors)
                                      :regex flyover-regex-mark-quotes
                                      :property `(:inherit flyover-marker
                                                           :background ,bg-color)))
                           (flyover--create-multiline-overlay-string
                            (if is-empty-line 0 col-pos) virtual-line indicator
                            wrapped-lines face-with-colors bg-color))))
    (if flyover-show-at-eol
        overlay-string
      (concat overlay-string "\n"))))

(defun flyover--create-multiline-overlay-string (col-pos virtual-line indicator lines face-with-colors bg-color)
  "Create multiline overlay string for LINES.
COL-POS is the column position, VIRTUAL-LINE is the line indicator,
INDICATOR is the error/warning icon, LINES are the wrapped message lines,
FACE-WITH-COLORS is the face for text, and BG-COLOR is the background color."
  (when flyover-debug
    (message "Debug multiline overlay-string: starting with col-pos=%S lines=%S" col-pos lines))
  (let* ((spaces (if (and (not flyover-show-at-eol) col-pos)
                     (make-string col-pos ?\s)
                   ""))
         (first-line (car lines))
         (remaining-lines (cdr lines))
         (first-line-string (concat spaces
                                    virtual-line
                                    indicator
                                    (flyover--mark-all-symbols
                                     :input (propertize (concat " " first-line " ")
                                                        'face face-with-colors)
                                     :regex flyover-regex-mark-quotes
                                     :property `(:inherit flyover-marker
                                                          :background ,bg-color))))
         (continuation-lines (mapcar (lambda (line)
                                       (concat spaces
                                               (make-string (+ (length virtual-line)
                                                               (length indicator)) ?\s)
                                               
                                               (flyover--mark-all-symbols
                                                :input (propertize (concat " " line " ")
                                                                   'face face-with-colors)
                                                :regex flyover-regex-mark-quotes
                                                :property `(:inherit flyover-marker
                                                                     :background ,bg-color))))
                                     remaining-lines))
         (all-lines (cons first-line-string continuation-lines))
         (result-string (string-join all-lines "\n")))
    
    (when flyover-debug
      (message "Debug multiline overlay-string: created string successfully"))
    (flyover--mark-all-symbols
     :input result-string
     :regex flyover-regex-mark-parens
     :property `(:inherit flyover-marker :background ,bg-color))))

(defun flyover--configure-overlay (overlay face msg beg error)
  "Configure OVERLAY with FACE, MSG, BEG, and ERROR."
  (condition-case configure-err
      (when (overlayp overlay)
        (flyover--setup-basic-overlay-properties overlay error)
        (let* ((components (flyover--create-overlay-display-components face error msg))
               (final-string (flyover--build-final-overlay-string components error msg)))
          (overlay-put overlay 'after-string
                       (propertize final-string
                                   'rear-nonsticky t
                                   'cursor-sensor-functions nil))))
    (error
     (flyover--handle-error 'overlay-configuration configure-err
                            "configure-overlay" (format "beg=%S" beg)))))

(defun flyover--clear-overlay-on-modification (overlay &rest _)
  "Clear OVERLAY when the buffer is modified."
  (delete-overlay overlay))

(defun flyover--create-overlay-string (col-pos virtual-line indicator marked-string bg-color)
  "Create the overlay string.
COL-POS is the column position.
VIRTUAL-LINE is the line indicator.
INDICATOR is the error/warning icon.
MARKED-STRING is the message with marked symbols.
BG-COLOR is the background color."
  (when flyover-debug
    (message "Debug overlay-string: starting with col-pos=%S" col-pos))
  (let* ((spaces (if (and (not flyover-show-at-eol) col-pos)
                     (make-string col-pos ?\s)
                   ""))
         (result-string
          (if flyover-show-at-eol
              (concat " " indicator marked-string)
            (concat spaces
                   virtual-line
                   indicator
                   marked-string))))

    (when flyover-debug
      (message "Debug overlay-string: created string successfully"))
    (flyover--mark-all-symbols
     :input result-string
     :regex flyover-regex-mark-parens
     :property `(:inherit flyover-marker :background ,bg-color))))

(defun flyover-replace-curly-quotes (text)
  "Replace curly quotes with straight quotes in TEXT."
  (replace-regexp-in-string "[“”]" "\""
    (replace-regexp-in-string "[‘’]" "'" text)))

(cl-defun flyover--mark-all-symbols (&key input regex property)
  "Highlight all symbols matching REGEX in INPUT with specified PROPERTY."
  (save-match-data
    (setq input (flyover-replace-curly-quotes input))  ; Replace curly quotes with straight quotes
    (let ((pos 0))
      (while (string-match regex input pos)
        (let* ((start (match-beginning 1))
               (end (match-end 1))
               (existing-face (text-properties-at start input))
               (new-face (append existing-face (list 'face property))))
          (add-text-properties start end new-face input)
          (setq pos end))))
    input))

(defun flyover-errors-at (pos)
  "Return the Flycheck errors at POS."
  (delq nil (mapcar (lambda (ov)
                      (when-let* ((err (overlay-get ov 'flycheck-error)))
                        (when (flycheck-error-p err)
                          err)))
                    (overlays-at pos))))

(defun flyover--remove-checker-name (msg)
  "Remove checker name prefix from (as MSG).
If it appears at the start.
Ignores colons that appear within quotes or parentheses."
  (when flyover-hide-checker-name
    (let ((case-fold-search nil))
      ;; Match start of string, followed by any characters except quotes/parens,
      ;; followed by a colon, capturing everything after
      (if (string-match flyover-checker-regex (flyover-replace-curly-quotes msg))
          (setq msg (string-trim (match-string 1 msg))))))
  msg)

(defun flyover--wrap-message (msg max-length)
  "Wrap MSG to multiple lines with each line no longer than MAX-LENGTH.
Returns a list of strings, each representing a line."
  (if (not flyover-wrap-messages)
      (list msg)
    (let ((words (split-string msg " " t))
          (lines '())
          (current-line ""))
      (dolist (word words)
        (let ((potential-line (if (string-empty-p current-line)
                                 word
                               (concat current-line " " word))))
          (if (<= (length potential-line) max-length)
              (setq current-line potential-line)
            ;; Current word would make line too long
            (when (not (string-empty-p current-line))
              (push current-line lines))
            (setq current-line word))))
      ;; Add the last line if it's not empty
      (when (not (string-empty-p current-line))
        (push current-line lines))
      (nreverse lines))))

(defun flyover--display-errors (&optional errors)
  "Display ERRORS using overlays."
  (condition-case display-err
      (let* ((filtered-errors (flyover--filter-errors (or errors (flyover--get-all-errors))))
             (sorted-errors (progn
                              (when flyover-debug
                                (message "Before sorting: %S"
                                         (mapcar (lambda (err)
                                                   (cons (flycheck-error-line err)
                                                         (flycheck-error-column err)))
                                                 filtered-errors)))
                              (sort filtered-errors #'flyover--error-position-<))))
        (when flyover-debug
          (message "After sorting: %S"
                   (mapcar (lambda (err)
                             (cons (flycheck-error-line err)
                                   (flycheck-error-column err)))
                           sorted-errors))
          (message "Error levels: %S"
                   (mapcar #'flycheck-error-level sorted-errors)))

        (when filtered-errors
          (flyover--clear-overlays)
          ;; Reverse the list to maintain correct display order
          (setq flyover--overlays
                (cl-loop for err in filtered-errors
                         when (flycheck-error-p err)
                         for level = (flycheck-error-level err)
                         for msg = (flycheck-error-message err)
                         for cleaned-msg = (and msg (flyover--remove-checker-name msg))
                         for region = (and cleaned-msg (flyover--get-error-region err))
                         for overlay = (and region (flyover--create-overlay
                                                    region level cleaned-msg err))
                         when overlay
                         collect overlay))))
    (error
     (when flyover-debug
       (message "Debug: Display error: %S" display-err)))))

(defun flyover--clear-overlays ()
  "Remove all flycheck overlays from the current buffer."
  (dolist (ov flyover--overlays)
    (when (overlayp ov)
      (delete-overlay ov)))
  (setq flyover--overlays nil)
  ;; Remove any remaining flyover overlays
  (remove-overlays (point-min) (point-max) 'flyover t))

;;;###autoload
(define-minor-mode flyover-mode
  "Minor mode for displaying Flycheck errors using overlays."
  :lighter " fo"
  :group 'flyover
  (if flyover-mode
      (flyover--enable)
    (flyover--disable)))

(defun flyover--safe-add-hook (hook function)
  "Safely add FUNCTION to HOOK if not already present."
  (unless (memq function (symbol-value hook))
    (add-hook hook function nil t)))

(defun flyover--safe-remove-hook (hook function)
  "Safely remove FUNCTION from HOOK if present."
  (when (memq function (symbol-value hook))
    (remove-hook hook function t)))

(defun flyover--enable-flymake-hooks ()
  "Enable Flymake-specific hooks."
  (cond
   ;; Modern Emacs with the proper hook
   ((boundp 'flymake-after-update-diagnostics-hook)
    (flyover--safe-add-hook 'flymake-after-update-diagnostics-hook
                                     #'flyover--maybe-display-errors-debounced))
   ;; Emacs 29+ with different hook name
   ((boundp 'flymake-diagnostics-updated-hook)
    (flyover--safe-add-hook 'flymake-diagnostics-updated-hook
                                     #'flyover--maybe-display-errors-debounced))
   ;; Fallback for older versions
   (t
    (flyover--safe-add-hook 'after-save-hook
                                     #'flyover--maybe-display-errors-debounced)
    (advice-add 'flymake-handle-report :after
                #'flyover--maybe-display-errors-debounced))))

(defun flyover--disable-flymake-hooks ()
  "Disable Flymake-specific hooks."
  (when (boundp 'flymake-after-update-diagnostics-hook)
    (flyover--safe-remove-hook 'flymake-after-update-diagnostics-hook
                                        #'flyover--maybe-display-errors-debounced))
  (when (boundp 'flymake-diagnostics-updated-hook)
    (flyover--safe-remove-hook 'flymake-diagnostics-updated-hook
                                        #'flyover--maybe-display-errors-debounced))
  (flyover--safe-remove-hook 'after-save-hook
                                      #'flyover--maybe-display-errors-debounced)
  (advice-remove 'flymake-handle-report
                 #'flyover--maybe-display-errors-debounced))

(defun flyover--enable ()
  "Enable Flycheck/Flymake overlay mode."
  (flyover--ensure-flycheck)
  (when (and (memq 'flycheck flyover-checkers)
             (featurep 'flycheck))
    (flyover--safe-add-hook 'flycheck-after-syntax-check-hook
                                     #'flyover--maybe-display-errors-debounced))
  (when (memq 'flymake flyover-checkers)
    (flyover--enable-flymake-hooks))
  
  (flyover--safe-add-hook 'after-change-functions
                                   #'flyover--handle-buffer-changes)
  ;; Force initial display of existing errors
  (flyover--maybe-display-errors))

(defun flyover--disable ()
  "Disable Flycheck/Flymake overlay mode."
  (flyover--safe-remove-hook 'flycheck-after-syntax-check-hook
                                      #'flyover--maybe-display-errors-debounced)
  (when (memq 'flymake flyover-checkers)
    (flyover--disable-flymake-hooks))
  
  (flyover--safe-remove-hook 'after-change-functions
                                      #'flyover--handle-buffer-changes)
  (flyover--clear-overlays))

(defun flyover--maybe-display-errors ()
  "Display errors except on current line."
  (unless (buffer-modified-p)
    (let ((current-line (line-number-at-pos))
          (current-col (current-column))
          (to-delete))
      (flyover--display-errors)
      (dolist (ov flyover--overlays)
        (when (and (overlayp ov)
                   (= (line-number-at-pos (overlay-start ov)) current-line))
          (when flyover-hide-when-cursor-is-on-same-line
            (push ov to-delete))
          (when (and flyover-hide-when-cursor-is-at-same-line
                     (overlay-get ov 'flycheck-error)
                     (let ((error (overlay-get ov 'flycheck-error)))
                       (= (if (featurep 'flycheck)
                              (flycheck-error-column error)
                            (plist-get error :column))
                          current-col)))
            (push ov to-delete))))
      ;; Delete collected overlays
      (dolist (ov to-delete)
        (delete-overlay ov)
        (setq flyover--overlays (delq ov flyover--overlays))))))

(defun flyover--maybe-display-errors-debounced ()
  "Debounced version of `flyover--maybe-display-errors`."
  (condition-case err
      (progn
        (when flyover--debounce-timer
          (cancel-timer flyover--debounce-timer))
        (setq flyover--debounce-timer
              (run-with-idle-timer flyover-debounce-interval nil
                                   #'flyover--maybe-display-errors)))
    (error
     (message "Error in debounced display: %S" err)
     (setq flyover--debounce-timer nil))))

(defun flyover--handle-buffer-changes (beg end _len)
  "Handle buffer modifications by clearing overlays on the modified lines.
BEG and END mark the beginning and end of the changed region."
  (condition-case err
      (when (buffer-modified-p)
        (let* ((beg-line (line-number-at-pos beg))
               (end-line (line-number-at-pos end)))
          ;; Remove overlays on all modified lines
          (dolist (ov flyover--overlays)
            (when (and ov
                       (overlayp ov)
                       (overlay-buffer ov)  ; Check if overlay is still valid
                       (let ((ov-line (line-number-at-pos (overlay-start ov))))
                         (and (>= ov-line beg-line) (<= ov-line end-line))))
              (delete-overlay ov)))
          ;; Update our list of valid overlays
          (setq flyover--overlays
                (cl-remove-if-not (lambda (ov)
                                    (and (overlayp ov)
                                        (overlay-buffer ov)))
                                  flyover--overlays))))
    (error
     (message "Error in flyover--handle-buffer-changes: %S" err))))

(defun flyover--get-cached-color (cache-key color-fn &rest args)
  "Get color from cache or compute and CACHE-KEY it using COLOR-FN with ARGS."
  (or (gethash cache-key flyover--color-cache)
      (puthash cache-key (apply color-fn args) flyover--color-cache)))

(defun flyover--clear-color-cache ()
  "Clear the color cache."
  (clrhash flyover--color-cache))

(defun flyover--color-to-rgb (color)
  "Convert COLOR (hex or name) to RGB components."
  (let ((cache-key (format "rgb-%s" color)))
    (flyover--get-cached-color
     cache-key
     (lambda (c)
       (let ((rgb (color-values c)))
         (if rgb
             (mapcar (lambda (x) (/ x 256)) rgb)
           (error "Invalid color: %s" c))))
     color)))

(defun flyover--rgb-to-hex (r g b)
  "Convert R G B components to hex color string."
  (format "#%02x%02x%02x" r g b))

(defun flyover--darken-color (color percent)
  "Darken COLOR by PERCENT."
  (let ((cache-key (format "darken-%s-%d" color percent)))
    (flyover--get-cached-color
     cache-key
     (lambda (c p)
       (let* ((rgb (flyover--color-to-rgb c))
              (darkened (mapcar (lambda (component)
                                  (min 255
                                      (floor (* component (- 100 p) 0.01))))
                                rgb)))
         (apply #'flyover--rgb-to-hex darkened)))
     color percent)))

(defun flyover--get-theme-face-color (face-name attribute &optional fallback)
  "Get color for FACE-NAME's ATTRIBUTE from current theme.
If not found, return FALLBACK color."
  (let ((color (face-attribute face-name attribute nil t)))
    (if (or (eq color 'unspecified) (not color))
        fallback
      color)))

(defun flyover--create-background-from-foreground (fg-color lightness)
  "Create a background color from FG-COLOR with LIGHTNESS percent.
Lower LIGHTNESS values create darker backgrounds."
  (let ((cache-key (format "bg-%s-%d" fg-color lightness)))
    (flyover--get-cached-color
     cache-key
     (lambda (fg l)
       (let* ((rgb (flyover--color-to-rgb fg))
              (bg (mapcar (lambda (component)
                            (min 255
                                (floor (* component (/ l 100.0)))))
                          rgb)))
         (apply #'flyover--rgb-to-hex bg)))
     fg-color lightness)))

(defun flyover--get-face-colors (level)
  "Get foreground and background colors for error LEVEL.
Uses theme colors when `flyover-use-theme-colors' is non-nil."
  (let ((fg (pcase level
              ((or 'error "error") (flyover--get-theme-face-color 'error :foreground "#ea8faa"))
              ((or 'warning "warning") (flyover--get-theme-face-color 'warning :foreground "#DCA561"))
              ((or 'info "info") (flyover--get-theme-face-color 'success :foreground "#a8e3a9"))
              (_ (flyover--get-theme-face-color 'warning :foreground "#DCA561")))))
    (cons fg (flyover--create-background-from-foreground fg flyover-background-lightness))))

(defun flyover--tint-color (color tint percent)
  "Tint COLOR according to TINT type and PERCENT amount.
TINT should be either =\'lighter or =\'darker."
  (pcase tint
    ('lighter
     (let* ((rgb (flyover--color-to-rgb color))
            (lightened (mapcar (lambda (component)
                                 (min 255
                                      (floor (+ component (* (- 255 component) (/ percent 100.0))))))
                               rgb)))
       (apply #'flyover--rgb-to-hex lightened)))
    ('darker
     (flyover--darken-color color percent))
    (_ color)))

(defun flyover-toggle ()
  "Toggle Flycheck Overlay mode."
  (interactive)
  (if flyover-mode
      (flyover-mode -1)
    (flyover-mode 1)))

(provide 'flyover)
;;; flyover.el ends here
