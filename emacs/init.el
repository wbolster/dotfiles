;;; init.el --- emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs configuration

;;; Code:

;; Bootstrap initialization

;; Reduce garbage collection: faster startup, also recommended for lsp-mode.
(setopt gc-cons-threshold (* 100 1024 1024))

(setopt load-prefer-newer t)

;; Make everything relative to where this file actually lives.
(setopt user-emacs-directory
    (concat "~/" (file-name-directory (file-symlink-p user-init-file))))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Bootstrap packages

(require 'package)
(setopt
 package-enable-at-startup nil
 package-user-dir (locate-user-emacs-file "packages"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package
  :demand t
  :custom
  ;; todo:enable ‘use-package-always-defer’ when everything that needs
  ;; it has ‘:demand t’
  ;; (use-package-always-defer t)
  (use-package-always-ensure t)
  (use-package-compute-statistics t)
  (use-package-enable-imenu-support t)
  (use-package-hook-name-suffix nil)
  (use-package-verbose t))

(use-package benchmark-init
  :demand t
  :hook (after-init-hook . benchmark-init/deactivate))

;; Early packages

(use-package dash
  :demand t
  :functions
  -contains-p -each -filter -first -flatten -last -map -map-when -partial
  -remove -replace-at -separate -snoc -sort -uniq -zip-pair
  :config
  (global-dash-fontify-mode))

(use-package exec-path-from-shell
  :demand t
  :config
  (exec-path-from-shell-initialize))

(use-package general
  :demand t
  :functions
  general-chord)

(use-package no-littering
  :demand t
  :after xdg
  :init
  ;; no :custom + :config b/c the variable must be set before loading
  (setopt no-littering-var-directory (expand-file-name "emacs/" (xdg-state-home)))
  (no-littering-theme-backups))

(use-package xdg
  :demand t
  :functions
  xdg-state-home)

;; Regular config

(defgroup w nil
  "Personal configuration."
  :group 'emacs
  :prefix "w/")

(defcustom w/read-extended-command-predicate-functions
  '(command-completion-default-include-p)
  "Predicates used by ‘read-extended-command-predicate’."
  :group 'w
  :type '(repeat symbol))

(defcustom w/ui-font-family "Sans"
  "Name of the font family used by the desktop environment's user interface."
  :group 'w
  :type 'string)

(defcustom w/document-font-family "Sans"
  "Name of the document font family used by the desktop environment."
  :group 'w
  :type 'string)

(defcustom w/major-modes
  '(c-mode
    emacs-lisp-mode
    fundamental-mode
    jinja2-mode
    js-mode
    json-mode
    lisp-interaction-mode
    markdown-mode
    nxml-mode
    python-mode
    rst-mode
    sh-mode
    sql-mode
    text-mode
    typescript-ts-mode
    web-mode
    yaml-mode)
  "Commonly used major modes."
  :group 'w
  :type '(repeat symbol))

(use-package emacs
  :demand t
  :hook (emacs-startup-hook . (lambda () (load custom-file 'noerror)))
  :commands
  w/narrow-dwim
  w/switch-major-mode
  :functions
  w/set-cycle
  :custom
  (auto-save-interval 100)
  (blink-cursor-blinks 1)
  (blink-cursor-delay .5)
  (blink-cursor-interval .5)
  (create-lockfiles nil)
  (custom-safe-themes t)
  (default-frame-alist
    '((child-frame-border-width . 0)
      (drag-internal-border . 2)
      (height . 48)
      (internal-border-width . 1)
      (undecorated . t)
      (width . 160)))
  (delete-by-moving-to-trash t)
  (disabled-command-function nil)
  (display-buffer-base-action
   '((display-buffer-reuse-window
      display-buffer-pop-up-window
      display-buffer-use-some-window)))
  (echo-keystrokes 0.5)
  (find-file-visit-truename t)
  (fit-window-to-buffer-horizontally t)
  (frame-resize-pixelwise t)
  (frame-title-format "%b")
  (indicate-buffer-boundaries 'left)
  (indent-tabs-mode nil)
  (inhibit-startup-screen t)
  (initial-major-mode 'text-mode)
  (initial-scratch-message nil)
  (major-mode 'text-mode) ;; default for new buffers; initial-major-mode is not sufficient
  (mode-line-compact 'long)
  (mode-line-position-column-line-format '(" %l:%c"))
  (native-comp-async-report-warnings-errors 'silent)
  (read-extended-command-predicate
   (lambda (command buffer)
     (run-hook-with-args-until-failure 'w/read-extended-command-predicate-functions command buffer)))
  (read-process-output-max (* 1024 1024)) ;; recommended by lsp-mode
  (recenter-positions '(top middle bottom))
  (require-final-newline 'visit-save)
  (scroll-conservatively 101)
  (scroll-margin 5)
  (sentence-end-double-space nil)
  (split-height-threshold nil)
  (split-width-threshold 120)
  (split-window-preferred-function 'visual-fill-column-split-window-sensibly)
  (switch-to-buffer-in-dedicated-window 'pop)
  (tab-always-indent 'complete)
  (tab-first-completion 'word)
  (tab-width 4)
  (use-short-answers t)
  (window-combination-resize t)
  (window-divider-default-bottom-width 2)
  (window-divider-default-places t)
  (window-divider-default-right-width 2)
  (window-resize-pixelwise t)

  :delight
  (abbrev-mode " ⋯")
  (auto-fill-function " ↲")
  (indent-tabs-mode " ⇥")
  (visual-line-mode (:eval (unless w/wrap-lines-mode " ⇉")))

  :config
  (setopt custom-file (expand-file-name "custom.el" user-emacs-directory))

  (blink-cursor-mode)
  (context-menu-mode)
  (menu-bar-mode -1)
  (pixel-scroll-precision-mode)
  (repeat-mode)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (window-divider-mode)

  (defun w/narrow-dwim ()
    "Narrow (or widen) to defun or region."
    (interactive)
    (cond
     ((region-active-p)
      (narrow-to-region (region-beginning) (region-end))
      (deactivate-mark)
      (message "Showing region only"))
     ((buffer-narrowed-p)
      (widen)
      (message "Showing everything"))
     (t
      (narrow-to-defun)
      (message "Showing defun only"))))

  (defun w/set-cycle (symbol values &optional set-fn)
    "Cycle the value of SYMBOL using the specified VALUES."
    (-let* ((current-index (or (seq-position values (symbol-value symbol)) -1))
            (new-index (mod (1+ current-index) (length values)))
            (new-value (nth new-index values)))
      (funcall (or set-fn 'customize-set-variable) symbol new-value)
      new-value))

  (defun w/switch-major-mode ()
    "Switch major mode."
    (interactive)
    (require 'dash)
    (when-let*
        ((major-modes (cons 'normal-mode w/major-modes))
         (choices
          (--map
           (let*
               ((name (string-remove-suffix "-mode" (symbol-name it)))
                (label
                 (and-let*
                     ((docstring (documentation it t))
                      (noise
                       (rx bos
                           (| "Major mode" "Simple mode")
                           (? (| " for editing" " for" " to edit"))))
                      (description
                       (->> (car (split-string docstring "\n" t))
                            (replace-regexp-in-string noise "")
                            (string-remove-suffix ".")
                            (string-trim)))
                      (label (format "%s: %s" name description))))))
             (cons (or label name) it))
           major-modes))
         (choice (completing-read "Switch major mode: " (mapcar #'car choices) nil t))
         (fn (cdr (assoc choice choices))))
      (funcall fn))))

(use-package emacs
  :demand t
  :if (and (display-graphic-p) (eq system-type 'darwin)) ;; macOS
  :bind
  ("s-q" . nil)
  :custom
  (ns-right-alternate-modifier 'none)
  (ns-use-native-fullscreen nil))

(use-package aggressive-indent
  :defer t
  :delight " ⇤")

(use-package auto-compile
  :demand t
  :custom
  (auto-compile-update-autoloads t)
  :config
  (auto-compile-on-load-mode))

(use-package auto-dark
  :demand t
  :if (display-graphic-p)
  :after modus-themes solarized-theme
  :custom
  (auto-dark-themes '((modus-vivendi-tinted) (modus-operandi-tinted)))
  :commands
  auto-dark-toggle-appearance
  :config
  (auto-dark-mode))

(use-package cape
  :demand t
  :general
  (:states 'insert
   "C-<tab>" cape-prefix-map)
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(use-package colorful-mode
  :defer t
  :custom
  (colorful-use-prefix t)
  (colorful-prefix-string "⬤"))

(use-package conf-mode
  :defer t
  :hook (conf-toml-mode-hook . w/conf-toml-mode-hook)
  :functions w/toml-format-taplo-region
  :config
  (defun w/conf-toml-mode-hook ()
    (setopt
     tab-width 2
     evil-shift-width tab-width)
    (reformatter-dwim-select 'w/toml-format-taplo))
  (reformatter-define w/toml-format-taplo
    :group 'toml
    :program "taplo"
    :args '("format" "-")))

(use-package consult
  :demand t
  :general
  (:states 'motion
   "/" #'consult-line
   "?" #'consult-line-multi)
  :commands
  w/consult-line-from-isearch
  w/consult-pulse-after-final-jump
  w/consult-scroll-point-into-view
  :config
  (remove-hook 'consult-after-jump-hook #'recenter)
  (add-hook 'consult-after-jump-hook #'w/consult-pulse-after-final-jump)
  (add-hook 'consult-after-jump-hook #'w/consult-scroll-point-into-view)
  (defun w/consult-line-from-isearch ()
    "Call ‘consult-line’ with the ‘isearch’ search string."
    (interactive)
    (consult-line isearch-string))
  (defun w/consult-pulse-after-final-jump ()
    "Highlight the jump target, unless completion is still active."
    (unless (active-minibuffer-window)
      (pulse-momentary-highlight-one-line)))
  (defun w/consult-scroll-point-into-view ()
    "Scroll the jump target into view."
    (unless (pos-visible-in-window-p)
      (let ((screen-line
             (if (< (point) (window-start)) scroll-margin (- -1 scroll-margin))))
        (recenter screen-line)))))

(use-package corfu
  :demand t
  :bind
  (:map corfu-map
   ("<tab>" . corfu-next)
   ("<backtab>" . corfu-previous)
   ("C-/" . #'w/corfu-move-to-minibuffer))
  :commands
  w/corfu-move-to-minibuffer
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :config
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-echo-mode)
  (defun w/corfu-move-to-minibuffer ()
    ;; based on https://github.com/minad/corfu
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (add-to-list 'corfu-continue-commands #'w/corfu-move-to-minibuffer))

(use-package css-mode
  :defer t
  :hook
  (css-mode-hook . w/css-mode-hook)
  (scss-mode-hook . w/css-mode-hook)
  :custom
  (css-fontify-colors nil)
  :config
  (defun w/css-mode-hook ()
    (lsp-deferred)
    (reformatter-dwim-select 'prettier-format)
    (colorful-mode)
    (modify-syntax-entry ?. ".")
    (modify-syntax-entry ?- "_")))

(use-package crux
  :defer t)

(use-package delight
  :demand t)

(use-package desktop
  :demand t
  :custom
  (desktop-auto-save-timeout 10)
  (desktop-load-locked-desktop 'check-pid)
  (desktop-restore-eager 5)
  :config
  (desktop-save-mode))

(use-package dired
  :demand t
  :ensure nil
  :functions
  dired-get-filename
  :general
  (:keymaps 'dired-mode-map
   :states '(motion normal)
   "-" #'dired-jump))

(use-package dired-x
  :demand t
  :after dired
  :ensure nil)

(use-package direnv
  :demand t
  :after exec-path-from-shell
  :if (executable-find "direnv")
  :hook (direnv-envrc-mode-hook . w/direnv-envrc-mode-hook)
  :config
  (direnv-mode)
  (defun w/direnv-envrc-mode-hook ()
    (add-hook 'after-save-hook #'direnv-allow)))

(use-package display-line-numbers
  :defer t
  :commands
  w/display-line-numbers-cycle
  :config
  (defun w/display-line-numbers-cycle ()
    (interactive)
    (unless display-line-numbers-mode
      (display-line-numbers-mode))
    (let ((new-value (w/set-cycle 'display-line-numbers '(t visual relative) 'set)))
      (message "Line numbering style: %s" (if (equal new-value t) 'absolute new-value)))))

(use-package docker
  :defer t)

(use-package dockerfile-mode
  :defer t
  :mode
  (rx "Dockerfile" (any "-_.") (* any) string-end)
  :hook (dockerfile-mode-hook . w/dockerfile-mode-hook)
  :general
  (:keymaps 'dockerfile-mode-map
   :states 'normal
   "<return> "'w/split-line-backslash
   "<remap> <evil-join>" #'w/evil-join-smart-backslash-eol)
  :config
  (defun w/dockerfile-mode-hook ()
    (modify-syntax-entry ?$ ".")))

(use-package ediff
  :defer t
  :custom
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package el-patch
  :defer t)

(use-package eldoc
  :defer t
  :delight)

(use-package elec-pair
  :demand t
  :functions
  electric-pair-default-inhibit
  :custom
  (electric-pair-inhibit-predicate #'w/electric-pair-inhibit)
  :config
  (defun w/electric-pair-inhibit (char)
    (or (minibufferp)
        (memq (bound-and-true-p vertico-map) (current-active-maps))
        (electric-pair-default-inhibit char)))
  (electric-pair-mode))

(use-package evil
  :demand t
  :hook (enable-theme-functions . w/tweak-evil-cursor)
  :general
  (:states 'motion
   "<tab>" 'evil-toggle-fold
   "C-<tab>" 'evil-jump-forward
   ";" #'evil-ex
   "z e" #'evil-scroll-line-up
   "z n" #'evil-scroll-line-down)
  (:states '(motion normal)
   [escape] #'w/evil-normal-state-cleanup)
  (:states '(motion normal visual)
   [remap evil-next-line] #'w/evil-next-line
   [remap evil-previous-line] #'w/evil-previous-line
   [remap evil-end-of-line] #'w/evil-end-of-line
   [remap evil-first-non-blank] #'w/evil-first-non-blank)
  (:states 'normal
   ;; useful for keychron k7 keyboards that require a Fn modifier for
   ;; tilde; use shift-tab (key below it) it as a workaround.
   "<backtab>" #'evil-invert-char)
  (:states 'visual
   "x" #'evil-exchange)
  (:states '(operator visual)
   "o" #'w/evil-text-object-symbol-dwim)
  (:states 'operator
   ;; the empty text object is a trick to make it possible to
   ;; quickly swap two text objects using evil-exchange "gx";
   ;; "gxp" move previously marked text without moving anything
   ;; back to the original location, or vice versa.
   "p" #'w/evil-empty-text-object)
  (:keymaps 'evil-outer-text-objects-map
   "g" #'w/evil-text-object-whole-buffer)
  (:states 'insert
   "<return>" #'comment-indent-new-line
   "C-a" #'w/evil-first-non-blank
   "C-c" #'evil-normal-state
   "C-d" #'delete-char
   "C-e" #'end-of-visual-line
   "C-h" [backspace]
   "C-k" #'w/kill-line-dwim
   "C-m" #'comment-indent-new-line
   "C-n" #'next-line
   "C-o" #'evil-normal-state
   "C-p" #'previous-line
   "C-t" #'w/evil-transpose-chars
   "C-v" #'yank  ;; during typing, ctrl-v is "paste", like everywhere else
   "C-SPC" #'cycle-spacing
   "C-'" #'w/typo-cycle-quotation-marks
   "C-," #'evil-shift-left-line  ;; shift line with < and > (same
   "C-<" #'evil-shift-left-line  ;; chars as in normal mode);
   "C-." #'evil-shift-right-line ;; used instead of standard vim
   "C->" #'evil-shift-right-line ;; bindings C-d and C-t.
   "C-=" (lambda () (interactive) (save-excursion (call-interactively #'evil-indent-line))))
  (:states '(insert replace)
   (general-chord "qw") #'evil-normal-state
   (general-chord "wq") #'evil-normal-state
   "C-g" #'evil-normal-state)
  :custom
  (evil-cross-lines t)
  (evil-emacs-state-tag "e ")
  (evil-insert-state-message nil)
  (evil-insert-state-tag "i ")
  (evil-motion-state-tag "m ")
  (evil-normal-state-tag "  ")
  (evil-operator-state-tag "o ")
  (evil-replace-state-tag "r ")
  (evil-shift-round nil)
  (evil-split-window-below t)
  (evil-undo-system 'undo-redo)
  (evil-visual-state-tag "v ")
  (evil-vsplit-window-right t)
  (evil-want-C-u-scroll t)
  (evil-want-C-w-in-emacs-state t)
  :commands
  evil-a-symbol
  evil-append-line
  evil-avy-goto-char-timer
  evil-avy-goto-line
  evil-avy-goto-line-above
  evil-avy-goto-line-below
  evil-backward-WORD-begin
  evil-buffer-new
  evil-change-to-initial-state
  evil-declare-repeat
  evil-define-key*
  evil-delete
  evil-delete-whole-line
  evil-end-of-line
  evil-end-of-visual-line
  evil-find-file-at-point-with-line
  evil-first-non-blank
  evil-first-non-blank-of-visual-line
  evil-forward-WORD-begin
  evil-forward-char
  evil-forward-word
  evil-indent-line
  evil-inner-symbol
  evil-insert
  evil-insert-newline-above
  evil-insert-newline-below
  evil-join
  evil-next-buffer
  evil-next-visual-line
  evil-org-set-key-theme
  evil-paste-pop
  evil-paste-pop-next
  evil-prev-buffer
  evil-previous-visual-line
  evil-repeat-start
  evil-repeat-stop
  evil-set-initial-state
  evil-set-jump
  evil-window-decrease-height
  evil-window-decrease-width
  evil-window-delete
  evil-window-down
  evil-window-increase-height
  evil-window-increase-width
  evil-window-left
  evil-window-new
  evil-window-next
  evil-window-prev
  evil-window-right
  evil-window-rotate-downwards
  evil-window-rotate-upwards
  evil-window-split
  evil-window-top-left
  evil-window-up
  evil-window-vnew
  evil-window-vsplit
  :functions
  evil-add-command-properties
  evil-initial-state-for-buffer
  evil-range
  evil-set-auxiliary-keymap
  :init
  (setopt
   evil-respect-visual-line-mode t
   evil-want-keybinding nil
   evil-want-integration t)
  :config
  (evil-mode)

  ;; use Y to copy to the end of the line; see evil-want-Y-yank-to-eol
  (evil-add-command-properties 'evil-yank-line :motion 'evil-end-of-line)

  ;; major modes may use a different lookup function
  (make-variable-buffer-local 'evil-lookup-func)

  ;; type numbers by holding alt using home row keys and by having a
  ;; "numpad overlay" starting at the home position for my right hand.
  (--each (-zip-pair (split-string "arstdhneio'luy7890km.," "" t)
                     (split-string "87659012345456789000.," "" t))
    (-let [(key . num) it]
      (general-define-key
       :states 'insert
       (concat "M-" key)
       (lambda () (interactive) (insert num)))))
  (general-define-key
   :states 'insert
   "M-DEL" 'backward-delete-char-untabify)

  (defun w/tweak-evil-cursor (_theme)
    "Tweak the appearance of the evil cursors."
    (setopt
     evil-motion-state-cursor (list solarized-color-yellow 'box)
     evil-normal-state-cursor (list solarized-color-yellow 'box)
     evil-visual-state-cursor (list solarized-color-yellow 'hollow)
     evil-insert-state-cursor  (list solarized-color-yellow 'bar)
     evil-replace-state-cursor (list solarized-color-magenta 'hbar)
     evil-operator-state-cursor (list solarized-color-magenta 'hollow)))

  (defun w/evil-normal-state-cleanup ()
    "Like `evil-force-normal-state', with some extra cleanups."
    (interactive)
    (when (eq last-command 'w/evil-normal-state-cleanup)
      ;; clean up some more when called twice in row
      (lazy-highlight-cleanup t)
      (remove-overlays nil nil 'category 'evil-snipe)
      (symbol-overlay-remove-all)
      (when (functionp 'flycheck-clear)
        (flycheck-clear))
      (when (functionp 'lsp-ui-doc-hide)
        (lsp-ui-doc-hide))
      (when (functionp 'evil-mc-undo-all-cursors)
        (evil-mc-undo-all-cursors))
      (let ((inhibit-message t))
        (evil-exchange-cancel)))
    (evil-force-normal-state)
    (when (eq (evil-initial-state-for-buffer) 'motion)
      (evil-change-to-initial-state)))

  (defun w/evil-transpose-chars ()
    "Invoke ‘transpose-chars’ on the right chars in insert state."
    (interactive)
    (backward-char)
    (transpose-chars nil)
    (unless (eolp) (forward-char)))

  (defun w/kill-line-dwim ()
    "Kill line, or join the next line when at eolp."
    (interactive)
    (let ((was-at-eol (eolp)))
      (kill-line)
      (when was-at-eol
        (fixup-whitespace))))

  (evil-define-motion w/evil-next-line (count)
    (when (null count)
      (setq count 1))
    (if visual-line-mode
        (progn
          (setq evil-this-type 'exclusive)
          (evil-next-visual-line count))
      (setq evil-this-type 'line)
      (evil-next-line count)))

  (evil-define-motion w/evil-previous-line (count)
    (when (null count)
      (setq count 1))
    (if visual-line-mode
        (progn
          (setq evil-this-type 'exclusive)
          (evil-previous-visual-line count))
      (setq evil-this-type 'line)
      (evil-previous-line count)))

  (evil-define-motion w/evil-end-of-line (count)
    :type inclusive
    (if visual-line-mode
        (evil-end-of-visual-line count)
      (evil-end-of-line count)))

  (evil-define-motion w/evil-first-non-blank ()
    :type exclusive
    (if visual-line-mode
        (evil-first-non-blank-of-visual-line)
      (evil-first-non-blank)))

  (evil-define-operator w/evil-join-smart-backslash-eol (beg end)
    "Like ‘evil-join’, but handles continuation line endings in a smarter way."
    :motion evil-line
    (prog1 (evil-join beg end)
      ;; delete ‘\’, and potentially one space before it
      (when (looking-back "\\\\" nil)
        (delete-char -1))
      (when (looking-back " " nil)
        (delete-char -1))))

  (evil-define-command w/split-line-backslash ()
    "Split line before the current word, using a continuation line ending."
    (interactive)
    (when (looking-at-p " ")
      (evil-forward-WORD-begin))
    (unless (looking-back " " 1)
      (evil-backward-WORD-begin))
    (fixup-whitespace)
    (evil-forward-WORD-begin)
    (insert "\\")
    (newline-and-indent))

  ;; todo: make "0" work visually in visual line mode. maybe using
  ;; something like this:
  ;; (evil-redirect-digit-argument evil-motion-state-map "0" 'evil-beginning-of-line)

  (evil-define-text-object w/evil-text-object-whole-buffer (count &optional _beg _end _type)
    "Text object for the whole buffer."
    (evil-range (point-min) (point-max) 'line))

  (evil-define-text-object w/evil-empty-text-object (count &optional _beg _end _type)
    "Empty text object."
    (evil-range (point) (point)))

  (evil-define-text-object w/evil-text-object-symbol-dwim (count &optional beg end _type)
    "Intelligently pick evil-inner-symbol or evil-a-symbol."
    (if (memq this-command '(evil-delete lispyville-delete))
        (evil-a-symbol count beg end type)
      (evil-inner-symbol count beg end type))))

(use-package evil-args
  :defer t
  :after evil
  :bind
  (:map evil-inner-text-objects-map
   ("a" . evil-inner-arg)
   :map evil-outer-text-objects-map
   ("a" . evil-outer-arg)))

(use-package evil-colemak-basics
  :demand t
  :after evil evil-snipe
  :delight
  :init
  (setopt evil-colemak-basics-char-jump-commands 'evil-snipe)
  :config
  (global-evil-colemak-basics-mode))

(use-package evil-commentary
  :defer t
  :after evil
  :general
  (:states 'normal
   "gc" #'evil-commentary
   "gy" #'evil-commentary-yank))

(use-package evil-exchange
  :defer t
  :custom
  (evil-exchange-highlight-face 'magit-diff-base)
  :general
  (:states '(normal visual)
   "gx" 'evil-exchange
   "gX" 'evil-exchange-cancel))

(use-package evil-goggles
  :demand t
  :after evil
  :delight
  :commands
  w/shoulder-surf-mode
  :functions
  evil-goggles-use-magit-faces
  :custom
  (evil-goggles-duration 1)
  (evil-goggles-blocking-duration .2)
  (evil-goggles-pulse t)
  :custom-face
  (evil-goggles-default-face ((t (:inherit highlight))))
  :init
  (defvar w/shoulder-surf-mode)
  :config
  ;; see https://github.com/edkolev/evil-goggles/pull/26
  (--each '((lispyville-yank                 :face evil-goggles-yank-face           :switch evil-goggles-enable-yank           :advice evil-goggles--generic-async-advice)
            (lispyville-delete               :face evil-goggles-delete-face         :switch evil-goggles-enable-delete         :advice evil-goggles--generic-blocking-advice)
            (lispyville-change               :face evil-goggles-change-face         :switch evil-goggles-enable-change         :advice evil-goggles--generic-blocking-advice)
            (lispyville-yank-line            :face evil-goggles-yank-face           :switch evil-goggles-enable-yank           :advice evil-goggles--generic-async-advice)
            (lispyville-delete-line          :face evil-goggles-delete-face         :switch evil-goggles-enable-delete         :advice evil-goggles--delete-line-advice)
            (lispyville-change-line          :face evil-goggles-change-face         :switch evil-goggles-enable-change         :advice evil-goggles--generic-blocking-advice)
            (lispyville-change-whole-line    :face evil-goggles-change-face         :switch evil-goggles-enable-change         :advice evil-goggles--generic-blocking-advice)
            (lispyville-join                 :face evil-goggles-join-face           :switch evil-goggles-enable-join           :advice evil-goggles--join-advice)
            (lispyville-comment-or-uncomment :face evil-goggles-nerd-commenter-face :switch evil-goggles-enable-nerd-commenter :advice evil-goggles--generic-async-advice)
            (lispyville-prettify             :face evil-goggles-indent-face         :switch evil-goggles-enable-indent         :advice evil-goggles--generic-async-advice))
    (add-to-list 'evil-goggles--commands it))

  (evil-goggles-mode)
  (evil-goggles-use-magit-faces)

  (define-minor-mode w/shoulder-surf-mode
    "Minor mode to make it easier for others to see what's happening on the screen."
    :global t
    :lighter " 👀"
    (let ((enabled w/shoulder-surf-mode))
      (global-hl-line-mode (if enabled 1 -1))
      (setopt
       evil-goggles-duration (if enabled 5 1)
       evil-goggles-blocking-duration (if enabled 1 .2))
      (set-face-attribute
       'evil-goggles-default-face nil
       :inherit (if enabled 'magit-diff-base 'highlight)))))

(use-package evil-numbers
  :defer t
  :after evil
  :general
  (:states 'normal
   "+" #'evil-numbers/inc-at-pt
   "-" #'evil-numbers/dec-at-pt))

(use-package evil-snipe
  :demand t
  :after evil
  :delight evil-snipe-local-mode
  :general
  (:keymaps 'evil-snipe-local-mode-map
   :states '(normal motion)
   ;; note: ‘evil-colemak-basics’ binds keys
   "s" nil
   "S" nil)
  :custom
  (evil-snipe-override-evil-repeat-keys nil)
  (evil-snipe-repeat-scope 'line)
  (evil-snipe-scope 'line)
  (evil-snipe-smart-case t)
  (evil-snipe-tab-increment t)
  :custom-face
  (evil-snipe-first-match-face ((t (:inherit default))))
  (evil-snipe-matches-face ((t (:inherit lazy-highlight))))
  :config
  (evil-snipe-mode))

(use-package evil-surround
  :defer t
  ;; todo: consider evil-embrace + embrace
  :general
  (:states 'operator
   "s" 'evil-surround-edit
   "S" 'evil-Surround-edit)
  (:states 'visual
   "S" 'evil-surround-region
   "gS" 'evil-Surround-region)
  :autoload
  w/evil-surround-define-surround-trigger-pairs
  :config
  (defun w/evil-surround-define-surround-trigger-pairs (scope &rest args)
    "Define :global or :local surround pairs as TRIGGER LEFT RIGHT (repeatable)."
    (unless (member scope '(:global :local))
      (error "Scope must be either :global or :local") )
    (let ((new-evil-surround-pairs-alist
           (copy-tree
            (pcase scope
              (:local evil-surround-pairs-alist)
              (:global (default-value 'evil-surround-pairs-alist))))))
      (pcase-dolist (`(,trigger ,left ,right) (seq-partition args 3))
        (setf (alist-get (string-to-char trigger) new-evil-surround-pairs-alist)
              (cons left right)))
      (pcase scope
        (:local (setq-local evil-surround-pairs-alist new-evil-surround-pairs-alist))
        (:global (setq-default evil-surround-pairs-alist new-evil-surround-pairs-alist)))))

  (w/evil-surround-define-surround-trigger-pairs
   :global
   ;; overwrite defaults to avoid spaces inside braces
   "(" "(" ")"
   "[" "[" "]"
   "{" "{" "}"
   ;; without shift key
   "0" "(" ")"
   "9" "(" ")"
   ;; typographic quotation marks
   "‘" "‘" "’"
   "’" "‘" "’"
   "q" "‘" "’"
   "“" "“" "”"
   "”" "“" "”"
   "Q" "“" "”"
   ;; ¿question? ¡answer!
   "?" "¿" "?"
   "!" "¡" "!"))

(use-package evil-swap-keys
  :demand t
  :after evil
  :delight " ⌨️"
  :config
  (global-evil-swap-keys-mode))

(use-package flycheck
  :demand t
  :after direnv
  :hook
  (flycheck-mode-hook . w/flycheck-show-error-other-file-mode)
  (flycheck-before-syntax-check-hook . direnv--maybe-update-environment)
  (flycheck-error-list-after-refresh-hook . w/flycheck-hide-error-list-header)
  :general
  (:keymaps 'flycheck-error-list-mode-map
   :states 'motion
   "n" #'flycheck-error-list-next-error
   "e" #'flycheck-error-list-previous-error
   "p" #'flycheck-error-list-previous-error
   "<return>" #'flycheck-error-list-goto-error)
  :commands
  flycheck-buffer
  flycheck-compile
  flycheck-next-error
  flycheck-previous-error
  flycheck-select-checker
  flycheck-verify-setup
  w/flycheck-compile-current
  w/flycheck-show-error-other-file-mode
  w/flycheck-toggle-error-window
  :functions
  flycheck-add-mode
  flycheck-get-checker-for-buffer
  flycheck-list-errors
  :custom
  (flycheck-checker-error-threshold 2000)
  (flycheck-display-errors-delay 1.0)
  (flycheck-idle-change-delay 3)
  (flycheck-mode-line-prefix "🧐")
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-python-flake8-executable "flake8")
  (flycheck-relevant-error-other-file-minimum-level nil)
  :custom-face
  (flycheck-error ((t (:underline nil :inherit error))))

  :config
  (global-flycheck-mode)
  (flycheck-add-mode 'sh-shellcheck 'direnv-envrc-mode)
  (add-to-list
   'display-buffer-alist
   '("\\*Flycheck errors\\*" .
     (display-buffer-in-side-window
      (side . bottom)
      (slot . 0)
      (preserve-size . (nil . t))
      (window-height . w/fit-bottom-error-window-to-buffer)
      (window-parameters . ((no-other-window . t)
                            (no-delete-other-windows . t))))))

  (defun w/flycheck-compile-current ()
    "Run ‘flycheck-compile’ using the current checker."
    (interactive)
    (flycheck-compile (flycheck-get-checker-for-buffer)))

  (defun w/flycheck-last-error ()
    "Jump to the last flycheck error."
    (interactive)
    (goto-char (point-max))
    (flycheck-previous-error))

  (defun w/flycheck-toggle-error-window ()
    "Show or hide the flycheck error list."
    (interactive)
    (if-let* ((buffer (get-buffer flycheck-error-list-buffer))
              (window (get-buffer-window buffer)))
        (progn
          ;; for some reason this sometimes gets stuck when using
          ;; the more specific (quit-windows-on buffer)
          (delete-window window))
      (flycheck-list-errors)))

  (define-minor-mode w/flycheck-show-error-other-file-mode
    "Quickly toggle showing of errors from other files"
    :lighter nil
    (setopt flycheck-relevant-error-other-file-show w/flycheck-show-error-other-file-mode)
    (when flycheck-mode
      (flycheck-buffer)))

  (defun w/flycheck-hide-error-list-header ()
    "Hide the error list header line."
    (when-let ((buffer (get-buffer "*Flycheck errors*")))
      (with-current-buffer buffer
        (setq header-line-format nil)))))

(use-package flycheck-package
  :demand t
  :after flycheck
  :config
  (flycheck-package-setup))

(use-package face-remap
  :defer t
  :if (display-graphic-p)
  :custom
  (global-text-scale-adjust-limits '(60 . 500))
  :general
  (:states 'motion
   "C-0" #'global-text-scale-adjust
   "C--" #'global-text-scale-adjust
   "C-=" #'global-text-scale-adjust
   "C-<wheel-down>" #'mouse-wheel-global-text-scale
   "C-<wheel-up>" #'mouse-wheel-global-text-scale))

(use-package git-link
  :defer t
  :custom
  (git-link-open-in-browser t))

(use-package git-modes
  :defer t
  :mode
  ((rx ".gitconfig" (* any) string-end) . gitconfig-mode)
  ((rx ".config/git/config" (* any) string-end) . gitconfig-mode))

(use-package git-rebase
  :demand t
  :ensure magit
  :after magit
  :general
  (:keymaps 'git-rebase-mode-map
   :states '(normal visual)
   "g" nil
   "n" #'evil-next-line
   "e" #'evil-previous-line
   "l" #'git-rebase-undo)
  (:keymaps 'git-rebase-mode-map
   :states 'normal
   "c" #'git-rebase-edit
   "d" #'git-rebase-kill-line
   "i" #'git-rebase-insert
   "p" #'git-rebase-pick
   "C-e" #'git-rebase-move-line-up
   "C-p" #'git-rebase-move-line-up
   "C-n" #'git-rebase-move-line-down
   "ZQ" #'with-editor-cancel
   "ZZ" #'with-editor-finish)
  :config
  (add-to-list 'global-evil-colemak-basics-modes '(not git-rebase-mode)))

(use-package gsettings
  :demand t
  :functions
  gsettings-get
  gsettings-gnome-running?
  w/gsettings-get-font-family
  :config
  (defun w/gsettings-get-font-family (font-name-with-size)
    (replace-regexp-in-string "\\(.*\\) [0-9.]+" "\\1" font-name-with-size))
  (when (gsettings-gnome-running?)
    (gsettings-apply-gnome-settings)
    (setopt
     w/ui-font-family
     (w/gsettings-get-font-family (gsettings-get "org.gnome.desktop.interface" "font-name"))
     w/document-font-family
     (w/gsettings-get-font-family (gsettings-get "org.gnome.desktop.interface" "document-font-name")))))

(use-package help
  :demand t
  :ensure emacs
  :bind
  (:map help-map  ;; unbind useless shortcuts to gpl, etc.
   ("g" . nil)    ;; describe-gnu-project
   ("C-c" . nil)  ;; describe-copying
   ("C-m" . nil)  ;; view-order-manuals
   ("C-o" . nil)  ;; describe-distribution
   ("C-w" . nil)) ;; describe-no-warranty
  :custom
  (help-window-select t))

(use-package help-mode
  :defer t
  :ensure emacs
  :hook (help-mode-hook . w/help-mode-hook)
  :bind
  (:map help-mode-map
   ("q" . nil))
  :config
  (defun w/help-mode-hook ()
    (setopt evil-lookup-func 'w/helpful-evil-lookup-func)))

(use-package helpful
  :defer t
  :hook (helpful-mode-hook . w/helpful-mode-hook)
  :bind
  (("<remap> <describe-command>" . helpful-command)
   ("<remap> <describe-function>" . helpful-callable)
   ("<remap> <describe-key>" . helpful-key)
   ("<remap> <describe-symbol>" . helpful-symbol)
   ("<remap> <describe-variable>" . helpful-variable))
  :general
  (:keymaps 'helpful-mode-map
   :states 'normal
   "gr" #'helpful-update)
  :commands
  w/helpful-evil-lookup-func
  :config
  (defun w/helpful-mode-hook ()
    (setopt
     evil-lookup-func 'w/helpful-evil-lookup-func
     evil-shift-width 2))
  (defun w/helpful-evil-lookup-func ()
    (call-interactively #'helpful-symbol)))

(use-package indent-bars
  :defer t)

(use-package isearch
  :demand t
  :ensure emacs
  :bind
  (:map isearch-mode-map
   ("C-o" . w/isearch-occur-and-exit)
   ("C-'" . avy-isearch)
   ("C-/" . w/consult-line-from-isearch))
  :general
  (:states 'motion
   "*" #'isearch-forward-thing-at-point
   "#" #'isearch-forward-thing-at-point)
  :commands
  w/isearch-occur-and-exit
  :custom
  (isearch-allow-prefix nil)
  (isearch-lazy-count t)
  (lazy-highlight-cleanup nil)
  (lazy-highlight-initial-delay 0.5)
  (lazy-highlight-max-at-a-time nil)
  (search-default-mode t)
  :config
  (defun w/isearch-occur-and-exit ()
    "Like ‘isearch-occur’ but also exits ‘isearch’."
    (interactive)
    (call-interactively #'isearch-occur)
    (isearch-exit)))

(use-package jinja2-mode
  :defer t)

(use-package jq-format
  :demand t
  :after json-mode
  :delight
  (jq-format-json-on-save-mode " ✒️")
  (jq-format-jsonlines-on-save-mode " ✒️"))

(use-package key-chord
  :demand t
  :config
  (key-chord-mode))

(use-package ligature
  :demand t
  :config
  ;; see https://github.com/mickeynp/ligature.el/wiki
  (ligature-set-ligatures 't '("www"))  ;; Cascadia / Fira Code
  (ligature-set-ligatures
   '(markdown-mode rst-mode)
   '(("=" (rx (+ "=") (? (| ">" "<"))))
     ("-" (rx (+ "-")))
     ("#" (rx (+ "#")))
     ("~" (rx (+ "~")))
     ("+" (rx (+ "+")))))
  (let* ((ligatures-font-cascadia-fira-code
          '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
            ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
            "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
            "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
            "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
            "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
            "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
            "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
            ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
            "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
            "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
            "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
            "\\\\" "://"))
         (ligatures-font-iosevka
          '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
            "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
            "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
            ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
         (ligatures-all-fonts
          (-uniq (append ligatures-font-cascadia-fira-code ligatures-font-iosevka))))
    (ligature-set-ligatures 'prog-mode ligatures-all-fonts))
  (global-ligature-mode))

(use-package lisp-indent-patch
  :demand t
  :load-path "lisp/"
  :after el-patch)

(use-package lsp-mode
  :defer t
  :delight " 🛠️"
  :hook
  (lsp-after-open-hook . w/lsp-mode-after-open-hook)
  (lsp-completion-mode-hook . w/lsp-completion-mode-hook)
  :commands
  lsp-describe-thing-at-point
  lsp-execute-code-action
  lsp-find-definition
  lsp-find-implementation
  lsp-find-type-definition
  lsp-rename
  lsp-ui-doc-show
  lsp-workspace-restart
  :custom
  (lsp-completion-provider :none) ;; use corfu
  (lsp-auto-execute-action nil)
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-headerline-breadcrumb-segments '(symbols))
  (lsp-keymap-prefix "C-c l")
  :config
  (defun w/lsp-mode-after-open-hook ()
    (lsp-origami-try-enable))
  (defun w/lsp-completion-mode-hook ()
    ;; see https://github.com/minad/corfu/wiki
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))))

(use-package lsp-origami
  :demand t
  :after lsp-mode)

(use-package lsp-ui
  :demand t
  :after lsp-mode)

(use-package marginalia
  :demand t
  :config
  (marginalia-mode))

(use-package minibuffer
  :demand t
  :ensure emacs
  :custom
  (enable-recursive-minibuffers t)
  (minibuffer-follows-selected-frame nil)
  :bind
  (:map minibuffer-local-map
   ("C-w" . backward-kill-word)
   ("C-u" . kill-whole-line)
   ("<escape>" . minibuffer-keyboard-quit)))

(use-package modus-themes
  :demand t
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t))

(use-package nxml-mode
  :defer t
  :ensure emacs
  :hook (nxml-mode-hook . w/nxml-mode-hook)
  :config
  (defun w/nxml-mode-hook ()
    (modify-syntax-entry ?< ".")
    (modify-syntax-entry ?> ".")
    (modify-syntax-entry ?/ ".")
    (modify-syntax-entry ?: "_")
    (reformatter-dwim-select 'xml-format)))

(use-package nyan-mode
  :defer t)

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)) ;; emacs 31: partial-completion behaves like substring

(use-package outline
  :defer t
  :delight
  (outline-minor-mode " ‣"))

(use-package pkgbuild-mode
  :defer t
  :custom
  (pkgbuild-update-sums-on-save nil)
  :custom-face
  (pkgbuild-error-face ((t (:inherit error)))))

(use-package profiler
  :defer t
  :hook (profiler-report-mode-hook . w/profiler-report-mode-hook)
  :general
  (:keymaps 'profiler-report-mode-map
   :states 'motion
   "<tab>" #'profiler-report-toggle-entry
   "<return>" #'profiler-report-toggle-entry
   "C-e" #'profiler-report-previous-entry
   "C-n" #'profiler-report-next-entry
   "C-p" #'profiler-report-previous-entry
   "r" #'profiler-report-render-calltree
   "R" #'profiler-report-render-reversed-calltree)
  :config
  (defun w/profiler-report-mode-hook ()
    (setopt evil-lookup-func 'w/helpful-evil-lookup-func))
  (evil-set-initial-state 'profiler-report-mode 'motion))

(use-package python-black
  :demand t
  :after python
  :delight
  (python-black-on-save-mode " 🖤"))

(use-package python-coverage
  :demand t
  :after python
  :delight
  (python-coverage-overlay-mode " 🚨"))

(use-package ranger
  :demand t
  :after dired evil
  :bind
  (:map ranger-mode-map
   ("h" . ranger-up-directory)
   ("n" . ranger-next-file)
   ("e" . ranger-prev-file)
   ("i" . w/ranger-find-directory)
   ("k" . ranger-search-next)
   ("K" . ranger-search-previous)
   ("'" . nil)
   ("/" . ranger-search))
  :commands
  deer-jump-other-window
  ranger-find-file
  :custom
  (ranger-cleanup-eagerly t)
  (ranger-deer-show-details nil)
  (ranger-excluded-extensions nil)
  (ranger-max-tabs 1)
  (ranger-override-dired 'deer)
  (ranger-show-hidden t)

  :config
  (add-to-list 'global-evil-colemak-basics-modes '(not ranger-mode))
  (with-eval-after-load 'direnv
    (add-to-list 'direnv-non-file-modes 'ranger-mode))

  ;; fixme: is using auxiliary keymap correct?
  (evil-set-auxiliary-keymap ranger-mode-map 'motion ranger-mode-map)

  (defun w/ranger-find-directory ()
    "Like ‘ranger-find-file’, but only for directories."
    (interactive)
    (when-let* ((name (dired-get-filename nil t))
                ((file-directory-p name)))
      (ranger-find-file name))))

(use-package recentf
  :demand t
  :custom
  (recentf-auto-cleanup 300)
  (recentf-max-saved-items 500)
  :commands
  w/counsel-recentf-other-window
  :functions
  recentf-expand-file-name
  :config
  (--each (list
           (recentf-expand-file-name no-littering-etc-directory)
           (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude it t))

  (recentf-mode)

  (defun w/counsel-recentf-other-window ()
    "Like ‘counsel-recentf’, but opens the file in another window."
    (interactive)
    (require 'counsel)
    (require 'ivy)
    (defvar ivy-inhibit-action)
    (let ((ivy-inhibit-action t))
      (find-file-other-window (counsel-recentf)))))

(use-package reformatter
  :defer t)

(use-package reformatter-dwim
  :defer t
  :load-path "lisp/"
  :autoload
  reformatter-dwim-select
  :general
  (:states '(normal visual)
   "g =" 'reformatter-dwim-evil)
  (:states 'normal
   "g +" 'reformatter-dwim-on-save-mode))

(use-package savehist
  :demand t
  :custom
  (savehist-autosave-interval 60)
  :config
  (--each '(kill-ring
            kmacro-ring
            last-kbd-macro
            regexp-search-ring
            search-ring
            shell-command-history)
    (add-to-list 'savehist-additional-variables it))
  (savehist-mode))

(use-package server
  :demand t
  :functions
  server-running-p
  :if (display-graphic-p)
  :config
  (unless (server-running-p)
    (server-start)))

(use-package sh-script
  :defer t
  :mode
  ((rx "bashrc" string-end) . sh-mode)
  ((rx ".bashrc-" (* any) string-end) . sh-mode)
  ((rx (* any) ".env" string-end) . sh-mode)
  :general
  (:keymaps 'sh-mode-map
   :states 'normal
   "<return> "'w/split-line-backslash
   "<remap> <evil-join>" #'w/evil-join-smart-backslash-eol)
  :custom
  (sh-indent-after-continuation 'always)
  :config
  (reformatter-define w/shell-format-bash-pretty-print
    :program "bash"
    :args '("--pretty-print" "-")
    :group 'sh-script))

(use-package shfmt
  :defer t)

(use-package smerge-mode
  :defer t
  :delight " 🔀"
  :commands
  smerge-keep-all
  smerge-keep-base
  smerge-keep-current
  smerge-keep-lower
  smerge-keep-upper
  smerge-next
  smerge-prev)

(use-package solarized-theme
  :demand t
  :if (display-graphic-p)
  :custom
  (solarized-emphasize-indicators nil)
  (solarized-scale-org-headlines nil)
  (solarized-use-less-bold t)
  (solarized-use-variable-pitch nil)
  (solarized-height-minus-1 1.0)
  (solarized-height-plus-1 1.0)
  (solarized-height-plus-2 1.0)
  (solarized-height-plus-3 1.0)
  (solarized-height-plus-4 1.0)
  :custom-face
  (vertical-border ((t (:foreground unspecified :inherit font-lock-comment-face))))
  :preface
  ;; todo: avoid these if possible. ones are used for useful things
  ;; and look up the values in ‘solarized-dark-color-palette-alist’
  ;; and ‘solarized-light-color-palette-alist’
  (defvar solarized-color-blue      "#268bd2")
  (defvar solarized-color-blue-l    "#69b7f0")
  (defvar solarized-color-cyan-l    "#69cabf")
  (defvar solarized-color-green-l   "#b4c342")
  (defvar solarized-color-magenta   "#d33682")
  (defvar solarized-color-magenta-l "#f771ac")
  (defvar solarized-color-orange-l  "#f2804f")
  (defvar solarized-color-red       "#dc322f")
  (defvar solarized-color-red-l     "#ff6e64")
  (defvar solarized-color-violet-l  "#9ea0e5")
  (defvar solarized-color-yellow    "#b58900")
  (defvar solarized-color-yellow-l  "#deb542"))

(use-package sphinx-mode
  :demand t
  :after rst
  :delight
  :config
  (setopt sphinx-mode-map (make-sparse-keymap)))

(use-package sql
  :defer t
  :mode
  ((rx (? ".") "psqlrc" string-end) . sql-mode)
  :hook (sql-mode-hook . w/sql-mode-hook)
  :commands
  w/sql-tweak-syntax-table
  :config
  (require 'reformatter-dwim)
  (require 'sqlformat)

  (defun w/sql-mode-hook ()
    (setopt evil-shift-width 2)
    (w/sql-tweak-syntax-table)
    (add-hook 'hack-local-variables-hook #'w/sql-tweak-syntax-table t t)
    (reformatter-dwim-select 'sqlformat))

  (defun w/sql-tweak-syntax-table ()
    (modify-syntax-entry ?_ "w")))

(use-package sqlformat
  :defer t
  :after sql
  :delight ('sqlformat-on-save-mode " 💄")
  :custom
  (sqlformat-command 'sqlfluff))

(use-package sudo-edit
  :defer t)

(use-package systemd
  :defer t)

(use-package terminal-here
  :defer t
  :custom
  terminal-here-linux-terminal-command 'gnome-terminal
  :config
  (when (and (eq system-type 'gnu/linux) (executable-find "ghostty"))
    (setopt terminal-here-linux-terminal-command
            (lambda (directory)
              `("ghostty" ,(format "--working-directory=%s" directory) "+new-window")))))

(use-package text-mode
  :defer t
  :ensure emacs
  :hook (text-mode-hook . w/text-mode-hook)
  :config
  (defun w/text-mode-hook ()
    (w/show-trailing-whitespace-mode)
    (guess-language-mode)))

(use-package transient
  :defer t
  :custom
  (transient-show-menu 1)
  :bind
  (:map transient-map
   ("C-n" . transient-history-next)
   ("C-p" . transient-history-prev))
  :config
  (add-to-list 'w/read-extended-command-predicate-functions 'transient-command-completion-not-suffix-only-p))

(use-package typo
  :defer t
  :delight " ”"
  :commands w/typo-cycle-quotation-marks
  :custom
  (typo-language "prefer-single")
  :config
  (add-to-list 'typo-quotation-marks '("prefer-single" "‘" "’" "“" "”"))
  (define-typo-cycle w/typo-cycle-quotation-marks
    "Cycle through various quotation marks."
    ("'" "‘" "’" "“" "”" "\"")))

(use-package vc
  :demand t
  :custom
  (vc-handled-backends '(git)))

(use-package vertico
  :demand t
  :custom
  (vertico-count 20)
  (vertico-cycle t)
  (vertico-multiform-categories
   '((consult-grep buffer)
     (consult-location buffer)
     (file (vertico-sort-function . vertico-sort-directories-first)
           (:keymap . vertico-directory-map))
     (imenu buffer)
     (symbol (vertico-sort-function . vertico-sort-alpha))))
  :functions
  vertico--candidate
  vertico--metadata-get
  :config
  (vertico-mode)
  (vertico-mouse-mode)
  (vertico-multiform-mode))

(use-package vertico-directory
  :demand t
  :ensure vertico
  :after vertico
  :bind
  (:map vertico-directory-map
   ("~" . #'w/vertico-directory-maybe-home)
   ("`" . #'w/vertico-directory-maybe-home)
   ("/" . #'w/vertico-directory-slash))
  :commands
  w/vertico-directory-maybe-home
  w/vertico-directory-slash
  :config
  (defun w/vertico-directory-maybe-home ()
    "Go to home dir when behind a slash, e.g. ‘~’ directly after ‘M-x find-file’."
    (interactive)
    (cond
     ((and (eql vertico--index -1)
           (looking-back "/" 1))
      (delete-minibuffer-contents)
      (insert "~/"))
     (t (self-insert-command 1))))
  (defun w/vertico-directory-slash ()
    "Descend into selected directory, quickly go to /, or insert slash."
    (interactive)
    (let* ((file-category (eq 'file (vertico--metadata-get 'category)))
           (selected (>= vertico--index 0))
           (candidate-directory (string-suffix-p "/" (vertico--candidate)))
           (after-slash (looking-back "/" 1)))
      (cond
       ((and file-category selected candidate-directory)
        (vertico-directory-enter))
       ((and file-category (not selected) after-slash)
        (delete-minibuffer-contents)
        (insert "/"))
       (t
        (self-insert-command 1))))))

(use-package which-func
  :demand t
  :custom
  (which-func-unknown "")
  (which-func-modes nil)
  :custom-face
  (which-func ((t (:foreground unspecified))))
  :config
  (setopt which-func-format (--remove (member it '("[" "]")) which-func-format))
  (which-function-mode))

(use-package which-key
  :demand t
  :delight
  :custom
  (which-key-add-column-padding 4)
  (which-key-dont-use-unicode nil)
  (which-key-idle-secondary-delay 0.0)
  (which-key-max-description-length 64)
  (which-key-show-early-on-C-h t)
  :config
  (which-key-mode))

(use-package whitespace-cleanup-mode
  :demand t
  :delight
  '(:eval (unless whitespace-cleanup-mode-initially-clean " ⎵"))
  :config
  (global-whitespace-cleanup-mode))

(use-package winner
  :demand t
  :custom
  (winner-dont-bind-my-keys t)
  :config
  (winner-mode))

(use-package xml-format
  :demand t
  :after nxml-mode)

(use-package yasnippet
  :demand t
  :delight (yas-minor-mode " 🐒")
  :init
  (setopt yas-alias-to-yas/prefix-p nil)
  :config
  (yas-global-mode))

(defvar-keymap w/buffer-map
  :doc "Keymap for buffer commands."
  "B" #'consult-buffer-other-window
  "C" #'clone-indirect-buffer-other-window
  "E" #'rename-buffer
  "H" #'unbury-buffer
  "K" #'kill-buffer-and-window
  "N" #'w/evil-buffer-new-other-window
  "b" #'consult-buffer
  "c" #'clone-indirect-buffer
  "e" #'crux-rename-file-and-buffer
  "h" #'bury-buffer
  "k" #'kill-current-buffer
  "m" #'w/switch-major-mode
  "n" #'evil-buffer-new
  "r" #'revert-buffer)

(defvar-keymap w/diff-map
  :doc "Keymap for vdiff commands."
  "C" #'vdiff-close-all-folds
  "E" #'vdiff-previous-fold
  "F" #'vdiff-refine-all-hunks
  "N" #'vdiff-next-fold
  "O" #'vdiff-open-all-folds
  "P" #'vdiff-previous-fold
  "R" #'vdiff-receive-changes-and-step
  "S" #'vdiff-send-changes-and-step
  "c" #'vdiff-close-fold
  "d" #'vdiff-hydra/body
  "e" #'vdiff-previous-hunk
  "f" #'vdiff-refine-this-hunk
  "n" #'vdiff-next-hunk
  "o" #'vdiff-open-fold
  "p" #'vdiff-previous-hunk
  "r" #'vdiff-receive-changes
  "s" #'vdiff-send-changes
  "u" #'vdiff-refresh
  "x" #'vdiff-remove-refinements-in-hunk)

(defvar-keymap w/file-map
  :doc "Keymap for file commands."
  "!" #'terminal-here
  "1" #'terminal-here
  "D" #'deer-jump-other-window
  "F" #'find-file-other-window
  "N" #'w/evil-buffer-new-other-window
  "R" #'w/counsel-recentf-other-window
  "S" #'sudo-edit-find-file
  "d" #'deer
  "f" #'find-file
  "g" #'w/open-gui-file-browser
  "i" #'insert-file
  "n" #'evil-buffer-new
  "r" #'consult-recent-file
  "s" #'sudo-edit)

(defvar-keymap w/flycheck-map
  :doc "Keymap for Flycheck commands."
  "C" #'flycheck-mode
  "M" #'flycheck-compile
  "b" #'flycheck-buffer
  "c" #'w/flycheck-toggle-error-window
  "e" #'flycheck-previous-error
  "m" #'w/flycheck-compile-current
  "n" #'flycheck-next-error
  "o" #'w/flycheck-show-error-other-file-mode
  "p" #'flycheck-previous-error
  "s" #'flycheck-select-checker
  "v" #'flycheck-verify-setup)

(defvar-keymap w/git-map
  :doc "Keymap for Git commands."
  "!" #'magit-git-command
  "A" #'w/magit-log-buffer-file-follow
  "S" #'w/magit-status-other-repository
  "a" #'magit-blame-addition
  "c" #'magit-commit-create
  "d" #'magit-diff-dwim
  "f" #'magit-file-dispatch
  "g" #'magit-dispatch
  "l" #'magit-log-current
  "s" #'magit-status
  "t" #'magit-toggle-buffer-lock
  "w" #'w/git-web-browse)

(defvar-keymap w/lsp-map
  :doc "Keymap for LSP commands."
  "/" #'lsp-ui-doc-show
  "?" #'lsp-ui-doc-show
  "a" #'lsp-execute-code-action
  "d" #'lsp-describe-thing-at-point
  "g" #'lsp-find-definition
  "i" #'lsp-find-implementation
  "r" #'lsp-rename
  "t" #'lsp-find-type-definition
  "x" #'lsp-workspace-restart)

(defvar-keymap w/merge-map
  :doc "Keymap for merge conflict commands."
  "a" #'smerge-keep-all
  "b" #'smerge-keep-base
  "c" #'smerge-keep-current
  "e" #'smerge-prev
  "l" #'smerge-keep-lower
  "m" #'smerge-mode
  "n" #'smerge-next
  "p" #'smerge-prev
  "u" #'smerge-keep-upper)

(defvar-keymap w/project-map
  :doc "Keymap for project commands."
  "!" #'terminal-here-project-launch
  "-" #'projectile-dired
  "/" #'w/counsel-ag-project
  "1" #'terminal-here-project-launch
  "?" #'w/counsel-ag-project-all-files
  "B" #'projectile-switch-to-buffer-other-window
  "D" #'projectile-find-dir-other-window
  "F" #'projectile-find-file-other-window
  "P" #'projectile-switch-open-project
  "R" #'projectile-replace-regexp
  "T" #'projectile-find-implementation-or-test-other-window
  "a" #'w/projectile-find-file-all
  "b" #'projectile-switch-to-buffer
  "c" #'projectile-compile-project
  "d" #'projectile-find-dir
  "f" #'projectile-find-file
  "g" #'w/projectile-open-gui-file-browser
  "k" #'projectile-kill-buffers
  "o" #'projectile-multi-occur
  "p" #'projectile-switch-project
  "q" #'w/projectile-project-bury-buffers
  "r" #'projectile-replace
  "s" #'projectile-save-project-buffers
  "t" #'projectile-toggle-between-implementation-and-test)

(defvar-keymap w/replace-map
  :doc "Keymap for replacement commands."
  "P" #'projectile-replace-regexp
  "Q" #'query-replace-regexp
  "p" #'projectile-replace
  "q" #'query-replace
  "r" #'w/query-replace-thing-at-point-dwim
  "s" #'w/query-replace-thing-at-point-dwim)

(defvar-keymap w/search-map
  :doc "Keymap for search commands."
  "F" #'ag-files
  "G" #'ag
  "R" #'ag-regexp
  "a" #'ag-project
  "f" #'ag-project-files
  "g" #'ag-project
  "r" #'ag-project-regexp)

(defvar-keymap w/toggle-map
  :doc "Keymap for toggle commands."
  "!" #'global-evil-swap-keys-mode
  "(" #'electric-pair-local-mode
  ")" #'electric-pair-local-mode
  "0" #'electric-pair-local-mode
  "1" #'global-evil-swap-keys-mode
  "9" #'electric-pair-local-mode
  "F" #'display-fill-column-indicator-mode
  "L" #'global-hl-line-mode
  "N" #'w/display-line-numbers-cycle
  "S-SPC" #'w/show-trailing-whitespace-mode
  "SPC" #'whitespace-mode
  "TAB" #'indent-bars-mode
  "W" #'w/sensible-wrap-mode-2
  "b" #'auto-dark-toggle-appearance
  "c" #'flycheck-mode
  "d" #'diff-hl-mode
  "f" #'auto-fill-mode
  "h" #'symbol-overlay-mode
  "l" #'hl-line-mode
  "n" #'display-line-numbers-mode
  "s" #'flyspell-mode
  "w" #'w/sensible-wrap-mode-1
  "z" #'w/origami-mode-toggle)

(defvar-keymap w/window-map
  :doc "Keymap for window commands."
  "+" #'evil-window-increase-width
  "-" #'evil-window-decrease-width
  "1" #'w/goto-window-1
  "2" #'w/goto-window-2
  "3" #'w/goto-window-3
  "4" #'w/goto-window-4
  "5" #'w/goto-window-5
  "6" #'w/goto-window-6
  "=" #'balance-windows
  "C-e" #'evil-window-increase-height
  "C-h" #'evil-window-decrease-width
  "C-i" #'evil-window-increase-width
  "C-n" #'evil-window-decrease-height
  "C-w" #'w/evil-window-next-or-vsplit
  "E" #'evil-window-up
  "F" #'w/make-frame-new-buffer
  "H" #'evil-window-left
  "I" #'evil-window-right
  "N" #'evil-window-down
  "R" #'evil-window-rotate-upwards
  "S" #'evil-window-new
  "U" #'winner-redo
  "V" #'evil-window-vnew
  "b" #'balance-windows
  "c" #'evil-window-delete
  "e" #'buf-move-up
  "f" #'make-frame
  "h" #'buf-move-left
  "i" #'buf-move-right
  "n" #'buf-move-down
  "o" #'delete-other-windows
  "p" #'toggle-window-dedicated
  "r" #'evil-window-rotate-downwards
  "s" #'evil-window-split
  "u" #'winner-undo
  "v" #'evil-window-vsplit
  "w" #'w/evil-window-next-or-vsplit)

(dolist (command '(evil-window-rotate-downwards
                   evil-window-rotate-upwards
                   evil-window-increase-width
                   evil-window-decrease-width
                   evil-window-decrease-height
                   evil-window-increase-height))
  (put command 'repeat-map 'w/window-map))

(defvar-keymap w/leader-map
  :doc "Leader keymap."
  "'" #'w/major-mode-hydra
  "/" #'w/hydra-search/body
  "1" #'w/goto-window-1
  "2" #'w/goto-window-2
  "3" #'w/goto-window-3
  "4" #'w/goto-window-4
  "5" #'w/goto-window-5
  "6" #'w/goto-window-6
  "H" #'symbol-overlay-remove-all
  "J" #'consult-imenu-multi
  "Q" #'unbury-buffer
  "S" #'save-some-buffers
  "X" #'execute-extended-command-for-buffer
  "SPC" #'whitespace-cleanup
  "a" w/search-map
  "b" w/buffer-map
  "c" w/flycheck-map
  "d" w/diff-map
  "f" w/file-map
  "g" w/git-map
  "h" #'w/symbol-overlay-put-dwim
  "j" #'consult-imenu
  "l" w/lsp-map
  "m" w/merge-map
  "n" #'w/narrow-dwim
  "o" #'w/occur-dwim
  "p" w/project-map
  "q" #'bury-buffer
  "r" w/replace-map
  "s" #'save-buffer
  "t" w/toggle-map
  "u" #'universal-argument
  "w" w/window-map
  "x" #'execute-extended-command
  "y" #'w/evil-copy-as-format)

(general-define-key
 :states 'motion
 "'" w/leader-map
 "," w/leader-map)

;;; todo: tidy up the messy stuff below ======================

(use-package hydra
  :after ivy
  :demand t
  :preface
  (defvar w/hydra-hint-delay 1
    "Delay before showing help.")

  ;; fixme: maybe use a registry pattern with an alist that maps major
  ;; modes (and derived modes) to a hydra, instead of buffer-local variables?
  (defvar w/major-mode-hydra nil
    "Hydra body for the current major mode.")
  (make-variable-buffer-local 'w/major-mode-hydra)

  (defun w/set-major-mode-hydra (hydra-body)
    "Set the buffer-local major-mode specific hydra to HYDRA-BODY."
    (setq w/major-mode-hydra hydra-body))

  (defun w/major-mode-hydra ()
    "Run major mode hydra, if any."
    (interactive)
    (if w/major-mode-hydra
        (call-interactively w/major-mode-hydra)
      (user-error "No major-mode specific hydra defined for %s" major-mode)))

  (defun w/hydra-evil-repeat-record-command ()
    "Record last command from the hydra in evil's repeat system."
    (evil-repeat-start)
    (setopt evil-repeat-info `((call-interactively ,real-this-command)))
    (evil-repeat-stop))

  (defun w/hydra-make-docstring (args)
    "Make a docstring for a hydra from ARGS."
    (setq args (--map-when (not (string-match-p "_" it))
                           (format "  %s:" it)
                           args))
    (concat "\n" (string-trim (string-join args "  "))))

  (defun w/hydra-set-defaults (body)
    "Add defaults to a hydra BODY list."
    (unless (plist-member body :exit)
      (setq body (plist-put body :exit t)))
    (unless (plist-member body :hint)
      (setq body (plist-put body :hint nil)))
    (unless (plist-member body :foreign-keys)
      (setq body (plist-put body :foreign-keys 'warn)))
    (setq body (plist-put body :idle w/hydra-hint-delay))
    body)

  (defun w/hydra-missing-uppercase-heads (heads)
    "Return missing uppercase hydra heads.

This creates uppercase versions for all lowercase HEADS that are only
defined as lowercase."
    (let* ((case-fold-search nil)
           (uppercase-keys
            (--filter (string-match-p "^[A-Z]$" it) (-map #'car heads))))
      (--map
       (-replace-at 0 (upcase (car it)) it)
       (--filter
        (and (string-match-p "^[a-z]$" (car it))
             (not (-contains-p uppercase-keys (upcase (car it)))))
        heads))))

  (defmacro w/make-hydra (name body &rest args)
    "Make a hydra NAME with BODY, using ARGS for heads and docstrings."
    (declare (indent 2))
    (-let [(docstrings heads) (-separate #'stringp args)]
      `(defhydra
         ,name
         ,(w/hydra-set-defaults body)
         ,(w/hydra-make-docstring docstrings)
         ,@(w/hydra-missing-uppercase-heads heads)
         ,@heads
         ("C-g" nil :exit t)
         ("<escape>" nil :exit t)))))

(defun w/buffer-worth-saving-p (name)
  "Does the buffer NAME indicate it may be worth saving?"
  (cond
   ((string-equal "*scratch*" name) t)
   ((string-prefix-p "*new*" name) t)  ;; evil-mode template
   ((string-match-p "\*" name) nil) ;; e.g. magit, help
   ((string-match-p "^ " name) nil) ;; hidden buffers
   (t t)))

(defun w/ask-confirmation-for-unsaved-buffers ()
  "Ask for confirmation for modified but unsaved buffers."
  (if (and (buffer-modified-p)
           (not (buffer-file-name))
           (not (member major-mode '(dired-mode ranger-mode)))
           (w/buffer-worth-saving-p (buffer-name)))
      (yes-or-no-p
       (format
        "Buffer %s modified but not saved; kill anyway? "
        (buffer-name)))
    t))

(add-hook
 'kill-buffer-query-functions
 #'w/ask-confirmation-for-unsaved-buffers)

(defun w/evil-buffer-new-other-window ()
  "Open a new window in another window."
  (interactive)
  (w/evil-window-next-or-vsplit)
  (call-interactively #'evil-buffer-new))

(defun w/open-gui-file-browser ()
  "Open a GUI browser for the directory containing the current file."
  (interactive)
  (when-let ((file-name (buffer-file-name))
             (directory-name (file-name-directory file-name))
             (file-exists (file-exists-p directory-name)))
    (call-process "xdg-open" nil 0 nil directory-name)))

(defvar w/faces-bold '(magit-popup-argument)
  "Faces that may retain their bold appearance.")

(defun w/tweak-faces (theme)
  "Tweak some font faces."
  (set-face-attribute
   'fixed-pitch nil
   :family (face-attribute 'default :family))
  (dolist (face (face-list))
    (unless (member face w/faces-bold)
      (set-face-attribute face nil :weight 'normal))))

(add-hook 'enable-theme-functions #'w/tweak-faces)

(use-package edit-indirect
  :hook
  (edit-indirect-after-creation-hook . w/edit-indirect-dedent)
  (edit-indirect-before-commit-hook . w/edit-indirect-reindent)

  :general
  (:states 'normal
   "gb" #'w/evil-edit-indirect)
  (:keymaps 'edit-indirect-mode-map
   :states 'normal
   [remap evil-save-modified-and-close] #'edit-indirect-commit
   [remap evil-quit] #'edit-indirect-abort)

  :preface
  (defvar w/edit-indirect-original-indentation 0
    "Original indentation of the edited region.")
  (make-variable-buffer-local 'w/edit-indirect-original-indentation)

  :config
  (defun w/edit-indirect-dedent ()
    (require 'rst)
    (let ((indentation (rst-find-leftmost-column (point-min) (point-max))))
      (setq w/edit-indirect-original-indentation indentation)
      (when (> indentation 0)
        (indent-rigidly (point-min) (point-max) (- indentation)))))

  (defun w/edit-indirect-reindent ()
    (when (> w/edit-indirect-original-indentation 0)
      (indent-rigidly (point-min) (point-max) w/edit-indirect-original-indentation)))

  (evil-define-operator w/evil-edit-indirect (beg end _type)
    (interactive "<R>")
    (edit-indirect-region beg end t)))

(use-package evil-easymotion
  :defer t
  :general
  (:states 'motion
   "SPC" #'w/hydra-teleport/body)
  :commands
  evilem-make-motion
  evilem-make-motion-plain
  evilem-create
  evilem-create-plain
  evilem-define

  :config
  (w/make-hydra w/hydra-teleport nil
    "teleport"
    "_w_,_f_,_b_,_gf_ word"
    ("w" evilem-motion-forward-word-begin)
    ("W" evilem-motion-forward-WORD-begin)
    ("f" evilem-motion-forward-word-end)
    ("F" evilem-motion-forward-WORD-end)
    ("b" evilem-motion-backward-word-begin)
    ("B" evilem-motion-backward-WORD-begin)
    ("gf" evilem-motion-backward-word-end)
    ("gF" evilem-motion-backward-WORD-end)
    "_n_,_e_,_l_ line"
    ("e" evilem-motion-previous-line)
    ("E" evil-avy-goto-line-above)
    ("n" evilem-motion-next-line)
    ("N" evil-avy-goto-line-below)
    ("l" evil-avy-goto-line)
    ("L" w/avy-goto-line-any-window)
    "_t_,_j_,_SPC_,_/_ char"
    ("SPC" evil-avy-goto-char-timer)
    ("S-SPC" (evil-avy-goto-char-timer t))
    ("t" evilem-motion-find-char)
    ("T" evilem-motion-find-char-to)
    ("j" evilem-motion-find-char-backward)
    ("J" evilem-motion-find-char-to-backward)
    "_k_ search"
    ("k" evilem-motion-search-next)
    ("K" evilem-motion-search-previous)
    "_h_ symbol"
    ("h" w/easymotion-symbol-overlay)
    ("/" evil-avy-goto-char-timer)
    "_o_ new line"
    ("o" (progn (evil-avy-goto-line) (call-interactively 'evil-open-below)))
    ("O" (progn (evil-avy-goto-line) (call-interactively 'evil-open-above)))
    "_d_ delete"
    ("d" w/avy-evil-delete-line)
    ("D" w/avy-evil-delete-lines)
    "_pd_ move"
    ("pd" (save-excursion (forward-line) (call-interactively 'avy-move-line)))
    ("pD" (save-excursion (forward-line) (call-interactively 'avy-move-region)))
    ("Pd" (save-excursion (call-interactively 'avy-move-line)))
    ("PD" (save-excursion (call-interactively 'avy-move-region)))
    "_py_ copy"
    ("py" (save-excursion (forward-line) (call-interactively 'avy-copy-line)))
    ("pY" (save-excursion (forward-line) (call-interactively 'avy-copy-region)))
    ("Py" (save-excursion (call-interactively 'avy-copy-line)))
    ("PY" (save-excursion (call-interactively 'avy-copy-region))))

  ;; declare generated heads that just jump around as evil motions, so
  ;; that they can be used for jumping around in visual state.
  (mapc
   'evil-declare-motion
   '(w/hydra-teleport/evilem-motion-forward-word-begin-and-exit
     w/hydra-teleport/evilem-motion-forward-WORD-begin-and-exit
     w/hydra-teleport/evilem-motion-forward-word-end-and-exit
     w/hydra-teleport/evilem-motion-forward-WORD-end-and-exit
     w/hydra-teleport/evilem-motion-backward-word-begin-and-exit
     w/hydra-teleport/evilem-motion-backward-WORD-begin-and-exit
     w/hydra-teleport/evilem-motion-backward-word-end-and-exit
     w/hydra-teleport/evilem-motion-backward-WORD-end-and-exit
     w/hydra-teleport/evilem-motion-previous-line-and-exit
     w/hydra-teleport/evil-avy-goto-line-above-and-exit
     w/hydra-teleport/evilem-motion-next-line-and-exit
     w/hydra-teleport/evil-avy-goto-line-below-and-exit
     w/hydra-teleport/evil-avy-goto-line-and-exit
     w/hydra-teleport/evil-avy-goto-char-timer-and-exit
     w/hydra-teleport/w/easymotion-symbol-overlay-and-exit
     w/hydra-teleport/evilem-motion-find-char-and-exit
     w/hydra-teleport/evilem-motion-find-char-to-and-exit
     w/hydra-teleport/evilem-motion-find-char-backward-and-exit
     w/hydra-teleport/evilem-motion-find-char-to-backward-and-exit
     w/hydra-teleport/evilem-motion-search-next-and-exit
     w/hydra-teleport/evilem-motion-search-previous-and-exit
     w/hydra-teleport/evilem-motion-search-next-and-exit
     w/hydra-teleport/evilem-motion-search-previous-and-exit))

  ;; make the basic motions also work in evil operator state
  (defvar w/teleport-map (make-sparse-keymap)
    "Keymap with basic avy/easymotion jumps.")
  (general-define-key
   :keymaps 'w/teleport-map
   "w" #'evilem-motion-forward-word-begin
   "W" #'evilem-motion-forward-WORD-begin
   "f" #'evilem-motion-forward-word-end
   "F" #'evilem-motion-forward-WORD-end
   "b" #'evilem-motion-backward-word-begin
   "B" #'evilem-motion-backward-WORD-begin
   "gf" #'evilem-motion-backward-word-end
   "gF" #'evilem-motion-backward-WORD-end
   "e" #'evilem-motion-previous-line
   "E" #'evil-avy-goto-line-above
   "n" #'evilem-motion-next-line
   "N" #'evil-avy-goto-line-below
   "l" #'evil-avy-goto-line
   "h" #'w/easymotion-symbol-overlay
   "SPC" #'evil-avy-goto-char-timer
   "t" #'evilem-motion-find-char
   "T" #'evilem-motion-find-char-to
   "j" #'evilem-motion-find-char-backward
   "J" #'evilem-motion-find-char-to-backward
   "k" #'evilem-motion-search-next
   "K" #'evilem-motion-search-previous
   "/" #'evilem-motion-search-next
   "?" #'evilem-motion-search-previous)
  (general-define-key
   :states 'operator
   "SPC" w/teleport-map)

  (evilem-make-motion
   w/easymotion-symbol-overlay
   (list
    'w/symbol-overlay-jump-next-any
    'w/symbol-overlay-jump-previous-any))
  (evilem-make-motion
   w/easymotion-symbol-overlay
   '(w/symbol-overlay-jump-next-any w/symbol-overlay-jump-previous-any))

  ;; todo: commented stuff below needs rethinking and cleaning up
  ;; (evil-define-key* 'normal global-map
  ;;   (kbd "SPC a") (lambda () (interactive) (avy-goto-char-timer) (call-interactively 'evil-append))
  ;;   (kbd "SPC A") (lambda () (interactive) (w/avy-evil-goto-end-of-line) (call-interactively 'evil-append-line))
  ;;   (kbd "SPC c") (lambda () (interactive) (avy-goto-line) (evil-first-non-blank) (call-interactively 'evil-change-line))
  ;;   (kbd "SPC C") 'w/avy-evil-change-region
  ;;   (kbd "SPC i") (lambda () (interactive) (avy-goto-char-timer) (call-interactively 'evil-insert))
  ;;   (kbd "SPC I") (lambda () (interactive) (avy-goto-line) (call-interactively 'evil-insert-line))
  ;;   (kbd "SPC $") 'w/avy-evil-goto-end-of-line)
  ;; (defun w/evil-end-of-next-line ()
  ;;   (interactive)
  ;;   (evil-next-line)
  ;;   (end-of-line))
  ;; (evilem-make-motion-plain
  ;;  w/avy-evil-goto-end-of-line
  ;;  (list 'evil-end-of-line 'w/evil-end-of-next-line)
  ;;  :pre-hook (setq evil-this-type 'line)
  ;;  :bind ((scroll-margin 0))
  ;;  :initial-point (goto-char (window-start)))
  ;; (defun w/avy-evil-change-region ()
  ;;   "Select two lines and change the lines between them."
  ;;   (interactive)
  ;;   (avy-with w/avy-evil-change-region
  ;;     (let* ((beg (progn (avy-goto-line) (point)))
  ;;            (end (save-excursion (goto-char (avy--line)) (forward-line) (point))))
  ;;       (evil-change beg end 'line nil nil))))

  (defun w/avy-evil-delete-line ()
    "Select a line and delete it."
    (interactive)
    (avy-with w/avy-evil-delete-line
      (save-excursion
        (let ((line (avy--line)))
          (unless (eq line t)
            (goto-char line)
            (evil-delete-whole-line
             (point)
             (line-beginning-position 2)
             'line nil nil))))))

  (defun w/avy-evil-delete-lines ()
    "Select two lines and delete the lines between them."
    (interactive)
    (avy-with w/avy-evil-delete-lines
      (let* ((beg (avy--line))
             (end (save-excursion (goto-char (avy--line)) (forward-line) (point))))
        (evil-delete beg end 'line nil nil))))

  (defun w/avy-goto-line-any-window ()
    "Go to line in any visible window."
    (interactive)
    (setq current-prefix-arg 4)
    (call-interactively 'avy-goto-line)))

(use-package evil-indent-plus
  :defer t
  :general
  (:keymaps 'evil-inner-text-objects-map
   "i" #'evil-indent-plus-i-indent
   "I" #'evil-indent-plus-i-indent-up
   "J" #'evil-indent-plus-i-indent-up-down
   "TAB" #'evil-indent-plus-i-indent)
  (:keymaps 'evil-outer-text-objects-map
   "i" #'evil-indent-plus-a-indent
   "I" #'evil-indent-plus-a-indent-up
   "J" #'evil-indent-plus-a-indent-up-down
   "TAB" #'evil-indent-plus-a-indent-up))

(use-package evil-string-inflection
  :defer t
  :general
  (:keymaps 'normal
   "g~" #'evil-operator-string-inflection
   "g`" #'evil-operator-string-inflection))

(use-package evil-textobj-anyblock
  :defer t
  ;; todo perhaps replace with https://github.com/noctuid/targets.el
  :general
  (:keymaps 'evil-inner-text-objects-map
   "b" #'evil-textobj-anyblock-inner-block)
  (:keymaps 'evil-outer-text-objects-map
   "b" #'evil-textobj-anyblock-a-block))

(use-package expand-region
  :defer t
  :custom
  (expand-region-fast-keys-enabled nil)
  :general
  (:states 'visual
   "<tab>" #'w/hydra-expand-region/er/expand-region)
  :config
  (w/make-hydra w/hydra-expand-region (:foreign-keys run)
    "expand-region"
    "_<tab>_ expand"
    ("<tab>" er/expand-region :exit nil)
    "_u_ndo"
    ("u" (er/expand-region -1) :exit nil)
    "_r_eset"
    ("r" (er/expand-region 0) :exit t)))

(define-minor-mode w/show-trailing-whitespace-mode
  "Show or hide trailing whitespace."
  :lighter nil
  (setopt show-trailing-whitespace w/show-trailing-whitespace-mode))

(use-package thingatpt
  :config
  (defun w/thing-at-point-dwim (&optional deactivate-selection move-to-beginning)
    "Return the active region or the symbol at point."
    (let ((thing))
      (cond
       ((region-active-p)
        (setq thing (buffer-substring-no-properties (region-beginning) (region-end)))
        (when move-to-beginning
          (goto-char (region-beginning))))
       (t
        (setq thing (thing-at-point 'symbol t))
        (when move-to-beginning
          (goto-char (beginning-of-thing 'symbol)))))
      (when deactivate-selection
        (deactivate-mark))
      thing)))

(use-package emacs  ;; replace
  :commands w/query-replace-thing-at-point-dwim
  :config
  (defun w/query-replace-thing-at-point-dwim ()
    "Return ‘query-replace’ for the active region or the symbol at point."
    (interactive)
    (let* ((use-boundaries (not (region-active-p)))
           (thing (regexp-quote (w/thing-at-point-dwim t t)))
           (replacement
            (read-from-minibuffer
             (format "Replace ‘%s’ with: " thing)
             thing nil nil
             query-replace-to-history-variable)))
      (when use-boundaries
        (setq thing (format "\\_<%s\\_>" thing)))
      (query-replace-regexp thing replacement))))

(use-package emacs  ;; occur
  :hook (occur-mode-hook . w/occur-mode-hook)
  :general
  (:keymaps 'occur-mode-map
   :states '(motion normal)
   "RET" #'occur-mode-goto-occurrence
   "C-e" #'occur-prev
   "C-n" #'occur-next
   "C-p" #'occur-prev
   "g r" #'revert-buffer)
  :commands
  w/occur-dwim
  :config
  (evil-set-initial-state 'occur-mode 'motion)
  (defun w/occur-mode-hook ()
    (toggle-truncate-lines t)
    (w/set-major-mode-hydra #'w/hydra-occur/body))

  (w/make-hydra w/hydra-occur nil
    "occur"
    "_n__e_ nav"
    ("n" occur-next :exit nil)
    ("e" occur-prev :exit nil)
    "_f_ollow"
    ("f" next-error-follow-minor-mode))

  (defun w/occur-dwim (&optional nlines)
    "Call `occur' with a sane default."
    (interactive "P")
    (let ((thing (read-string
                  "Open occur for regexp: "
                  (regexp-quote (or (w/thing-at-point-dwim) ""))
                  'regexp-history)))
      (occur thing nlines)
      (evil-normal-state))))

(use-package ag
  :defer t
  :custom
  (ag-project-root-function 'projectile-project-root)
  (ag-reuse-buffers t)
  :hook (ag-mode-hook . w/ag-mode-hook)
  :commands
  w/counsel-ag-project
  w/counsel-ag-project-all-files
  :general
  (:keymaps 'ag-mode-map
   :states 'motion
   "gr" #'recompile)
  :config
  (defun w/ag-mode-hook ()
    (toggle-truncate-lines t))

  (defun w/counsel-ag-project (&optional unrestricted)
    "Run ‘counsel-ag’ on the current project."
    (interactive)
    (let ((extra-args (if unrestricted "--unrestricted" ""))
          (prompt (if unrestricted "search all project files: " "search project files: ")))
      (counsel-ag nil (projectile-project-root) extra-args prompt)))

  (defun w/counsel-ag-project-all-files ()
    "Run counsel-ag on all files within the project root."
    (interactive)
    (w/counsel-ag-project t)))

;; todo: switch to rg/ripgrep
;; (use-package rg)

;; todo: switch to deadgrep
;; perhaps use ,/ for hydra?
(use-package deadgrep
  :custom
  (deadgrep-display-buffer-function 'display-buffer)
  :commands
  w/hydra-search/body
  :general
  (:keymaps 'deadgrep-mode-map
   :states '(motion normal)
   "g" nil
   "g r" 'deadgrep-restart
   "C-n" 'deadgrep-forward
   "C-e" 'deadgrep-backward
   "C-p" 'deadgrep-backward
   "<return>" 'deadgrep-visit-result-other-window
   "<tab>" #'deadgrep-toggle-file-results
   )
  :config
  (evil-collection-init 'deadgrep)
  (w/make-hydra w/hydra-search nil
    "search"
    "_/_ search"
    ("/" deadgrep)))

(use-package symbol-overlay
  :demand t
  :delight
  :hook (enable-theme-functions . w/symbol-overlay-tweak-faces)
  :custom
  (symbol-overlay-idle-time 1.0)

  :commands
  w/symbol-overlay-put-dwim

  :general
  (:states 'motion
   "C-p" #'symbol-overlay-jump-prev
   "C-n" #'symbol-overlay-jump-next)
  (:states 'normal
   "C-p" #'w/evil-paste-pop-or-previous-symbol
   "C-n" #'w/evil-paste-pop-next-or-next-symbol)

  :config
  (setq symbol-overlay-map (make-sparse-keymap))

  (defun w/symbol-overlay-tweak-faces (theme)
    (set-face-attribute
     'symbol-overlay-default-face nil
     :foreground solarized-color-magenta
     :inherit 'unspecified)
    (let ((solarized-colors
           (list solarized-color-yellow-l
                 solarized-color-orange-l
                 solarized-color-red-l
                 solarized-color-magenta-l
                 solarized-color-violet-l
                 solarized-color-blue-l
                 solarized-color-cyan-l
                 solarized-color-green-l)))
      (--zip-with
       (set-face-attribute
        it nil
        :foreground (face-attribute 'default :background)
        :background other)
       symbol-overlay-faces
       solarized-colors)))

  (defun w/symbol-overlay-put-dwim ()
    "Toggle highlighting of the symbol at point (or the active region's content)."
    (interactive)
    (setq deactivate-mark t)
    (let* ((regexp
            (if (region-active-p)
                (regexp-quote (buffer-substring (region-beginning) (region-end)))
              (symbol-overlay-get-symbol)))
           (keyword (symbol-overlay-assoc regexp)))
      (if keyword
          (symbol-overlay-maybe-remove keyword)
        (symbol-overlay-put-all regexp nil))))

  (defun w/symbol-overlay-jump-any (direction)
    (-if-let*
        ((positions
          (->> (symbol-overlay-get-list 0)
               (-remove
                (lambda (ov)
                  (< (overlay-start ov) (point) (overlay-end ov))))
               (-map 'overlay-start)
               (-sort '<)
               (-uniq)))
         (target-position
          (if (eq direction 'forward)
              (-first (-partial '< (point)) positions)
            (-last (-partial '> (point)) positions))))
        (goto-char target-position)
      (user-error "No more highlighted symbols")))

  (defun w/symbol-overlay-jump-next-any ()
    (interactive)
    (w/symbol-overlay-jump-any 'forward))

  (defun w/symbol-overlay-jump-previous-any ()
    (interactive)
    (w/symbol-overlay-jump-any 'backward))

  (defun w/symbol-overlay-jump-first ()
    (interactive)
    (unless (symbol-overlay-get-list)
      (user-error "No highlighted symbols"))
    (goto-char (point-min))
    (w/symbol-overlay-jump-next-any))

  (defun w/symbol-overlay-jump-last ()
    (interactive)
    (unless (symbol-overlay-get-list)
      (user-error "No highlighted symbols"))
    (goto-char (point-max))
    (w/symbol-overlay-jump-previous-any))

  (defun w/evil-paste-pop-or-previous-symbol (count)
    "Either paste-pop (with COUNT) or jump to previous symbol occurrence."
    (interactive "p")
    (condition-case nil
        (evil-paste-pop count)
      (user-error
       (symbol-overlay-jump-prev))))

  (defun w/evil-paste-pop-next-or-next-symbol (count)
    "Either paste-pop-next (with COUNT) or jump to next symbol occurrence."
    (interactive "p")
    (condition-case nil
        (evil-paste-pop-next count)
      (user-error
       (symbol-overlay-jump-next)))))

;;;; previous/next navigation

;; previous/next thing (inspired by vim unimpaired)
;; todo: this should become a fancy hydra

(defun w/last-error ()
  "Jump to the last error; similar to ‘first-error’."
  (interactive)
  (condition-case err (while t (next-error)) (user-error nil)))

(general-define-key
 :states '(motion normal)
 "[ SPC" (lambda () (interactive) (save-excursion (evil-insert-newline-above)))
 "] SPC" (lambda () (interactive) (save-excursion (evil-insert-newline-below)))
 "[b" 'evil-prev-buffer
 "]b" 'evil-next-buffer
 "[c" 'flycheck-previous-error
 "]c" 'flycheck-next-error
 "[C" 'flycheck-first-error
 "]C" 'w/flycheck-last-error
 "[d" 'w/diff-hl-previous-hunk
 "]d" 'w/diff-hl-next-hunk
 "[e" 'previous-error
 "]e" 'next-error
 "[E" 'first-error
 "]E" 'w/last-error
 "[h" 'w/symbol-overlay-jump-previous-any
 "]h" 'w/symbol-overlay-jump-next-any
 "[H" 'w/symbol-overlay-jump-first
 "]H" 'w/symbol-overlay-jump-last
 "[m" 'smerge-prev
 "]m" 'smerge-next
 "[o" 'symbol-overlay-jump-prev
 "]o" 'symbol-overlay-jump-next
 "]s" (lambda () (interactive)
        (evil-forward-word)
        (call-interactively 'evil-next-flyspell-error))
 "[s" 'evil-prev-flyspell-error
 "[w" 'evil-window-prev
 "]w" 'evil-window-next
 "[z" 'origami-backward-fold-same-level
 "]z" 'origami-forward-fold
 "C-," 'evil-prev-buffer
 "C-." 'evil-next-buffer)

;; todo: this is not very useful currently
(w/make-hydra w/hydra-navigation-forward nil
  "nav"
  ("[" w/hydra-navigation-backward/body)
  ("]" nil)
  "_b_uffer"
  ("b" evil-next-buffer)
  ("B" evil-next-buffer :exit nil)
  ("[" w/hydra-navigation-backward/body)
  "_z_ folds"
  ("z" origami-forward-fold)
  ("Z" origami-forward-fold :exit nil))

(w/make-hydra w/hydra-navigation-backward nil
  "nav"
  ("[" nil)
  ("]" w/hydra-navigation-forward/body)
  "_b_uffer"
  ("b" evil-prev-buffer)
  ("B" evil-prev-buffer :exit nil)
  "_z_ folds"
  ("z" origami-backward-fold-same-level)
  ("Z" origami-backward-fold-same-level :exit nil))

(use-package rainbow-delimiters
  :defer t)

(use-package highlight-parentheses
  :defer t
  :delight)

(use-package adaptive-wrap
  :hook (visual-line-mode-hook . w/maybe-activate-adaptive-wrap-prefix-mode)
  :config
  (defun w/maybe-activate-adaptive-wrap-prefix-mode ()
    (when (derived-mode-p 'text-mode)
      (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))))

(use-package visual-fill-column
  :defer t)

(defvar w/wrap-lines-saved-fill-column nil
  "Saved ‘fill-column’ value.")

(use-package virtual-auto-fill)

(define-minor-mode w/wrap-lines-mode
  "Smart combination of auto-fill, visual-line, and visual-fill-column."
  :lighter" ⇶"
  (if w/wrap-lines-mode
      (progn
        (setq w/wrap-lines-saved-fill-column fill-column
              visual-fill-column-width fill-column
              fill-column most-positive-fixnum)
        ;; (auto-fill-mode -1)
        ;; (visual-line-mode)
        ;; (visual-fill-column-mode)
        (virtual-auto-fill-mode))
    (setopt fill-column w/wrap-lines-saved-fill-column
          visual-fill-column-width nil)
    ;; (visual-line-mode -1)
    ;; (auto-fill-mode)
    ;; (virtual-auto-fill-mode -1)
    (visual-fill-column-mode -1)))

(defun w/sensible-wrap-mode-1 ()
  "Dubious attempt at sensible wrapping, mode 1."
  (interactive)
  (let ((mode (if (derived-mode-p 'text-mode) 'w/wrap-lines-mode 'toggle-truncate-lines)))
    (call-interactively mode)))

(defun w/sensible-wrap-mode-2 ()
  "Dubious attempt at sensible wrapping, mode 2."
  (interactive)
  (let ((mode (if (derived-mode-p 'text-mode) 'toggle-truncate-lines 'visual-line-mode)))
    (call-interactively mode)))

(defun w/evil-fill-paragraph-dwim ()
  "Fill the current paragraph."
  (interactive)
  ;; move point after comment marker; useful for multi-line comments.
  (save-excursion
    (end-of-line)
    (fill-paragraph)))

(general-define-key :states 'normal
  "Q" #'w/evil-fill-paragraph-dwim)

(defun w/use-very-long-lines ()
  "Use very long lines so that `fill-paragraph' and related functions do not add newlines."
  (interactive)
  (setopt fill-column most-positive-fixnum)
  (auto-fill-mode -1))

(use-package origami
  :custom
  (origami-show-fold-header t)

  :custom-face
  (origami-fold-replacement-face ((t (:inherit magit-diff-context-highlight))))
  (origami-fold-fringe-face ((t (:inherit magit-diff-context-highlight))))

  :commands
  w/origami-parser-imenu-flat
  w/origami-mode-toggle

  :config
  (face-spec-reset-face 'origami-fold-header-face)

  (defun w/origami-mode-toggle ()
    (interactive)
    (origami-mode 'toggle)
    (when origami-mode
      (if (> (point) 1)
          (origami-show-only-node (current-buffer) (point))
        (origami-close-all-nodes (current-buffer)))))

  (defun w/origami-parser-imenu-flat (create)
    "Origami parser producing folds for each imenu entry, without nesting."
    (lambda (content)
      (let ((orig-major-mode major-mode))
        (with-temp-buffer
          (insert content)
          (funcall orig-major-mode)
          (let* ((items
                  (-as-> (imenu--make-index-alist t) items
                         (-flatten items)
                         (-filter 'listp items)))
                 (positions
                  (-as-> (-map #'cdr items) positions
                         (-filter 'identity positions)
                         (-map-when 'markerp 'marker-position positions)
                         (-filter 'natnump positions)
                         (cons (point-min) positions)
                         (-snoc positions (point-max))
                         (-sort '< positions)
                         (-uniq positions)))
                 (ranges
                  (-zip-pair positions (-map '1- (cdr positions))))
                 (fold-nodes
                  (--map
                   (-let*
                       (((range-beg . range-end) it)
                        (line-beg
                         (progn (goto-char range-beg)
                                (line-beginning-position)))
                        (offset
                         (- (min (line-end-position) range-end) line-beg))
                        (fold-node
                         (funcall create line-beg range-end offset nil)))
                     fold-node)
                   ranges)))
            fold-nodes))))))

;; todo https://github.com/sshaw/copy-as-format/issues/2
(use-package copy-as-format
  :general
  (:states 'visual
   "Y" #'w/evil-copy-as-format)
  :custom
  (copy-as-format-default "slack")
  (copy-as-format-format-alist  ;; only retain formats i use
   '(("github" copy-as-format--github)
     ("jira" copy-as-format--jira)
     ("markdown" copy-as-format--markdown)
     ("rst" copy-as-format--rst)
     ("slack" copy-as-format--slack)))
  :config
  (evil-define-operator w/evil-copy-as-format (beg end type)
    "Evilified version of copy-as-format"
    :move-point nil
    :repeat nil
    (interactive "<R>")
    (save-excursion
      (goto-char beg)
      (when (eq type 'line)
        (beginning-of-line))
      (push-mark (point) t t)
      (goto-char end)
      (when (eq type 'line)
        (forward-line -1)
        (end-of-line))
      (let ((current-prefix-arg t))
        (copy-as-format))
      (pop-mark))))


(use-package projectile
  :defer t
  :delight

  :custom
  (projectile-completion-system 'default)
  (projectile-current-project-on-switch 'keep)
  (projectile-ignored-projects '("/usr/local/" "~/"))
  ;; (projectile-mode-line nil)  ;; causes eager loading, :delight has same effect
  (projectile-require-project-root nil)
  (projectile-sort-order 'recently-active)
  (projectile-switch-project-action 'projectile-vc)

  :commands
  w/projectile-find-file-all
  w/projectile-open-gui-file-browser
  w/projectile-project-bury-buffers

  :init
  (add-hook 'find-file-hook (lambda () (require 'projectile)))

  :config
  (projectile-mode)

  (defun w/projectile-find-file-all (&optional pattern)
    "Find any file in the current project, including ignored files."
    (interactive
     (list
      (read-string
       "file name pattern (empty means all): "
       (if buffer-file-name
           (concat (file-name-extension buffer-file-name) "$")
         "")
       ".")))
    (let* ((collection
            (projectile-make-relative-to-root
             (directory-files-recursively (projectile-project-root) pattern)))
           (selection (completing-read "Find file in complete project: " collection nil t nil 'file-name-history))
           (name (concat (file-name-as-directory (projectile-project-root)) selection)))
      (find-file name)  ))

  (defun w/projectile-project-bury-buffers ()
    "Quit all windows and bury all buffers for the current project."
    (interactive)
    (-each (projectile-project-buffers)
      (lambda (buffer)
        (-each (get-buffer-window-list buffer)
          (lambda (window)
            (quit-window nil window)))
        (bury-buffer buffer))))

  (defun w/projectile-open-gui-file-browser ()
    "Open a GUI browser for the directory containing the current file."
    (interactive)
    (when-let ((directory-name (projectile-project-root)))
      (call-process "xdg-open" nil 0 nil directory-name)))
  )

(use-package avy
  :custom
  (avy-all-windows nil)
  (avy-all-windows-alt t)
  (avy-background t)
  (avy-style 'de-bruijn)
  (avy-keys (string-to-list "arstneiofu"))
  :commands
  avy-with) ;; used by evil-easymotion helpers

(defun w/declare-jump (command)
  "Declare COMMAND to be nonrepeatable."
  (evil-add-command-properties command :jump t))

(--each '(evil-backward-paragraph
          evil-backward-section-begin
          evil-backward-section-end
          evil-forward-paragraph
          evil-forward-section-begin
          evil-forward-section-end
          evil-goto-first-line
          evil-goto-line
          evil-goto-mark
          evil-goto-mark-line
          evil-scroll-down
          evil-scroll-page-down
          evil-scroll-page-up
          evil-scroll-up
          evil-window-bottom
          evil-window-middle
          evil-window-top
          recenter-top-bottom
          switch-to-buffer)
  (w/declare-jump it))

(use-package dumb-jump
  :general
  (:states 'motion
   "gd" #'w/dumb-jump-go
   "gD" #'w/dumb-jump-go-other-window)

  :config

  (defun w/dumb-jump-go ()
    (interactive)
    (let ((s (w/thing-at-point-dwim t)))
      ;; dumb-jump does not have proper extensibility via
      ;; defcustom variable for this. :(
      (when (derived-mode-p 'rst-mode)
        (setq s (string-remove-suffix "_" s)))
      (dumb-jump-go nil nil s)))

  (defun w/dumb-jump-go-other-window ()
    (interactive)
    (let ((dumb-jump-window 'other))
      (w/dumb-jump-go)))

  (w/declare-jump 'w/dumb-jump-go)
  (w/declare-jump 'w/dumb-jump-go-other-window)

  (defun w/jump-around-advice (fn &rest args)
    ;; TODO: figure out whether the buffer changed. if the jump was in
    ;; the same buffer, check whether the target was already between
    ;; (window-start) and (window-end), and if so, avoid scrolling.
    (let ((original-buffer (current-buffer))
          (original-point (point))
          (original-window-start (window-start))
          (original-window-end (window-end)))
      (evil-set-jump)
      (apply fn args)
      (unless (and (eq (current-buffer) original-buffer)
                   (<= original-window-start (point) original-window-end))
        (recenter-top-bottom 0))
      (unless (and (eq (current-buffer) original-buffer)
                   (eq (point) original-point)))))

  (advice-add 'dumb-jump-go :around #'w/jump-around-advice))


;;;; frames and windows

;; my preferred window layout is multiple full-height windows,
;; next to each other in a horizontal fashion, i.e. screen
;; divided into columns.

(use-package emacs
  :config
  (defun w/fit-bottom-error-window-to-buffer (window)
    "Size request for a small error window at the bottom."
    (fit-window-to-buffer window 10 5)))

(defun w/evil-window-next-or-vsplit ()
  "Focus next window, or vsplit if it is the only window in this frame."
  (interactive)
  (if (> (count-windows) 1)
      (evil-window-next nil)
    (evil-window-vsplit)))

(defun w/evil-goto-window (n)
  "Go to window N."
  (evil-window-top-left)
  (evil-window-next n))

(defun w/goto-window-1 ()
  "Go to the first window."
  (interactive)
  (w/evil-goto-window 1))

(defun w/goto-window-2 ()
  "Go to the second window."
  (interactive)
  (w/evil-goto-window 2))

(defun w/goto-window-3 ()
  "Go to the third window."
  (interactive)
  (w/evil-goto-window 3))

(defun w/goto-window-4 ()
  "Go to the fourth window."
  (interactive)
  (w/evil-goto-window 4))

(defun w/goto-window-5 ()
  "Go to the fifth window."
  (interactive)
  (w/evil-goto-window 5))

(defun w/goto-window-6 ()
  "Go to the sixth window."
  (interactive)
  (w/evil-goto-window 6))

(w/declare-jump 'w/evil-window-next-or-vsplit)
(w/declare-jump 'w/goto-window-1)
(w/declare-jump 'w/goto-window-2)
(w/declare-jump 'w/goto-window-3)
(w/declare-jump 'w/goto-window-4)
(w/declare-jump 'w/goto-window-5)
(w/declare-jump 'w/goto-window-6)

;; todo: write these bindings in a more concise way
(cond
 ((eq system-type 'darwin)  ;; osx: command key
  (evil-define-key*
    'motion global-map
    (kbd "s-1") 'w/goto-window-1
    (kbd "s-2") 'w/goto-window-2
    (kbd "s-3") 'w/goto-window-3
    (kbd "s-4") 'w/goto-window-4
    (kbd "s-5") 'w/goto-window-5
    (kbd "s-6") 'w/goto-window-6)
  (bind-keys
   ("s-1" . w/goto-window-1)
   ("s-2" . w/goto-window-2)
   ("s-3" . w/goto-window-3)
   ("s-4" . w/goto-window-4)
   ("s-5" . w/goto-window-5)
   ("s-6" . w/goto-window-6)
   ))
 (t  ;; others: control key
  (evil-define-key*
    'motion global-map
    (kbd "C-SPC") 'evil-window-next
    (kbd "C-S-SPC") 'evil-window-next
    (kbd "C-`") 'evil-window-next
    (kbd "C-~") 'evil-window-prev
    (kbd "C-1") 'w/goto-window-1
    (kbd "C-2") 'w/goto-window-2
    (kbd "C-3") 'w/goto-window-3
    (kbd "C-4") 'w/goto-window-4
    (kbd "C-5") 'w/goto-window-5
    (kbd "C-6") 'w/goto-window-6)
  (bind-keys
   ("C-SPC" . evil-window-next)
   ("C-S-SPC" . evil-window-prev)
   ("C-`" . evil-window-next)
   ("C-~" . evil-window-prev)
   ("C-1" . w/goto-window-1)
   ("C-2" . w/goto-window-2)
   ("C-3" . w/goto-window-3)
   ("C-4" . w/goto-window-4)
   ("C-5" . w/goto-window-5)
   ("C-6" . w/goto-window-6))))

(use-package buffer-move
  :defer t)

(defun w/make-frame-new-buffer ()
  "Make a new frame with a new buffer."
  (interactive)
  (with-selected-frame (make-frame)
    (call-interactively #'evil-buffer-new)))

;; replace evil-window-map completely
(general-define-key
 :states '(emacs motion)
 (kbd "C-w") w/window-map)

(use-package ispell
  :defer t
  :custom
  (ispell-dictionary "english"))

;; todo https://github.com/d12frosted/flyspell-correct
(use-package flyspell
  :defer t
  :delight " ∼")
;; (use-package flyspell-correct-ivy)  ;; todo

(use-package guess-language
  :defer t
  :custom
  (guess-language-languages '(en nl)))

(defvar w/ivy-height-percentage 30
  "Percentage of the screen height that ivy should use.")

(use-package counsel
  :after ivy
  :delight
  :config
  ;; (counsel-mode)
  (ivy-configure 'counsel-M-x :initial-input ""))

(use-package ivy
  :demand t
  :delight
  :hook (window-size-change-functions . w/adjust-ivy-height)
  :general
  (:keymaps 'ivy-minibuffer-map
   "C-h" 'ivy-backward-delete-char
   "C-w" 'ivy-backward-kill-word
   "C-u" 'kill-whole-line
   "C-<return>" 'ivy-immediate-done
   "C-<tab>" 'ivy-insert-current
   "<escape>" 'minibuffer-keyboard-quit)
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-height 20)
  (ivy-wrap t)
  :config
  (setopt ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
  (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight))
  ;; (ivy-mode 1)
  (defun w/clamp-number (num low high)
    "Clamp NUM between LOW and HIGH."
    (min high (max num low)))
  (defun w/adjust-ivy-height (frame)
    "Adjust ivy-height based on the current FRAME height."
    (let* ((total-lines (frame-text-lines frame))
           (lines (truncate (* total-lines w/ivy-height-percentage 0.01)))
           (new-height (w/clamp-number lines 10 20)))
      (setopt ivy-height new-height))))

(use-package ivy-hydra)

(use-package ivy-rich
  :after ivy counsel
  :custom
  (ivy-rich-parse-remote-buffer nil)
  :config
  (ivy-rich-mode 1))

(use-package smex)

(use-package autorevert
  :delight auto-revert-mode
  :custom
  (auto-revert-check-vc-info t)
  (auto-revert-interval 15)
  :config
  (global-auto-revert-mode))

(use-package magit
  :defer t
  :delight
  (magit-wip-mode)

  :hook
  (git-commit-mode-hook . w/git-commit-mode-hook)
  (magit-process-mode-hook . goto-address-mode)

  :custom
  (git-commit-fill-column 72)
  (magit-blame-heading-format "%C %-10a %s")
  (magit-blame-mode-lighter " annotate")
  (magit-blame-time-format "%Y%m%d")
  (magit-branch-prefer-remote-upstream '("master"))
  (magit-branch-read-upstream-first 'fallback)
  (magit-bury-buffer-function 'magit-mode-quit-window)
  (magit-completing-read-function 'magit-builtin-completing-read)
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function 'display-buffer)
  (magit-list-refs-sortby '("-committerdate"))
  (magit-prefer-remote-upstream t)
  (magit-process-popup-time 10)
  (magit-status-goto-file-position t)

  :custom-face
  (magit-mode-line-process ((t (:inherit magit-mode-line-process-error))))

  :commands
  magit-toggle-buffer-lock
  w/git-web-browse
  w/gitlab-insert-merge-request-template
  w/magit-log-buffer-file-follow
  w/magit-status-other-repository

  :init
  (add-hook 'find-file-hook (lambda () (require 'magit)))

  :config
  ;; note: a :general stanza won't work because of execution order:
  ;; custom bindings go on top of what evil-collection-init does
  ;; todo: make ,q use the various magit-*-bury-buffer functions, then
  ;; unbind q to force ,q usage.
  (general-def
    :keymaps 'magit-mode-map
    :states '(normal visual)
    [escape] nil
    "n" #'evil-next-visual-line
    "e" #'evil-previous-visual-line
    "C-n" #'magit-section-forward
    "C-e" #'magit-section-backward
    "C-p" #'magit-section-backward
    "<tab>" #'magit-section-cycle
    "C-<tab>" #'magit-section-toggle
    "C-w" w/window-map
    "/" 'consult-line)
  (general-def
    :keymaps 'magit-blame-read-only-mode-map
    :states '(motion normal)
    "n" nil
    "e" nil
    "C-n" #'magit-blame-next-chunk
    "C-e" #'magit-blame-previous-chunk
    "C-p" #'magit-blame-previous-chunk
    "<tab>" #'magit-blame-cycle-style
    "<return>" 'magit-show-commit)
  (general-def
    :keymaps 'magit-diff-mode-map
    "SPC" nil
    "DEL" nil)
  (general-def
    :keymaps 'magit-hunk-section-map
    "<return>" #'magit-diff-visit-file-other-window
    "C-<return>" #'magit-diff-visit-worktree-file-other-window)
  (general-def
    :keymaps '(magit-diff-mode-map
               magit-log-mode-map
               magit-mode-map
               magit-process-mode-map
               magit-refs-mode
               magit-revision-mode-map
               magit-status-mode-map)
    :states 'normal
    "q" nil
    "'" nil)
  (general-def
    :keymaps '(magit-diff-mode-map
               magit-log-mode-map
               magit-mode-map
               magit-process-mode-map
               magit-refs-mode
               magit-revision-mode-map
               magit-status-mode-map)
    "q" nil
    "'" nil)

  ;; no special behaviour for magit windows
  (remove-hook 'magit-post-display-buffer-hook 'magit-maybe-set-dedicated)

  ;; (magit-wip-mode)

  (--each '(("~" . 2)
            ("~/Projects/" . 2)
            ("~/Documents/" . 3)
            ("~/Sync/" . 3))
    (-let [(dir . depth) it]
      (add-to-list 'magit-repository-directories (cons dir depth) t)))

  (add-to-list 'evil-overriding-maps '(magit-blame-mode-map . nil))

  (transient-replace-suffix 'magit-commit 'magit-commit-autofixup
    '(6 "x" "Absorb changes" magit-commit-absorb))
  (transient-append-suffix 'magit-push
    "-n" '("/c" "Skip Gitlab CI" "--push-option=ci.skip"))
  (transient-append-suffix 'magit-push
    "/c" '("/m" "Create Gitlab merge request" "--push-option=merge_request.create"))

  ;; hide author names from magit-blame annotations;
  ;; it's usually about why/what/when, not who.
  (setf (->> magit-blame-styles
             (alist-get 'headings)
             (alist-get 'heading-format))
        "%C %s\n")

  (with-eval-after-load 'direnv
    (--each '(magit-blob-mode
              magit-diff-mode
              magit-log-mode
              magit-status-mode)
      (add-to-list 'direnv-non-file-modes it)))

  (with-eval-after-load 'evil-colemak-basics
    (add-to-list 'global-evil-colemak-basics-modes
                 '(not magit-log-mode magit-status-mode)))

  (defun w/git-commit-mode-hook ()
    (when (and (bobp) (eolp))
      (call-interactively #'evil-insert)))

  (defun w/magit-status-other-repository ()
    "Open git status for another repository."
    (interactive)
    (setq current-prefix-arg t)
    (call-interactively 'magit-status))

  (defun w/git-web-browse ()
    "Open a web browser for the current git repo or file."
    (interactive)
    (if (region-active-p)
        (let ((git-link-open-in-browser t))
          (call-interactively #'git-link)
          (setq kill-ring (cdr kill-ring)))
      (call-interactively #'git-link)))

  (defun w/magit-log-buffer-file-follow ()
    (interactive)
    (magit-log-buffer-file t))

  (defun w/gitlab-insert-merge-request-template ()
    (interactive)
    (-if-let* ((template-dir ".gitlab/merge_request_templates/")
               (dir (locate-dominating-file (or (buffer-file-name) default-directory) template-dir))
               (template-file
                (read-file-name "Template: " (concat dir template-dir)) nil t 'file-regular-p))
        (insert-file-contents template-file)
      (user-error "No merge request templates found"))))

(use-package evil-collection
  :demand t
  :after evil
  :custom
  evil-collection-want-unimpaired-p nil

  :config
  (defun w/colemak-hnei-rotation (_mode mode-keymaps &rest _rest)
    (evil-collection-translate-key 'normal mode-keymaps
      "n" "j"
      "e" "k"
      "i" "l"
      "j" "e"
      "k" "n"
      "l" "i"))

  ;; todo this messes up my own overrides somehow
  ;; (add-hook 'evil-collection-setup-hook #'w/colemak-hnei-rotation)
  (evil-collection-init))

(use-package magit-imerge
  :after magit)

(use-package forge
  :after magit
  :demand t
  :general
  (:keymaps 'magit-mode-map
   "'" nil
   "h" 'forge-dispatch)
  :config
  (with-demoted-errors "%S"
    (transient-suffix-put 'magit-dispatch "@" :key "h"))
  (transient-append-suffix 'forge-dispatch "f n" '("w" "web" git-link-homepage)))

(use-package diff-hl
  :hook
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  (diff-hl-mode-hook . diff-hl-update)
  :commands
  w/diff-hl-next-hunk
  w/diff-hl-previous-hunk
  :config
  (w/declare-jump 'w/diff-hl-next-hunk)
  (w/declare-jump 'w/diff-hl-previous-hunk)
  (defun w/diff-hl-previous-hunk ()
    "Jump to the previous hunk."
    (interactive)
    (diff-hl-mode)
    (diff-hl-previous-hunk))
  (defun w/diff-hl-next-hunk ()
    "Jump to the next hunk."
    (interactive)
    (diff-hl-mode)
    (diff-hl-next-hunk))
  (defun w/diff-hl-update-around-advice (fn &rest args)
    (let ((vc-handled-backends '(Git)))
      (apply fn args)))
  (advice-add 'diff-hl-update :around #'w/diff-hl-update-around-advice))

(use-package vdiff
  :defer t
  :custom
  (vdiff-subtraction-style 'single)
  (vdiff-subtraction-fill-char ?·)
  (vdiff-3way-layout-function 'w/vdiff-3way-layout-function-vertical)
  :general
  (:keymaps 'vdiff-mode-map
   :states 'normal
   [remap evil-save-modified-and-close] 'vdiff-quit)
  :custom-face
  (vdiff-addition-face ((t (:inherit magit-diff-added))))
  (vdiff-change-face ((t (:inherit magit-diff-base))))
  (vdiff-subtraction-face ((t (:inherit magit-diff-removed))))
  (vdiff-refine-added ((t (:inherit magit-diff-added-highlight))))
  (vdiff-refine-changed ((t (:inherit magit-diff-base-highlight))))
  (vdiff-open-fold-face ((t (:inherit magit-diff-context))))
  (vdiff-closed-fold-face ((t (:inherit magit-diff-context-highlight))))

  :commands
  vdiff-close-all-folds
  vdiff-close-fold
  vdiff-hydra/body
  vdiff-next-fold
  vdiff-next-hunk
  vdiff-open-all-folds
  vdiff-open-fold
  vdiff-previous-fold
  vdiff-previous-hunk
  vdiff-receive-changes
  vdiff-receive-changes-and-step
  vdiff-refine-all-hunks
  vdiff-refine-this-hunk
  vdiff-refresh
  vdiff-remove-refinements-in-hunk
  vdiff-send-changes
  vdiff-send-changes-and-step

  :config
  (defun w/vdiff-3way-layout-function-vertical (buffer-a buffer-b buffer-c)
    (delete-other-windows)
    (switch-to-buffer buffer-a)
    (set-window-buffer (split-window-horizontally) buffer-c)
    (set-window-buffer (split-window-horizontally) buffer-b)))

(use-package vdiff-magit
  :demand t
  :after magit vdiff
  :custom
  (vdiff-magit-stage-is-2way t)
  :general
  (:keymaps 'magit-mode-map
   "e" #'vdiff-magit-dwim
   "E" #'vdiff-magit)
  :config
  (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
  (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
  (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
  (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit))

(use-package prog-mode
  :ensure nil
  :defer t
  :hook (prog-mode-hook . w/prog-mode-hook)
  :config
  (defun w/prog-mode-hook ()
    (setq-local comment-auto-fill-only-comments t)
    (auto-fill-mode)
    (column-number-mode)
    (flyspell-prog-mode)
    (highlight-parentheses-mode)
    (symbol-overlay-mode)
    (w/show-trailing-whitespace-mode)))

(use-package cc-mode
  :defer t
  :hook (c-mode-hook . w/c-mode-hook)
  :config
  (defun w/c-mode-hook ()
    (setopt evil-shift-width 2)
    (evil-swap-keys-swap-double-single-quotes)
    (evil-swap-keys-swap-square-curly-brackets)
    (evil-swap-keys-swap-underscore-dash)))

(use-package caddyfile-mode
  :hook (caddyfile-mode-hook . w/caddyfile-mode-hook)
  :config
  (defun w/caddyfile-mode-hook ()
    (setopt tab-width 2))
  (reformatter-define caddyfile-format
    :program "caddy"
    :args '("fmt" "-")
    :lighter " caddyfmt"
    :group 'caddyfile-format))

(use-package compile
  :defer t
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  :general
  (:keymaps 'compilation-mode-map
   :states '(motion normal)
   "g r" #'recompile
   "C-e" #'compilation-previous-error
   "C-n" #'compilation-next-error
   "C-p" #'compilation-previous-error)

  :hook
  (compilation-mode-hook . w/compilation-mode-hook)
  (compilation-finish-functions . w/compilation-finished)

  :config
  (w/make-hydra w/hydra-compilation nil
    "compilation"
    "_r_ecompile"
    ("r" recompile))

  (defun w/compilation-use-xterm-color-filter ()
    (remove-hook 'comint-output-filter-functions 'ansi-color-process-output t)
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter t))

  (defun w/compilation-mode-hook ()
    (electric-pair-local-mode -1)
    (w/set-major-mode-hydra #'w/hydra-compilation/body)
    (w/compilation-use-xterm-color-filter)
    (remove-hook 'comint-output-filter-functions 'comint-postoutput-scroll-to-bottom))

  (defun w/compilation-finished (buffer _status)
    (with-current-buffer buffer
      (evil-normal-state))))

(use-package comint
  :defer t
  :ensure nil

  :custom
  (comint-move-point-for-output 'all)

  :general
  (:keymaps 'comint-mode-map
   "<escape>" #'evil-normal-state)
  (:keymaps 'comint-mode-map
   :states 'normal
   "C-e" #'comint-previous-prompt
   "C-n" #'comint-next-prompt
   "C-p" #'comint-previous-prompt
   "<return>" 'w/comint-find-file-or-goto-end)
  (:keymaps 'comint-mode-map
   :states 'insert
   "<return>" #'comint-send-input
   "C-n" #'comint-next-input
   "C-p" #'comint-previous-input
   "C-r" #'comint-history-isearch-backward
   "C-/" #'w/comint-insert-history)

  :hook (comint-mode-hook . w/compilation-mode-hook)

  :config
  (evil-set-initial-state 'comint-mode 'normal)

  (defun w/comint-find-file-or-goto-end ()
    (interactive)
    (condition-case nil
        (evil-find-file-at-point-with-line)
      (user-error
       (goto-char (point-max))
       (evil-append-line 0))))

  (defun w/comint-insert-history ()
    (interactive)
    (let* ((collection (-uniq (ring-elements comint-input-ring)))
           (text (completing-read "Command history: " collection nil t)))
      (insert text))))

(use-package xterm-color
  :defer t)

(use-package cus-edit
  :ensure nil
  :general
  (:keymaps 'custom-mode-map
   :states 'normal
   "<return>" #'Custom-newline
   "C-e" #'widget-backward
   "C-n" #'widget-forward
   "C-p" #'widget-backward
   "ZZ" #'Custom-buffer-done
   "<down-mouse-1>" #'widget-button-click
   "<down-mouse-2>" #'widget-button-click)
  :config
  (evil-set-initial-state 'Custom-mode 'normal))

(use-package cython-mode
  :defer t)

(use-package flycheck-cython
  :demand t
  :after cython-mode flycheck)

(use-package elisp-mode
  :defer t
  :ensure emacs
  :general
  (:keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
   :states 'insert
   "C-d" 'lispy-delete)
  (:keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
   :states '(motion normal visual)
   "H" 'lispyville-backward-sexp
   "I" 'lispyville-forward-sexp
   "(" 'lispyville-left
   ")" 'lispyville-right
   ">" 'lispyville->
   "<" 'lispyville-<)
  :hook (emacs-lisp-mode-hook . w/emacs-lisp-mode-hook)

  :config
  (defun w/emacs-lisp-mode-hook ()
    (setopt
     evil-lookup-func 'w/helpful-evil-lookup-func
     evil-shift-width 2)
    (w/set-major-mode-hydra #'w/hydra-emacs-lisp/body)
    (electric-pair-local-mode -1)
    (lispy-mode)
    (lispyville-mode)
    (aggressive-indent-mode)
    (highlight-parentheses-mode -1)
    (rainbow-delimiters-mode))

  (require 'which-func)
  (add-to-list 'which-func-modes 'emacs-lisp-mode)

  (w/make-hydra w/hydra-emacs-lisp nil
    "elisp"
    "_b_ eval-buffer"
    ("b" eval-buffer)
    "_d_ eval-defun"
    ("d" eval-defun)
    "_e_val-last-sexp"
    ("e" eval-last-sexp)
    ("E" pp-eval-last-sexp)
    "_h_elp"
    ("h" helpful-at-point)
    "_m_acro-expand"
    ("m" pp-macroexpand-last-sexp)
    "_r_ eval-region"
    ("r" eval-region)))

(use-package lispy
  :demand t
  :after elisp-mode
  :delight
  :config
  (lispy-set-key-theme '(lispy)))

(use-package lispyville
  :demand t
  :after elisp-mode
  :delight " 🎂"
  :custom
  (lispyville-key-theme
   '(operators
     c-w
     prettify
     (atom-movement t)
     commentary))
  :config
  (lispyville-set-key-theme))

(use-package graphql-mode :defer t)

(use-package groovy-mode
  :defer t)

(defgroup prettier nil
  "Formatting using Prettier."
  :group 'languages)
(reformatter-define prettier-format
  :program "prettier"
  :args `("--stdin-filepath"
          ,(file-name-nondirectory (buffer-file-name (buffer-base-buffer))))
  :lighter " Prettier"
  :group 'prettier)
(delight 'prettier-format-on-save-mode " 💄" t)
(reformatter-define prettier-format-html
  :program "prettier"
  :args '("--parser=html")
  :lighter " Prettier"
  :group 'prettier)
(reformatter-define prettier-format-js
  :program "prettier"
  :args '("--parser=babel")
  :lighter " Prettier"
  :group 'prettier)

(use-package sgml-mode
  :defer t
  :hook
  (html-mode-hook . w/html-mode-hook)
  (mhtml-mode-hook . w/html-mode-hook)
  :config
  (defun w/html-mode-hook ()
    (reformatter-dwim-select 'prettier-format)
    (setopt evil-shift-width 2)))

(use-package js2-mode
  :defer t
  :mode
  (rx ".cjs" string-end)
  (rx ".mjs" string-end)
  :hook
  (js-mode-hook . w/js-mode-hook)
  (js-mode-hook . js2-minor-mode)
  :config
  (defun w/js-mode-hook ()
    (modify-syntax-entry ?_ "w")
    (setopt
     tab-width 2
     evil-shift-width tab-width
     js-indent-level tab-width)
    (reformatter-dwim-select 'prettier-format)))

(use-package json-mode
  :defer t
  :hook (json-mode-hook . w/json-mode-hook)
  :config
  (defun w/json-mode-hook ()
    (setopt
     tab-width 2
     evil-shift-width tab-width
     js-indent-level tab-width)
    (reformatter-dwim-select 'jq-format-json)
    (evil-swap-keys-swap-colon-semicolon)
    (evil-swap-keys-swap-double-single-quotes)))

(use-package markdown-mode
  :defer t
  :hook (markdown-mode-hook . w/markdown-mode-hook)
  :custom
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t)

  :config
  (defun w/markdown-mode-hook ()
    (setopt evil-shift-width 2)
    (w/set-major-mode-hydra #'w/hydra-markdown/body)
    (flyspell-mode)
    (w/evil-surround-define-surround-trigger-pairs
     :local
     "b" "**" "**" ;; strong emphasiss
     "c" "`" "`" ;; inline code
     "e" "*" "*")) ;; emphasis

  (evil-declare-repeat 'markdown-promote)
  (evil-declare-repeat 'markdown-demote)

  (w/make-hydra w/hydra-markdown nil
    "markdown"
    "_1__2__3__4__5_ _!__@_ _h_eader"
    ("1" markdown-insert-header-atx-1)
    ("2" markdown-insert-header-atx-2)
    ("3" markdown-insert-header-atx-3)
    ("4" markdown-insert-header-atx-4)
    ("5" markdown-insert-header-atx-5)
    ("!" markdown-insert-header-setext-1)
    ("@" markdown-insert-header-setext-2)
    "_c_ode"
    ("c" markdown-insert-gfm-code-block)
    "_h_yperlink"
    ("h" markdown-insert-link)
    "_i_mage"
    ("i" markdown-insert-image)
    "_n_umber"
    ("n" markdown-cleanup-list-numbers)
    "_p_review"
    ("p" markdown-preview)
    ("P" markdown-live-preview-mode)
    "_q_uote"
    ("q" w/markdown-blockquote-dwim))

  (defun w/markdown-blockquote-dwim ()
    (interactive)
    (if (region-active-p)
        (call-interactively #'markdown-blockquote-region)
      (save-excursion
        (markdown-blockquote-region
         (line-beginning-position)
         (line-end-position))))))

(use-package org
  :defer t
  :hook (org-mode-hook . w/org-mode-hook)
  :config
  (defun w/org-mode-hook ()
    (evil-org-mode))
  :custom
  (org-ellipsis " [...]"))

(use-package evil-org
  :defer t
  :after org
  :delight
  :custom
  (evil-org-retain-visual-state-on-shift t)
  :config
  (setopt
   evil-org-movement-bindings
   '((left . "h")
     (down . "n")
     (up . "e")
     (right . "i")))
  (evil-org-set-key-theme))

(use-package python
  :defer t
  :interpreter ("python" . python-mode)
  :hook (python-mode-hook . w/python-mode-hook)
  :mode
  ((rx ".bzl" string-end) . python-mode)  ;; starlark
  ((rx ".pyi" string-end) . python-mode)

  :general
  (:keymaps 'python-mode-map
   :states 'normal
   [remap evil-join] #'w/evil-join-python
   [backspace] 'python-nav-backward-up-list
   "<return>" 'python-black-partial-dwim)
  (:keymaps 'python-mode-map
   :states '(operator visual)
   "H" 'python-nav-backward-sexp-safe
   "I" 'python-nav-forward-sexp-safe
   "ae" 'evil-indent-plus-a-indent-up
   "an" 'evil-indent-plus-a-indent-up-down
   "ue" 'evil-indent-plus-i-indent-up
   "un" 'evil-indent-plus-i-indent-up-down)
  (:keymaps 'inferior-python-mode-map
   :states 'insert
   "<tab>" #'python-shell-completion-complete-or-indent)

  :config
  (add-to-list 'which-func-modes 'python-mode)

  (defun w/python-mode-hook ()
    (setopt fill-column 79)
    (setq-local
     comment-fill-column 72
     indent-bars-starting-column 12)
    (if-let* ((env-var-value (getenv "EMACS_PYTHON_ISORT_ENABLED"))
              (_ (string-equal env-var-value "0")))
        nil
      (python-isort-on-save-mode-enable-dwim))
    (if-let* ((env-var-value (getenv "EMACS_PYTHON_BLACK_ENABLED"))
              (_ (string-equal env-var-value "0")))
        nil
      (python-black-on-save-mode-enable-dwim))
    (reformatter-dwim-select 'python-black)
    (modify-syntax-entry ?_ "w")
    (w/set-major-mode-hydra #'w/hydra-python/body)
    (evil-swap-keys-swap-colon-semicolon)
    (evil-swap-keys-swap-underscore-dash)
    (indent-bars-mode)
    ;; (lispyville-mode)
    (origami-mode)
    ;; (python-docstring-mode)
    (w/evil-surround-define-surround-trigger-pairs
     :local
     "`" "``" "``") ;; for reStructuredText literals in docstrings
    (evil--add-to-alist
     origami-parser-alist
     'python-mode 'w/origami-parser-imenu-flat))

  ;; todo: integrate this with the global easymotion hydra
  ;; (evilem-make-motion
  ;;  w/easymotion-python
  ;;  (list
  ;;   ;; Collect interesting positions around point, and all visible
  ;;   ;; blocks in the window. Results are ordered: forward after point,
  ;;   ;; then backward from point.
  ;;   'python-nav-end-of-statement 'python-nav-end-of-block 'python-nav-forward-block
  ;;   'python-nav-beginning-of-statement 'python-nav-beginning-of-block 'python-nav-backward-block)
  ;;  :pre-hook (setq evil-this-type 'line))
  ;; (evil-define-key* 'motion python-mode-map
  ;;   (kbd "SPC TAB") 'w/easymotion-python)
  ;; (defun w/swiper-python-definitions ()
  ;;   (interactive)
  ;; (swiper "^\\s-*\\(def\\|class\\)\\s- "))
  ;; (evil-define-key* 'motion python-mode-map
  ;;   (kbd "SPC /") 'w/swiper-python-definitions)

  (reformatter-define python-isort
    :program "isort"
    :args '("--atomic" "--stdout" "-")
    :lighter " 📜"
    :group 'python)

  (defun python-isort-on-save-mode-enable-dwim ()
    (interactive)
    ;; note: this is brittle as it relies on python-black internals for
    ;; its pyproject.toml detection logic
    (when-let*
        ((file-name (buffer-file-name))
         (python-black--config-file-marker-regex (rx bol "[tool.isort]" eol))
         (project-uses-isort (python-black--in-blackened-project-p file-name))
         (is-3rd-party (not (python-black--third-party-file-p file-name))))
      (python-isort-on-save-mode)))

  (evil-define-operator w/evil-join-python (beg end)
    "Like ‘evil-join’, but handles comments and some continuation styles sensibly."
    :motion evil-line
    (evil-join beg end)
    (let ((first-line-is-comment
           (save-excursion
             (evil-first-non-blank)
             (looking-at-p "#")))
          (joined-line-is-comment
           (looking-at " *#")))
      (cond
       ;; joining two comment lines: remove # character
       ((and first-line-is-comment joined-line-is-comment)
        (delete-region (point) (match-end 0))
        (just-one-space))
       ;; joining non-comment line with a comment line:
       ;; ensure two spaces before the ‘#’ comment marker (pep8)
       (joined-line-is-comment
        (delete-region (match-beginning 0) (match-end 0))
        (insert "  #")
        (forward-char)
        (just-one-space))
       ;; the joined line starts with period; remove the space before
       ;; it. this is useful for ‘fluent’ (chained) method calls,
       ;; e.g. sqlalchemy queries.
       ((looking-at-p (rx " ."))
        (delete-char 1))
       ;; the first line ends with a comma and the joined line starts
       ;; with a closing paren: remove the unnecessary comma.
       ((and (looking-back "," nil)
             (looking-at-p (rx (or ")" "}" "]"))))
        (delete-char -1))
       ;; adjacent string literals: turn into one by removing space separated quotes
       ((--any
         (and (looking-back (rx (literal it)) nil)
              (looking-at-p (rx (seq  " " (literal it)))))
         '("\"" "'"))
        (backward-char)
        (delete-char 3)))))

  (defun w/python-insert-statement (position statement)
    "Insert a new STATEMENT before or after the statement at point."
    (python-nav-beginning-of-statement)
    (let ((indent (buffer-substring-no-properties
                   (line-beginning-position) (point))))
      (cond
       ((eq position 'after)
        (python-nav-end-of-statement)
        (insert "\n" indent statement)
        (python-nav-beginning-of-statement))
       ((eq position 'before)
        (insert-before-markers statement "\n" indent)
        (python-nav-backward-statement)))))

  (defun w/python-insert-breakpoint ()
    "Insert a pdb trace statement using PDB-MODULE before the current statement."
    (interactive nil python-mode)
    (w/python-insert-statement 'before "breakpoint()  # FIXME"))

  (defun w/python-insert-ipython-repl (position)
    "Insert an IPython repl statement before or after the current statement."
    (w/python-insert-statement
     position
     (format "__import__(\"IPython\").embed()  # FIXME")))

  (evil-define-command w/python-print-expression ()
    (interactive)
    (let ((thing (w/thing-at-point-dwim)))
      (w/python-insert-statement
       'after
       (format "print(f\"{%s=}\")" thing))))

  (evil-define-command w/python-reveal-type-expression ()
    (interactive)
    (let ((thing (w/thing-at-point-dwim)))
      (w/python-insert-statement
       'after
       (format "reveal_type(%s)" thing))))

  (defun w/python-kwargize ()
    (interactive nil python-mode)
    (when-let ((thing (thing-at-point 'symbol)))
      (goto-char (beginning-of-thing 'symbol))
      (save-excursion
        (insert (format "%s=" thing)))))

  (evil-define-operator w/python-refactor-make-variable (beg end _type)
    "Refactor the current region into a named variable."
    (interactive "<R>")
    (let ((name (read-string "Variable name: "))
          (code (delete-and-extract-region beg end)))
      (insert name)
      (w/python-insert-statement
       'before
       (format "%s = %s" name code))))

  (defun w/python-insert-import-statement ()
    "Add an import statement for the thing at point."
    (interactive nil python-mode)
    (let ((thing (w/thing-at-point-dwim)))
      (unless thing
        (user-error "No thing at point"))
      (w/python-insert-statement
       'before
       (format "import %s  # fixme: move to proper place" thing))))

  (evil-define-command w/python-split-string ()
    "Split the string at point into two strings on multiple lines"
    (interactive)
    (-if-let* ((pss (syntax-ppss))
               (quote-char (nth 3 pss))
               ;; (quote-char (if (eql quote-char t) "\"\"\"" quote-char))
               (string-start-pos (nth 8 pss))
               (current-char (char-to-string (char-after)))
               (string-prefix "")
               (string-delimiter ""))
        (save-match-data
          (save-excursion
            ;; detect prefix for f-strings, byte strings, etc.
            (goto-char string-start-pos)
            (when (looking-back (rx (+ (any "BbFfRrUu"))) nil)
              (setq string-prefix (thing-at-point 'symbol)))
            (setq string-delimiter
                  (cond
                   ((eql quote-char t)
                    (looking-at (rx (or "\"\"\"" "'''" "\"" "'")))
                    (match-string 0))
                   (t
                    (char-to-string quote-char)))))
          ;; split before point, but after a space if on one
          (when (string-equal current-char " ")
            (forward-char))
          ;; split after space
          (insert string-delimiter)
          (save-excursion
            (insert string-prefix string-delimiter))
          (newline-and-indent))
      (user-error "No string at point")))

  (evil-declare-repeat 'w/python-split-string)

  (w/make-hydra w/hydra-python nil
    "python"
    "_b_reakpoint"
    ("b" w/python-insert-breakpoint)
    "_c_overage"
    ("c" python-coverage-overlay-mode)
    "_i_mport"
    ("i" w/python-insert-import-statement)
    "_k_ kwarg"
    ("k" w/python-kwargize)
    "_m_ mypy"
    ("m" w/python-reveal-type-expression)
    "_p_ print"
    ("p" w/python-print-expression)
    "_r_epl"
    ("r" (w/python-insert-ipython-repl 'after))
    ("R" (w/python-insert-ipython-repl 'before))
    "_s_ split"
    ("s" w/python-split-string)
    "_t_ pytest"
    ("t" python-pytest-dispatch)
    ("T" python-pytest-repeat)
    "_v_ariable"
    ("v" w/python-refactor-make-variable)))

(use-package evil-text-object-python
  :demand t
  :after evil python
  :general
  (:keymaps 'python-mode-map
   :states '(operator visual)
   "ul" 'evil-text-object-python-inner-statement
   "al" 'evil-text-object-python-outer-statement
   "uf" 'evil-text-object-python-function)
  (:keymaps 'python-mode-map
   :states 'operator
   [remap evil-forward-char] #'w/evil-forward-char-or-python-statement)
  :config
  (defun w/evil-forward-char-or-python-statement (count)
    "Intelligently pick a statement or a character."
    (interactive "p")
    (cond
     ((memq this-command '(evil-change lispyville-change))
      (evil-text-object-python-inner-statement count))
     ((memq this-command '(evil-delete evil-shift-left evil-shift-right lispyville-delete))
      (evil-text-object-python-outer-statement count))
     (t
      (evil-forward-char count)))))

(use-package evil-python-movement
  :demand t
  :after evil python
  :general
  (:keymaps 'python-mode-map
   :states 'normal
   "[{" 'evil-python-movement-lsb-lsb
   "]}" 'evil-python-movement-rsb-rsb
   "[[" 'evil-python-movement-lsb-m
   "]]" 'evil-python-movement-rsb-m))

(use-package pip-requirements
  :defer t
  :mode
  ((rx "requirements-" (* any) ".in" string-end) . pip-requirements-mode)

  :config
  ;; avoid network traffic when opening a requirements.txt file
  (setq pip-packages '(this is a fake package listing)))

(use-package python-docstring
  :defer t
  :delight
  :custom
  (python-fill-docstring-style 'symmetric)
  (python-docstring-sentence-end-double-space nil))

(use-package python-pytest
  :demand t
  :after python
  :custom
  (python-pytest-arguments
   '("--color"
     "--failed-first"
     "--maxfail=10"
     "--verbose --verbose"))
  :general
  (:keymaps 'python-pytest-mode-map
   :states 'motion
   "g r" #'python-pytest-repeat)

  :hook
  (python-pytest-mode-hook . w/python-pytest-mode-hook)
  (python-pytest-finished-hook . evil-force-normal-state)

  :config
  (w/make-hydra w/hydra-python-pytest nil
    "python-pytest"
    "_r_epeat"
    ("r" python-pytest-repeat nil)
    "_t_ pytest"
    ("t" python-pytest-dispatch nil)
    ("T" python-pytest-repeat nil))

  (defun w/python-pytest-mode-hook ()
    (origami-mode)
    (w/show-trailing-whitespace-mode -1)
    (w/compilation-use-xterm-color-filter)
    (remove-hook 'comint-output-filter-functions 'comint-postoutput-scroll-to-bottom t)
    (w/set-major-mode-hydra #'w/hydra-python-pytest/body)
    (modify-syntax-entry ?/ ".")
    (when-let ((project-root (projectile-project-root)))
      (add-to-list 'prettify-symbols-alist `(,(string-remove-suffix "/" project-root) . ?…)))
    (when-let ((venv-path (getenv "VIRTUAL_ENV")))
      (add-to-list 'prettify-symbols-alist `(,venv-path . ?…)))
    (prettify-symbols-mode))

  (defun w/python-pytest-origami-parser (create)
    (lambda (content)
      (let ((orig-major-mode major-mode))
        (with-temp-buffer
          (insert content)
          (goto-char (point-min))
          (let ((nodes nil)
                (node nil)
                (pos (point-min)))
            (while (search-forward-regexp "^===" nil t)
              (beginning-of-line)
              (setq node (funcall
                          create
                          pos
                          (1- (point))
                          (save-excursion
                            (goto-char pos)
                            (- (line-end-position) (line-beginning-position)))
                          nil)
                    nodes (cons node nodes)
                    pos (point))
              (forward-char))
            (setq node (funcall
                        create
                        (1- (point))
                        (point-max)
                        (progn
                          (end-of-line)
                          (current-column))
                        nil)
                  nodes (cons node nodes))
            (reverse nodes))))))

  (require 'origami)
  (evil--add-to-alist
   origami-parser-alist
   'python-pytest-mode 'w/python-pytest-origami-parser))

(use-package rst
  :defer t
  :hook (rst-mode-hook . w/rst-mode-hook)
  :custom
  (rst-default-indent 0)
  (rst-indent-comment 2)
  (rst-indent-field 2)
  (rst-indent-literal-normal 2)
  (rst-preferred-adornments '((?= over-and-under 0)
                              (?= simple 0)
                              (?- simple 0)
                              (?~ simple 0)
                              (?+ simple 0)
                              (?` simple 0)
                              (?# simple 0)
                              (?@ simple 0)))
  (rst-preferred-bullets '(?- ?*))

  :config
  (defun w/rst-mode-hook ()
    (setopt
     evil-shift-width 2
     rst-mode-abbrev-table nil)
    (w/set-major-mode-hydra #'w/hydra-rst/body)
    (flyspell-mode)
    (origami-mode)
    (sphinx-mode)
    (evil--add-to-alist
     origami-parser-alist
     'rst-mode 'w/origami-parser-imenu-flat)
    (w/evil-surround-define-surround-trigger-pairs
     :local
     "b" "**" "**" ;; strong
     "c" "``" "``" ;; inline code
     "C" ".. code-block::\n\n" "" ;; code-block
     "d" ":doc:`" " <...>`" ;; doc link
     "e" "*" "*" ;; emphasis
     "l" "`" " <...>`_" ;; hyperlink
     "t" ":term:`" "`") ;; glossary term
    (make-local-variable 'evil-inner-text-objects-map)
    (general-define-key
     :keymaps 'evil-inner-text-objects-map
     "c" #'w/evil-rst-inner-code-block)
    (general-define-key
     :keymaps 'evil-outer-text-objects-map
     "c" #'w/evil-rst-a-code-block))

  (w/make-hydra w/hydra-rst nil
    "restructuredtext"
    "_a_djust"
    ("a" rst-adjust)
    "_c_ edit code block"
    ("c" w/rst-edit-code-block-dwim)
    "_e_mphasise"
    ("e" w/evil-rst-emphasise)
    "_l_ist"
    ("l" w/evil-rst-bullet-list)
    ("L" w/evil-rst-bullet-list-all)
    "_s_trong"
    ("s" w/evil-rst-strong)
    "_w_rap"
    ("w" w/evil-rst-wrap))

  (require 'dumb-jump)
  (add-to-list
   'dumb-jump-language-file-exts
   '(:language "rst" :ext "rst" :agtype "restructuredtext" :rgtype "rst"))
  (add-to-list
   'dumb-jump-find-rules
   '(:language "rst"
     :type "variable"
     :supports ("ag" "grep" "rg" "git-grep")
     ;; :regex "\\.\\.\\s+_JJJ:"  ;; seems to work
     :regex "\\.\\.\\s+\(_|\\\|\)JJJ\(:|\\\|\)"
     :tests (".. _test:" "  .. _test:\n" ".. |test|")
     :not (".. _tester" "  ..image:: test.png" ".. test::")))
  ;; (custom-initialize-default dumb-jump-find-rules)

  (defvar w/rst-code-block-language-major-modes-mapping
    '(("elisp" . emacs-lisp-mode))
    "Mapping of code block languages to major modes.")

  (defun w/rst-goto-beginning-of-block ()
    (let ((language))
      (search-backward "::")
      (goto-char (match-end 0))
      (setq language
            (string-trim (buffer-substring-no-properties
                          (point) (line-end-position))))
      (when (string-empty-p language)
        (setq language nil))
      (forward-line)
      (while (and (looking-at-p "$") (not (eobp)))
        (forward-line))
      language))

  (defun w/evil-rst-code-block (type)
    "Find the range of an inner/outer code block."
    (let* (language
           (range
            (save-excursion
              (setq language (w/rst-goto-beginning-of-block))
              (if (eq type 'inner)
                  (evil-indent-plus-i-indent)
                (evil-indent-plus-a-indent)))))
      (unless (<= (first range) (point) (second range) )
        (user-error "Not in a block"))
      (cons language range)))

  (evil-define-text-object w/evil-rst-a-code-block (count &optional _beg _end _type)
    (cdr (w/evil-rst-code-block 'outer)))

  (evil-define-text-object w/evil-rst-inner-code-block (count &optional _beg _end _type)
    (cdr (w/evil-rst-code-block 'inner)))

  (defun w/rst-edit-code-block-dwim ()
    (interactive)
    (let* ((block (w/evil-rst-code-block 'inner))
           (language (car block))
           (range (cdr block))
           (block-major-mode
            (or
             (cdr (assoc language w/rst-code-block-language-major-modes-mapping))
             (when language (intern (concat language "-mode")))))
           (edit-indirect-guess-mode-function
            (lambda (parent-buffer beg end)
              (if (and block-major-mode (symbolp block-major-mode))
                  (funcall block-major-mode)
                ;; (python-mode)
                (edit-indirect-default-guess-mode parent-buffer beg end)
                ))))
      (edit-indirect-region (first range) (1+ (second range)) t)))

  ;; todo: integrate this with the global easymotion hydra
  ;; (evilem-make-motion
  ;;  w/easymotion-rst
  ;;  (list 'rst-forward-section 'rst-backward-section)
  ;;  :pre-hook (setq evil-this-type 'line))
  ;; (evil-define-key* 'motion rst-mode-map
  ;;   (kbd "SPC TAB") 'w/easymotion-rst)
  (evil-define-operator w/evil-rst-bullet-list (beg end &optional count)
    :type line
    (interactive "<r>P")
    (let ((all (not (null count))))
      (rst-bullet-list-region beg end all)))
  (evil-define-operator w/evil-rst-bullet-list-all (beg end)
    :type line
    (interactive "<r>")
    (w/evil-rst-bullet-list beg end t))
  (evil-define-operator w/evil-rst-wrap (beg end type open close)
    (interactive "<R><C>")
    (let ((end-marker (make-marker)))
      (set-marker end-marker end)
      (set-marker-insertion-type end-marker t)
      (save-excursion
        (goto-char beg)
        (when (and (eq type 'line)
                   (bolp)
                   (looking-at adaptive-fill-regexp))
          (goto-char (match-end 0)))
        (insert open)
        (goto-char end-marker)
        (when (eq type 'line)
          (backward-char))
        (insert (or close open)))
      (set-marker end-marker nil)))
  (evil-define-operator w/evil-rst-emphasise (beg end type)
    (interactive "<R>")
    (w/evil-rst-wrap beg end type "*"))
  (evil-define-operator w/evil-rst-strong (beg end type)
    (interactive "<R>")
    (w/evil-rst-wrap beg end type "**"))
  (defun w/rst-header-1 ()
    (interactive)
    ;; todo: this is incomplete
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      (save-restriction
        (narrow-to-region beg end)
        (rst-adjust-section-title nil)))))

(use-package rust-mode
  :defer t
  :hook (rust-mode-hook . w/rust-mode-hook)
  :config
  (defun w/rust-mode-hook ()
    (evil-swap-keys-swap-underscore-dash)
    (evil-swap-keys-swap-double-single-quotes)
    (evil-swap-keys-swap-square-curly-brackets)
    (origami-mode)
    (evil--add-to-alist
     origami-parser-alist
     'rust-mode 'w/origami-parser-imenu-flat)))

(use-package typescript-ts-mode
  ;; built-in
  :hook (typescript-ts-mode-hook . w/typescript-ts-mode-hook)
  :mode
  (rx (or ".ts" ".cts" ".mts") string-end)
  :custom
  (typescript-indent-level 2)
  :general
  (:keymaps 'typescript-ts-mode-map
   :states 'insert
   ;; "<return>" #'c-context-line-break
   )
  :config
  (defun w/typescript-ts-mode-hook ()
    (reformatter-dwim-select 'prettier-format)
    (lsp-deferred)))

(use-package web-mode
  :defer t
  :mode (rx ".vue" string-end)
  :hook (web-mode-hook . w/web-mode-hook)
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-enable-comment-interpolation t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-indent-style 2)
  (web-mode-markup-comment-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-part-padding 0)
  (web-mode-script-padding 0)
  (web-mode-style-padding 0)
  :custom-face
  (web-mode-current-column-highlight-face
   ((t (:inherit hl-line
        :background unspecified
        :weight unspecified))))
  :config
  (setf (alist-get "vue" web-mode-engines-auto-pairs nil nil 'equal) nil)
  (defun w/web-mode-hook ()
    (reformatter-dwim-select 'prettier-format)
    (lsp-deferred)))

(use-package woman
  :defer t
  :init
  (add-to-list
   'warning-suppress-log-types
   '((defvaralias losing-value woman-topic-history))))

(use-package yaml-mode
  :defer t
  :hook (yaml-mode-hook . w/yaml-mode-hook)
  :config
  (defun w/yaml-mode-hook ()
    (setopt evil-shift-width yaml-indent-offset)
    (evil-swap-keys-swap-colon-semicolon)
    (evil-swap-keys-swap-double-single-quotes)
    (origami-mode)
    (evil--add-to-alist
     origami-parser-alist
     'yaml-mode 'w/origami-parser-imenu-flat)))

(load (concat user-emacs-directory "init-local") t)

(provide 'init)
;;; init.el ends here
