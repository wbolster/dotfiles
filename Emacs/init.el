;;; init.el --- emacs configuration -*- lexical-binding: t; -*-

;;;; Commentary:

;; Emacs configuration

;;;; Code:

;;;
;;; Packages
;;;

(setq
 load-prefer-newer t
 package-archives '(
  ("melpa" . "https://melpa.org/packages/")
  ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
  ("gnu" . "https://elpa.gnu.org/packages/")))

(require 'package)
(package-initialize)
(require 'auto-compile)
(auto-compile-on-load-mode)

(defmacro after (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))


;;;
;;; General
;;;

(setq
 disabled-command-function nil
 tls-checktrust 'ask)
(fset 'yes-or-no-p 'y-or-n-p)
(modify-syntax-entry ?_ "w")

;; ensure a server is running
(require 'server)
(unless (server-running-p)
  (server-start))

;; Backup and autosave files
(setq
 auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/\\1" t))
 backup-directory-alist '((".*" . "~/.emacs.d/backups/"))
 make-backup-files nil)
(defun my-ask-confirmation-for-unsaved-buffers ()
  "Ask for confirmation when a non-special buffer has not been saved."
  ;; Do not ask for buffers that have a * in their name, except
  ;; buffers that start with *new* since that is the template used by
  ;; evil-mode for new buffers.
  (if (and
       (buffer-modified-p)
       (not (buffer-file-name))
       (or
        (string-prefix-p "*new*" (buffer-name))
        (not (string-match-p "\*" (buffer-name)))))
      (y-or-n-p (format "Buffer %s modified but not saved; kill anyway? " (buffer-name)))
    t))
(add-to-list 'kill-buffer-query-functions 'my-ask-confirmation-for-unsaved-buffers)

;; Resist agitprop
(global-set-key (kbd "C-h g") nil)
(global-set-key (kbd "C-h C-c") nil)
(global-set-key (kbd "C-h C-m") nil)
(global-set-key (kbd "C-h C-o") nil)
(global-set-key (kbd "C-h C-w") nil)

;; OSX keyboard
(global-set-key (kbd "s-q") nil)
(setq ns-right-alternate-modifier 'none)


;;;
;;; Visual appearance
;;;

;; Feedback while typing
(setq echo-keystrokes 0.5)
(show-paren-mode t)

;; Reduce clutter
(setq
 frame-resize-pixelwise t
 frame-title-format "%b"
 inhibit-startup-screen t
 initial-scratch-message nil
 ns-use-native-fullscreen nil)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(toggle-frame-maximized)
(tool-bar-mode -1)

;; Theme
(defvar my-dark-theme 'solarized-dark "The preferred dark theme.")
(defvar my-light-theme 'solarized-light "The preferred light theme.")
(setq
 solarized-emphasize-indicators nil
 solarized-scale-org-headlines nil
 solarized-use-less-bold t
 solarized-use-variable-pitch nil
 solarized-height-minus-1 1.0
 solarized-height-plus-1 1.0
 solarized-height-plus-2 1.0
 solarized-height-plus-3 1.0
 solarized-height-plus-4 1.0)
(defun my-toggle-dark-light-theme ()
  "Toggle between a dark and light theme."
  (interactive)
  (if (eq (first custom-enabled-themes) my-light-theme)
      (load-theme my-dark-theme t)
    (load-theme my-light-theme t)))
(defun my-set-theme-from-environment ()
  "Set the theme based on presence/absence of a configuration file."
  (interactive)
  (if (file-exists-p "~/.config/dark-theme")
      (load-theme my-dark-theme t)
    (load-theme my-light-theme t)))
(defun my-disable-themes ()
  "Disable all enabled themes."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))
(advice-add 'load-theme :before '(lambda (&rest args) (my-disable-themes)))
(my-set-theme-from-environment)

;; Cursor
(blink-cursor-mode 0)
(setq
 evil-normal-state-cursor   '("#859900" box)     ; green
 evil-visual-state-cursor   '("#cb4b16" box)     ; orange
 evil-insert-state-cursor   '("#268bd2" bar)     ; blue
 evil-replace-state-cursor  '("#dc322f" bar)     ; red
 evil-operator-state-cursor '("#dc322f" hollow)) ; red

;; Font faces
(defun my-remove-bold-underline-from-all-faces ()
  "Remove unwanted attributes from all font faces."
  (interactive)
  (mapc
   (lambda (face) (set-face-attribute face nil :weight 'normal :underline nil))
   (face-list)))
;; (my-remove-bold-underline-from-all-faces)  ;; Disabled for now because
                                              ;; color theming alone is
                                              ;; not good enough.

;; Mode line
(setq rm-blacklist '(
  " Abbrev"
  " ARev"
  " company"
  " counsel"
  " DS"  ;; python-docstring
  " Fill"
  " FlyC"
  " hl-p"
  " hl-s"  ;; highlight-symbol
  " hnei"  ;; evil-colemak-basics
  " ivy"
  " Outl"
  " s-/"  ;; evil-commentary
  " snipe"
  " SP"  ;; smart-parens
  " Undo-Tree"
))
(setq
 sml/line-number-format "%l"
 sml/name-width '(1 . 40)
 sml/projectile-replacement-format "%s:")
(sml/setup)

;; Line numbering
(defun my-relative-line-numbers-format (offset)
  "Format relative line number for OFFSET."
  (number-to-string (abs (if (= offset 0) (line-number-at-pos) offset))))
(setq relative-line-numbers-format 'my-relative-line-numbers-format)

;; Misc
(setq fci-rule-width 2)


;;;
;;; Whitespace
;;;

(setq
 require-final-newline 'visit-save
 sentence-end-double-space nil)
(setq-default
 indent-tabs-mode nil
 show-trailing-whitespace t
 tab-width 4)
(defun my-hide-trailing-whitespace ()
  "Helper to hide trailing whitespace, intended for mode hooks."
  (setq show-trailing-whitespace nil))
(add-hook 'buffer-menu-mode-hook 'my-hide-trailing-whitespace)
(defun my-toggle-show-trailing-whitespace ()
  "Toggle `show-trailing-whitespace`."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))


;;;
;;; evil
;;;

(setq
 evil-cleverparens-swap-move-by-word-and-symbol t
 evil-cross-lines t
 evil-want-C-u-scroll t
 evil-want-C-w-in-emacs-state t)
(setq
 evil-colemak-basics-char-jump-commands 'evil-snipe)
(setq
 evil-snipe-auto-disable-substitute nil
 evil-snipe-override-evil-repeat-keys nil
 evil-snipe-scope 'line
 evil-snipe-repeat-scope 'line
 evil-snipe-smart-case t
 evil-snipe-tab-increment t)

(require 'evil)
(require 'evil-magit)
(add-to-list 'evil-overriding-maps '(magit-blame-mode-map . nil))

(evil-mode)
(evil-commentary-mode)
(global-evil-colemak-basics-mode)
(global-evil-surround-mode)
(global-evil-swap-keys-mode)
(global-evil-visualstar-mode)

;; use Y to copy to the end of the line; see evil-want-Y-yank-to-eol
(evil-add-command-properties 'evil-yank-line :motion 'evil-end-of-line)

;; esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(with-eval-after-load 'ivy
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit))
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; overwrite evil-surround defaults to not put spaces inside braces
(evil-add-to-alist
 'evil-surround-pairs-alist
 ?\( '("(" . ")")
 ?\[ '("[" . "]")
 ?\{ '("{" . "}"))

;; text objects
(evil-define-text-object my-evil-text-object-symbol-dwim (count &optional beg end type)
  "Intelligently pick evil-inner-symbol or evil-a-symbol."
  (if (eq this-command 'evil-delete)
      (evil-a-symbol count)
    (evil-inner-symbol count)))
(evil-define-key '(operator visual) global-map
  "o" 'my-evil-text-object-symbol-dwim)
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
(define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
(define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block)
(evil-indent-plus-default-bindings)

;; directory navigation (inspired by vim vinagre)
(evil-define-key 'motion global-map "-" 'dired-jump)
(define-key dired-mode-map "-" 'dired-jump)


;;;
;;; leader key shortcuts (augmented elsewhere in config)
;;;

(defvar my-leader-map
  (make-sparse-keymap)
  "Keymap for 'leader key' shortcuts.")
(defvar my-visual-leader-map
  (make-sparse-keymap)
  "Keymap for 'leader key' shortcuts in visual mode.")
(evil-define-key 'motion global-map "," my-leader-map)
(evil-define-key 'visual global-map "," my-visual-leader-map)
(define-key my-leader-map " " 'whitespace-cleanup)
(define-key my-leader-map "b" 'ivy-switch-buffer)
(define-key my-leader-map "B" 'ivy-switch-buffer-other-window)
(define-key my-leader-map "f" 'counsel-find-file)
(define-key my-leader-map "F" 'find-file-other-window)
(define-key my-leader-map "k" (lambda () (interactive) (kill-buffer nil)))
(define-key my-leader-map "K" 'kill-buffer-and-window)
(define-key my-leader-map "s" 'save-buffer)
(define-key my-leader-map "S" 'save-some-buffers)
(define-key my-leader-map "u" 'universal-argument)
(define-key my-leader-map "x" 'counsel-M-x)
(define-key my-leader-map "+" 'evil-numbers/inc-at-pt)
(define-key my-leader-map "=" 'evil-numbers/inc-at-pt)  ;; without shift key
(define-key my-leader-map "-" 'evil-numbers/dec-at-pt)


;;;
;;; movement and editing
;;;

;; scrolling
(setq
 indicate-buffer-boundaries 'left
 scroll-conservatively 101
 scroll-margin 5)
(evil-define-key 'motion global-map
  (kbd "z z") 'recenter-top-bottom)

;; j/k should move visual lines. do not modify evil-motion-state,
;; since that will break operators taking a motion, e.g. 'dj' to
;; delete the current and next line.
(evil-define-key '(normal visual) global-map
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  (kbd "C-j") 'evil-next-line
  (kbd "C-k") 'evil-previous-line)

;; smart parens
(require 'smartparens-config)
(smartparens-global-mode)
(show-smartparens-global-mode)

;; some emacs and shell style bindings (emacs inspired) in insert mode
(defun my-evil-transpose-chars ()
  "Invoke 'transpose-chars' on the right chars in insert state."
  (interactive)
  (backward-char)
  (transpose-chars nil)
  (unless (eolp) (forward-char)))
(defun my-kill-line-dwim ()
  "Kill line, or join the next line when at eolp."
  (interactive)
  (if (not (eolp))
      (kill-line)
    (kill-line)
    (delete-horizontal-space)))
(evil-define-key 'insert global-map
  (kbd "C-a") 'evil-first-non-blank
  (kbd "C-d") 'delete-char
  (kbd "C-e") 'end-of-line
  (kbd "C-h") [backspace]
  (kbd "C-k") 'my-kill-line-dwim
  (kbd "C-n") 'next-line
  (kbd "C-p") 'previous-line
  (kbd "C-t") 'my-evil-transpose-chars
  ;; during typing, ctrl-v is "paste", like everywhere else
  (kbd "C-v") 'yank
  ;; shift line with < and > (same chars as in normal mode; replaces C-d and C-t)
  (kbd "C-,") 'evil-shift-left-line
  (kbd "C-<") 'evil-shift-left-line
  (kbd "C-.") 'evil-shift-right-line
  (kbd "C->") 'evil-shift-right-line)

;; previous/next thing (inspired by vim unimpaired)
(defun my-last-error ()
  "Jump to the last error; similar to 'first-error'."
  (interactive)
  (condition-case err (while t (next-error)) (user-error nil)))
(defun my-flycheck-last-error ()
  "Jump to the last flycheck error."
  (interactive)
  (goto-char (point-max))
  (flycheck-previous-error))
(evil-define-key 'motion global-map
  (kbd "[ SPC") (lambda () (interactive) (save-excursion (evil-insert-newline-above)))
  (kbd "] SPC") (lambda () (interactive) (save-excursion (evil-insert-newline-below)))
  "[b" 'evil-prev-buffer
  "]b" 'evil-next-buffer
  "[c" 'flycheck-previous-error
  "]c" 'flycheck-next-error
  "[C" 'flycheck-first-error
  "]C" 'my-flycheck-last-error
  "[d" 'diff-hl-previous-hunk
  "]d" 'diff-hl-next-hunk
  "[e" 'previous-error
  "]e" 'next-error
  "[E" 'first-error
  "]E" 'my-last-error
  "[m" 'smerge-prev
  "]m" 'smerge-next
  "[s" 'highlight-symbol-prev
  "]s" 'highlight-symbol-next
  "[S" 'highlight-symbol-prev-in-defun
  "]S" 'highlight-symbol-next-in-defun
  "[w" 'evil-window-prev
  "]w" 'evil-window-next
  "[z" 'outline-previous-visible-heading
  "]z" 'outline-next-visible-heading)

;; jumps
(add-hook 'evil-jumps-post-jump-hook #'nav-flash-show)

;; quickly swap two text objects using "gx"; the empty text object is
;; a trick to make "gxp" work to move previously marked text without
;; moving anything back to the original location.
(evil-define-text-object
  my-evil-empty-text-object (count &optional beg end type)
  "Empty text object for cut/paste style evil-exchange interaction."
  (evil-range (point) (point)))
(define-key evil-operator-state-map "p" 'my-evil-empty-text-object)
(evil-exchange-install)

;; symbol navigation, without masking evil-paste-pop functionality.
(defun my-evil-paste-pop-or-highlight-symbol-prev (count)
  "Either paste-pop (with COUNT) or jump to previous symbol occurence."
  (interactive "p")
  (condition-case nil
      (evil-paste-pop count)
    (user-error
     (highlight-symbol-prev))))
(defun my-evil-paste-pop-next-or-highlight-symbol-next (count)
  "Either paste-pop-next (with COUNT) or jump to next symbol occurence."
  (interactive "p")
  (condition-case nil
      (evil-paste-pop-next count)
    (user-error
     (highlight-symbol-next))))
(evil-define-key 'motion global-map
  (kbd "C-p") 'highlight-symbol-prev
  (kbd "C-n") 'highlight-symbol-next)
(evil-define-key 'normal global-map
  (kbd "C-p") 'my-evil-paste-pop-or-highlight-symbol-prev
  (kbd "C-n") 'my-evil-paste-pop-next-or-highlight-symbol-next)

;; move text around
(require 'drag-stuff)
(evil-define-key 'normal global-map
  (kbd "M-j") 'drag-stuff-down
  (kbd "M-k") 'drag-stuff-up
  (kbd "M-h") 'evil-shift-left-line
  (kbd "M-l") 'evil-shift-right-line)
(evil-define-key 'visual global-map
  (kbd "M-h") (lambda (beg end)
                (interactive "r")
                (evil-shift-left beg end)
                (evil-force-normal-state)
                (call-interactively 'evil-visual-restore))
  (kbd "M-l") (lambda (beg end)
                (interactive "r")
                (evil-shift-right beg end)
                (evil-force-normal-state)
                (call-interactively 'evil-visual-restore)))

;; indent on enter, keeping comments open (if any)
(evil-define-key 'insert global-map
  (kbd "RET") 'comment-indent-new-line)

;; filling
(defun my-evil-fill-paragraph-dwim ()
  "Dwim helper to fill the current paragraph"
  (interactive)
  ;; Move point after comment marker; useful for multi-line comments.
  (end-of-line)
  (fill-paragraph)
  (evil-first-non-blank))
(evil-define-key 'normal global-map
  "Q" 'my-evil-fill-paragraph-dwim)

;; dumb jump
(setq dumb-jump-selector 'ivy)
(defun my-jump-around-advice (fn &rest args)
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
                 (eq (point) original-point))
      (nav-flash-show))))
(advice-add 'dumb-jump-go :around 'my-jump-around-advice)
(evil-define-key 'motion global-map
  "gd" 'dumb-jump-go-current-window
  "gD" 'dumb-jump-go-other-window)

;; avy and evil-easymotion
(setq
 avy-all-windows nil
 avy-all-windows-alt t
 avy-background t
 avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o))
(avy-setup-default)
(evilem-default-keybindings "SPC")
(defun my-avy-evil-change-region ()
  "Select two lines and change the lines between them."
  (interactive)
  (avy-with my-avy-evil-change-region
    (let* ((beg (progn (avy-goto-line) (point)))
           (end (save-excursion (goto-char (avy--line)) (forward-line) (point))))
      (evil-change beg end 'line nil nil))))
(defun my-avy-evil-delete-line ()
  "Select a line and delete it."
  (interactive)
  (avy-with my-avy-evil-delete-line
    (save-excursion
      (let ((line (avy--line)))
        (unless (eq line t)
          (goto-char line)
          (evil-delete-whole-line
           (point)
           (line-beginning-position 2)
           'line nil nil))))))
(defun my-avy-evil-delete-region ()
  "Select two lines and delete the lines between them."
  (interactive)
  (avy-with my-avy-evil-delete-region
    (let* ((beg (avy--line))
           (end (save-excursion (goto-char (avy--line)) (forward-line) (point))))
      (evil-delete beg end 'line nil nil))))
(defun my-avy-goto-char-timer-any-window ()
  "Go to character in any visible window."
  (interactive)
  (setq current-prefix-arg t)
  (call-interactively 'avy-goto-char-timer))
(defun my-avy-goto-line-any-window ()
  "Go to line in any visible window."
  (interactive)
  (setq current-prefix-arg 4)
  (call-interactively 'avy-goto-line))
(defun my-evil-end-of-next-line ()
  (interactive)
  (evil-next-line)
  (end-of-line))
(evilem-make-motion-plain
 my-avy-evil-goto-end-of-line
 (list 'evil-end-of-line 'my-evil-end-of-next-line)
 :pre-hook (setq evil-this-type 'line)
 :bind ((scroll-margin 0))
 :initial-point (goto-char (window-start)))
(evil-define-key 'motion global-map
  (kbd "SPC SPC") 'avy-goto-char-timer
  (kbd "SPC S-SPC") 'my-avy-goto-char-timer-any-window
  (kbd "S-SPC S-SPC") 'my-avy-goto-char-timer-any-window
  (kbd "SPC l") 'avy-goto-line
  (kbd "SPC L") 'my-avy-goto-line-any-window)
(evil-define-key 'normal global-map
  (kbd "SPC a") (lambda () (interactive) (avy-goto-char-timer) (call-interactively 'evil-append))
  (kbd "SPC A") (lambda () (interactive) (my-avy-evil-goto-end-of-line) (call-interactively 'evil-append-line))
  (kbd "SPC c") (lambda () (interactive) (avy-goto-line) (evil-first-non-blank) (call-interactively 'evil-change-line))
  (kbd "SPC C") 'my-avy-evil-change-region
  (kbd "SPC d") 'my-avy-evil-delete-line
  (kbd "SPC D") 'my-avy-evil-delete-region
  (kbd "SPC i") (lambda () (interactive) (avy-goto-char-timer) (call-interactively 'evil-insert))
  (kbd "SPC I") (lambda () (interactive) (avy-goto-line) (call-interactively 'evil-insert-line))
  (kbd "SPC o") (lambda () (interactive) (avy-goto-line) (call-interactively 'evil-open-below))
  (kbd "SPC O") (lambda () (interactive) (avy-goto-line) (call-interactively 'evil-open-above))
  (kbd "SPC p d") (lambda () (interactive) (next-line) (call-interactively 'avy-move-line))
  (kbd "SPC p D") (lambda () (interactive) (next-line) (call-interactively 'avy-move-region))
  (kbd "SPC P d") 'avy-move-line
  (kbd "SPC P D") 'avy-move-region
  (kbd "SPC p y") (lambda () (interactive) (next-line) (call-interactively 'avy-copy-line))
  (kbd "SPC p Y") (lambda () (interactive) (next-line) (call-interactively 'avy-copy-region))
  (kbd "SPC P y") 'avy-copy-line
  (kbd "SPC P Y") 'avy-copy-region
  (kbd "SPC $") 'my-avy-evil-goto-end-of-line)

;; evil-snipe. the t/T/f/F overrides are the most important ones,
;; since avy/evil-easymotion already allows for fancy jumps, e.g. via
;; avy-goto-char-timer.
(evil-snipe-mode 1)
(set-face-attribute 'evil-snipe-matches-face nil :inherit 'lazy-highlight)
(evil-define-key 'motion evil-snipe-mode-map
  "s" nil
  "S" nil)
(define-key evil-snipe-parent-transient-map (kbd "SPC")
  ;; Turn an active snipe into an avy/easy-motion overlay.
  (evilem-create (list 'evil-snipe-repeat
                       'evil-snipe-repeat-reverse)
                 :bind ((evil-snipe-scope 'visible)
                        (evil-snipe-enable-highlight)
                        (evil-snipe-enable-incremental-highlight))))

;; selection
(setq expand-region-fast-keys-enabled nil)
(defhydra hydra-expand-region ()
  "\nexpand-region  _TAB_ expand  _-_ contract  _r_eset"
  ("<escape>" (er/expand-region 0) nil :exit t)
  ("TAB" (er/expand-region 1) nil)
  ("-" (er/expand-region -1) nil)
  ("r" (er/expand-region 0) nil))
(evil-define-key 'visual global-map
  (kbd "TAB")
  (lambda ()
    (interactive)
    (er/expand-region 1)
    (hydra-expand-region/body)))


;; narrowing
(defun my-narrow-dwim ()
  "Narrow (or widen) to defun or region."
  (interactive)
  (if (region-active-p)
      (progn
        (narrow-to-region (region-beginning) (region-end))
        (deactivate-mark)
        (message "Showing region only"))
    (if (buffer-narrowed-p)
        (progn
          (widen)
          (message "Showing everything"))
      (narrow-to-defun)
      (message "Showing defun only"))))
(define-key my-leader-map "n" 'my-narrow-dwim)
(define-key my-visual-leader-map "n" 'my-narrow-dwim)


;;;
;;; toggles
;;;

(defhydra hydra-toggle (:exit t :foreign-keys warn)
  "\ntoggle  _b_ackgound  _c_olemak  _d_iff  _f_ill  _l_ine  _m_aximize  _n_umber  _o_utline  _r_elative-number  _t_runcate  _v_isual-line  _w_riteroom  _SPC_ whitespace  _1_ num/sym"
  ("<escape>" nil nil)
  ("b" my-toggle-dark-light-theme nil)
  ("B" my-set-theme-from-environment nil)
  ("c" evil-colemak-basics-mode nil)
  ("d" global-diff-hl-mode nil)
  ("f" auto-fill-mode nil)
  ("F" fci-mode nil)
  ("l" hl-line-mode nil)
  ("m" toggle-frame-maximized nil)
  ("M" toggle-frame-fullscreen nil)
  ("n" (progn (relative-line-numbers-mode -1) (linum-mode 'toggle)) nil)
  ("N" (progn (line-number-mode 'toggle) (column-number-mode 'toggle)) nil)
  ("o" outline-minor-mode nil)
  ("r" (progn (linum-mode -1) (relative-line-numbers-mode 'toggle)) nil)
  ("t" toggle-truncate-lines nil)
  ("v" visual-line-mode nil)
  ("SPC" whitespace-mode nil)
  ("S-SPC" my-toggle-show-trailing-whitespace nil)
  ("w" writeroom-mode nil)
  ("W" (progn (delete-other-windows) (writeroom-mode 'toggle)) nil)
  ("1" global-evil-swap-keys-mode nil)
  ("!" global-evil-swap-keys-mode nil))
(define-key my-leader-map "t" 'hydra-toggle/body)


;;;
;;; Zooming / text size
;;;

(require 'default-text-scale)  ;; functions below use non-autoloaded functions
(defvar my-default-text-scale-height
  (face-attribute 'default :height)  ;; inherited from environment configuration
  "The default text scale height.")
(if (<= my-default-text-scale-height 60)
    ;; when started as an emacs daemon process, the default face's
    ;; height attribute is bogus. use a sane default in that case.
    (setq my-default-text-scale-height 100))
(defun my-default-text-scale-set (height)
  (interactive "nHeight (e.g. 110) ")
  (default-text-scale-increment (- height (face-attribute 'default :height))))
(defun my-default-text-scale-reset ()
  (interactive)
  (my-default-text-scale-set my-default-text-scale-height))
(when (display-graphic-p)
  (my-default-text-scale-reset))
(defhydra hydra-zoom ()
  "\nzoom  _i_n  _o_ut  _z_ normal    writeroom  _n_arrower _w_ider _r_eset"
  ("<escape>" nil nil)
  ("i" default-text-scale-increase nil)
  ("o" default-text-scale-decrease nil)
  ("z" my-default-text-scale-reset nil :exit t)
  ("0" my-default-text-scale-reset nil :exit t)
  ("=" default-text-scale-increase nil)
  ("+" default-text-scale-increase nil)
  ("-" default-text-scale-decrease nil)
  ("n" (writeroom-decrease-width) nil)
  ("w" (writeroom-increase-width) nil)
  ("r" (writeroom-adjust-width nil) nil :exit t))
(define-key my-leader-map "z" 'hydra-zoom/body)


;;
;; Window layout
;;
;; Preferred layout is full-height windows, up to three next to each
;; other in a horizontal fashion, i.e. screen divided into columns.
;;

(setq
 help-window-select t
 split-height-threshold nil
 split-width-threshold 120
 split-window-preferred-function 'visual-fill-column-split-window-sensibly
 evil-split-window-below t
 evil-vsplit-window-right t
 writeroom-global-effects nil
 writeroom-maximize-window nil)
(advice-add 'delete-window :after '(lambda (&rest args) (balance-windows)))
(advice-add 'display-buffer :after '(lambda (&rest args) (balance-windows)))

(defun my-evil-window-next-or-vsplit ()
  "Focus next window, or vsplit if it is the only window in this frame."
  (interactive)
  (if (> (count-windows) 1)
      (evil-window-next nil)
    (evil-window-vsplit))
  (nav-flash-show))
(defun my-evil-goto-window (n)
  "Go to window N."
  (evil-window-top-left)
  (evil-window-next n)
  (nav-flash-show))
(defun my-evil-goto-window-1 ()
  "Go to the first window."
  (interactive)
  (my-evil-goto-window 1))
(defun my-evil-goto-window-2 ()
  "Go to the second window."
  (interactive)
  (my-evil-goto-window 2))
(defun my-evil-goto-window-3 ()
  "Go to the third window."
  (interactive)
  (my-evil-goto-window 3))
(defun my-evil-goto-window-4 ()
  "Go to the fourth window."
  (interactive)
  (my-evil-goto-window 4))

(evil-define-key 'motion global-map
  (kbd "C-1") 'my-evil-goto-window-1  ;; linux: control key
  (kbd "C-2") 'my-evil-goto-window-2
  (kbd "C-3") 'my-evil-goto-window-3
  (kbd "C-4") 'my-evil-goto-window-4
  (kbd "s-1") 'my-evil-goto-window-1  ;; osx: command key
  (kbd "s-2") 'my-evil-goto-window-2
  (kbd "s-3") 'my-evil-goto-window-3
  (kbd "s-4") 'my-evil-goto-window-4)
(define-key my-leader-map "1" 'my-evil-goto-window-1)
(define-key my-leader-map "2" 'my-evil-goto-window-2)
(define-key my-leader-map "3" 'my-evil-goto-window-3)
(define-key my-leader-map "4" 'my-evil-goto-window-4)

(defhydra my-hydra-window (:exit t :foreign-keys warn )
  "\nwindow  _h__n__e__i_ nav  _c_lose  _o_nly  _r_otate  _s_plit  _v_split  _w_ cycle"
  ("<escape>" nil nil)
  ("<return>" nil nil)
  ("h" evil-window-left nil)
  ("H" buf-move-left nil)
  ("n" evil-window-down nil)
  ("N" buf-move-down nil)
  ("e" evil-window-up nil)
  ("E" buf-move-up nil)
  ("i" evil-window-right nil)
  ("I" buf-move-right nil)
  ("c" evil-window-delete nil)
  ("o" delete-other-windows nil)
  ("r" evil-window-rotate-downwards nil :exit nil)
  ("R" evil-window-rotate-upwards nil :exit nil)
  ("s" evil-window-split nil)
  ("S" evil-window-new nil)
  ("v" evil-window-vsplit nil)
  ("V" evil-window-vnew nil)
  ("w" my-evil-window-next-or-vsplit nil)
  ("C-w" my-evil-window-next-or-vsplit nil)
  ("1" my-evil-goto-window-1 nil)
  ("2" my-evil-goto-window-2 nil)
  ("3" my-evil-goto-window-3 nil)
  ("4" my-evil-goto-window-4 nil)
  ("=" balance-windows nil))
(evil-define-key '(emacs motion) global-map
  (kbd "C-w") 'my-hydra-window/body)
(define-key my-leader-map "w" 'my-hydra-window/body)


;;
;; Projects
;;

(defun my-projectile-detect-test-file-name ()
  "Detect associated test file name for the current buffer."
  (or (if (projectile-test-file-p (buffer-file-name))
          (file-relative-name (buffer-file-name) (projectile-project-root)))
      (projectile-find-matching-test (buffer-file-name))))
(defun my-projectile-switch-open-project-buffer ()
  "Switch to a buffer of an open project."
  (interactive)
  (let ((projectile-switch-project-action 'projectile-switch-to-buffer))
    (projectile-switch-open-project)))
(setq
 projectile-ignored-projects '("/usr/local/")
 projectile-mode-line nil
 projectile-require-project-root nil)
(projectile-global-mode)
(defhydra hydra-project (:exit t :foreign-keys warn)
  "\nproject  _b_uffer  _d_ir  _f_ile  _k_ill  _p_roject  _s_ave  _t_est/impl  _-_ top dir"
  ("<escape>" nil nil)
  ("b" projectile-switch-to-buffer nil)
  ("B" projectile-switch-to-buffer-other-window nil)
  ("-" projectile-dired nil)
  ("d" projectile-find-dir nil)
  ("D" projectile-find-dir-other-window nil)
  ("f" projectile-find-file nil)
  ("F" projectile-find-file-other-window nil)
  ("k" projectile-kill-buffers nil)
  ("p" my-projectile-switch-open-project-buffer nil)
  ("P" projectile-switch-project nil)
  ("s" projectile-save-project-buffers nil)
  ;; Use the "other window" variant for the lowercase version for
  ;; switching between test and implementation since that is generally
  ;; more useful.
  ("t" projectile-find-implementation-or-test-other-window nil)
  ("T" projectile-toggle-between-implementation-and-test nil))
(define-key my-leader-map "p" 'hydra-project/body)


;;;
;;; Version control
;;;

(with-eval-after-load 'magit
  (require 'magithub))

(setq
 auto-revert-check-vc-info t
 magit-branch-prefer-remote-upstream '("master")
 magit-branch-read-upstream-first nil
 magit-prefer-remote-upstream t
 magit-process-popup-time 10
 magit-fetch-arguments '("--prune")
 magit-log-arguments '("--graph" "--color" "--decorate" "--follow" "-n256")
 magit-rebase-arguments '("--autostash")
 magit-tag-arguments '("--annotate"))

;; colemak tweaks
(evil-define-key '(normal visual) magit-mode-map
  (kbd "n") 'evil-next-visual-line
  (kbd "e") 'evil-previous-visual-line)
(defun my-disable-colemak ()
  "Disable colemak overrides."
  (evil-colemak-basics-mode -1))
(dolist (hook '(magit-status-mode-hook magit-log-mode-hook))
  (add-hook hook #'my-disable-colemak))

;; hl-diff
(global-diff-hl-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; Pop-ups sometimes contain trailing whitespace.
(add-hook 'magit-popup-mode-hook 'my-hide-trailing-whitespace)

;; Magit shortcuts
(defhydra hydra-git (:exit t :foreign-keys warn)
  "\ngit  _b_lame  _c_ommit  _d_iff  _f_ile  _g_ popup  _l_og  _r_efs  _s_tatus  _t_ lock  _w_eb  _!_ command"
  ("<escape>" nil nil)
  ("!" magit-git-command nil)
  ("b" magit-blame nil)
  ("B" magit-log-buffer-file nil)
  ("c" magit-commit nil)
  ("d" magit-diff nil)
  ("f" counsel-git nil)
  ("g" magit-dispatch-popup nil)
  ("l" magit-log-current nil)
  ("L" magit-log-all nil)
  ("r" magit-show-refs-popup nil)
  ("s" magit-status nil)
  ("S"
   (lambda ()
     "Open git status for another repository."
     (interactive)
     (setq current-prefix-arg t)
     (call-interactively 'magit-status))
   nil)
  ("t" magit-toggle-buffer-lock nil)
  ("w"
   (lambda ()
     "Browse repository on the web; invokes hub."
     (interactive)
     (shell-command "hub browse"))
   nil)
  ("W"
   (lambda ()
     "Compare repository on the web; invokes hub."
     (interactive)
     (shell-command "hub compare"))
   nil))
(define-key my-leader-map "g" 'hydra-git/body)

;; smerge
(defhydra hydra-smerge (:exit t :foreign-keys warn)
  "\nsmerge  _c_urrent  _m_ine  _o_ther  _b_ase  _a_ll      go to  _j_ next  _k_ previous"
  ("<escape>" nil nil)
  ("c" smerge-keep-current nil)
  ("m" smerge-keep-mine nil)
  ("b" smerge-keep-base nil)
  ("o" smerge-keep-other nil)
  ("a" smerge-keep-all nil)
  ("j" 'smerge-next nil :exit nil)
  ("k" 'smerge-prev nil :exit nil))
(define-key my-leader-map "m" 'hydra-smerge/body)


;;;
;;; Search
;;;

(defun my-thing-at-point-dwim ()
  "Return the symbol at point, or the region contents if activated."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'symbol t)))
;; isearch
(setq isearch-allow-prefix nil)

;; swiper
(defun my-swiper-thing-at-point ()
  "Start `swiper` searching for the thing at point."
  (interactive)
  (let ((query (my-thing-at-point-dwim)))
    (evil-force-normal-state)  ; do not expand region in visual mode
    (swiper query)))
(define-key my-leader-map "/" 'swiper)
(define-key my-visual-leader-map "/" 'my-swiper-thing-at-point)
(evil-define-key 'motion global-map
  "/" 'swiper
  "g/" 'evil-search-forward)
(define-key my-leader-map "/" 'evil-search-forward)

;; ag, the silver searcher
(setq ag-reuse-buffers t)
(defhydra hydra-ag (:exit t :foreign-keys warn)
  "\nag  _g_ project  _f_iles  _r_egex"
  ("<escape>" nil nil)
  ("a" ag-project nil)
  ("f" ag-project-files nil)
  ("F" ag-files nil)
  ("g" ag-project nil)
  ("G" ag nil)
  ("r" ag-project-regexp nil)
  ("R" ag-regexp nil))
(define-key my-leader-map "a" 'hydra-ag/body)
(add-hook
 'ag-mode-hook
 (lambda ()
   (toggle-truncate-lines t)))

;; swiper style search using ag; uses shift-/, since it's conceptually
;; an alternative to swiper.
(defun my-counsel-ag-project ()
  "Run counsel-ag on the current project, defaulting to the symbol at point."
  (interactive)
  (counsel-ag
   (my-thing-at-point-dwim)
   (projectile-project-root)))
(define-key my-leader-map "?" 'my-counsel-ag-project)

;; symbol highlighting
(setq
 highlight-symbol-idle-delay 1.0
 highlight-symbol-on-navigation-p t)
(define-key my-leader-map "h" 'highlight-symbol)
(define-key my-leader-map "H" 'highlight-symbol-remove-all)
(define-key my-leader-map "r" 'highlight-symbol-query-replace)
(define-key my-visual-leader-map "h"
  (lambda (start end) (interactive "r")
    (highlight-symbol-add-symbol
     (buffer-substring-no-properties start end))))

;; occur
(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))
(define-key my-leader-map "o" 'occur-dwim)


;;;
;;; Completion
;;;

;; ivy, counsel
(setq
 ivy-count-format "(%d/%d) "
 ivy-initial-inputs-alist nil
 ivy-wrap t
 magit-completing-read-function 'ivy-completing-read
 projectile-completion-system 'ivy)
(ivy-mode 1)
(counsel-mode 1)
(define-key my-leader-map "," 'ivy-resume)

;; ivy-rich integration
(ivy-set-display-transformer
 'ivy-switch-buffer
 'ivy-rich-switch-buffer-transformer)
(ivy-set-display-transformer
 'ivy-switch-buffer-other-window
 'ivy-rich-switch-buffer-transformer)

;; company
(require 'company)
(setq
 company-auto-complete 'company-explicit-action-p
 company-dabbrev-downcase nil
 company-dabbrev-ignore-case t
 company-idle-delay nil
 company-occurrence-weight-function 'company-occurrence-prefer-any-closest
 company-require-match nil
 company-selection-wrap-around t
 company-transformers '(company-sort-by-occurrence)
 evil-complete-next-func (lambda (arg) (company-manual-begin))
 evil-complete-previous-func (lambda (arg) (call-interactively 'company-dabbrev)))
(add-to-list 'company-auto-complete-chars ?\( )
(add-to-list 'company-backends 'company-files)
(add-hook 'after-init-hook 'global-company-mode)
(evil-define-key 'insert global-map
  (kbd "C-<return>") 'company-manual-begin
  (kbd "C-n") 'company-manual-begin)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-<return>") 'company-select-next)


;;;
;;; Minibuffer
;;;

(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)
(define-key minibuffer-local-map (kbd "C-u") 'kill-whole-line)
(define-key ivy-minibuffer-map (kbd "C-w") 'ivy-backward-kill-word)
(define-key ivy-minibuffer-map (kbd "C-u") 'kill-whole-line)
(add-hook 'minibuffer-inactive-mode-hook 'my-hide-trailing-whitespace)


;;;
;;; Programming
;;;

;; Flycheck
(setq
 flycheck-checker-error-threshold 1000
 flycheck-display-errors-delay 1.0)
(global-flycheck-mode)

;; Compilation
(add-hook 'compilation-mode-hook 'my-hide-trailing-whitespace)
(add-hook 'comint-mode-hook 'my-hide-trailing-whitespace)


;;;
;;; Major modes
;;;

(setq-default major-mode 'text-mode)

;; text editing
(setq-default typo-language "prefer-single")
(with-eval-after-load 'typo
  (add-to-list 'typo-quotation-marks '("prefer-single" "‘" "’" "“" "”")))
(defun my-text-mode-hook ()
  (auto-fill-mode)
  (evil-swap-keys-swap-question-mark-slash)
  (typo-mode)
  (visual-line-mode))
(add-hook 'text-mode-hook 'my-text-mode-hook)

;; programming languages
(setq
 fic-highlighted-words
 '("FIXME" "fixme"
   "TODO" "todo"
   "BUG" "bug"
   "XXX" "xxx"))
(defun my-prog-mode-hook ()
  (abbrev-mode)
  (evil-swap-keys-swap-number-row)
  (auto-fill-mode)
  (column-number-mode)
  (fic-mode)
  (highlight-parentheses-mode)
  (highlight-symbol-mode))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

;; compilation
(evil-define-key 'motion compilation-mode-map
  "gr" 'recompile)

;; emacs lisp
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (setq evil-shift-width 2)
   ;; (evil-cleverparens-mode)  ;; FIXME: useless with colemak
   (rainbow-delimiters-mode)))
(evil-define-key 'motion emacs-lisp-mode-map (kbd "RET")
  (defhydra hydra-emacs-lisp (:exit t :foreign-keys warn)
    "\nelisp  _e_val"
    ("RET" nil nil)
    ("<escape>" nil nil)
    ("e" eval-last-sexp nil)))

;; jinja
(add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode))

;; json
(defun my-json-mode-hook ()
  (setq
   tab-width 2
   json-reformat:indent-width tab-width
   evil-shift-width tab-width)
  (evil-swap-keys-swap-colon-semicolon)
  (evil-swap-keys-swap-double-single-quotes))
(add-hook 'json-mode-hook #'my-json-mode-hook)

;; latex
(setq TeX-engine 'xetex)

;; python
(defun my-python-mode-hook ()
  (setq
   fill-column 72
   python-fill-docstring-style 'symmetric)
  (evil-swap-keys-swap-colon-semicolon)
  (evil-swap-keys-swap-underscore-dash)
  (outline-minor-mode)
  (python-docstring-mode))
(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'comint-output-filter-functions 'python-pdbtrack-comint-output-filter-function)
(dolist (open '("(" "{" "["))
  (sp-local-pair
   'python-mode open nil
   :unless '(sp-point-before-word-p)))
(defvar my-python-pytest-arguments-history nil
  "Argument history for pytest invocations.")
(defun my-python-pytest (&optional arguments)
  "Run pytest with specified ARGUMENTS."
  (interactive
   (list
    (read-string
     "pytest arguments: "
     (or
      (let ((test-file-name (my-projectile-detect-test-file-name)))
        (if test-file-name (format "-k %s " test-file-name)))
      (first my-python-pytest-arguments-history))
     'my-python-pytest-arguments-history)))
  (let ((default-directory (projectile-project-root)))
    (compile (format "py.test %s" arguments) t)))
(defun my-python-insert-pdb-trace (mod)
  "Insert a pdb trace statement using MOD right before the current statement."
  (python-nav-beginning-of-statement)
  (insert-before-markers
   (format
    "import %s; %s.set_trace()  # FIXME\n%s"
    mod mod
    (buffer-substring-no-properties  ;; copy indentation
     (line-beginning-position) (point))))
  (forward-line -1)
  (beginning-of-line-text))
(evilem-make-motion
 my-easymotion-python
 (list
  ;; Collect interesting positions around point, and all visible
  ;; blocks in the window. Results are ordered: forward after point,
  ;; then backward from point.
  'python-nav-end-of-statement 'python-nav-end-of-block 'python-nav-forward-block
  'python-nav-beginning-of-statement 'python-nav-beginning-of-block 'python-nav-backward-block)
 :pre-hook (setq evil-this-type 'line))
(defun my-swiper-python-definitions ()
  (interactive)
  (swiper "^\\s-*\\(def\\|class\\)\\s- "))
(evil-define-operator my-evil-join-python (beg end)
  "Like 'evil-join', but handles comments sensibly."
  :motion evil-line
  (evil-join beg end)
  (let ((first-line-is-comment (save-excursion
                                 (evil-first-non-blank)
                                 (looking-at-p "#")))
        (joined-line-is-comment (looking-at " #")))
    (when joined-line-is-comment
      (if first-line-is-comment
          (delete-region (point) (match-end 0))
        (insert " ")
        (forward-char)))))
(defun my-evil-forward-char-or-python-statement (count)
  "Intelligently pick a statement or a character."
  (interactive "p")
  (cond
   ((eq this-command 'evil-change)
    (evil-text-object-python-inner-statement count))
   ((memq this-command '(evil-delete evil-shift-left evil-shift-right))
    (evil-text-object-python-outer-statement count))
   (t (evil-forward-char count))))
(defun my-python-refactor-make-variable (beg end)
  "Refactor the current region into a named variable."
  (interactive "r")
  (let ((name (read-string "Variable name: "))
        (code (delete-and-extract-region beg end)))
    (insert name)
    (python-nav-beginning-of-statement)
    (insert-before-markers
     (format "%s = %s\n%s" name code
      (buffer-substring-no-properties (line-beginning-position) (point))))
    (forward-line -1)
    (beginning-of-line-text)))
(defhydra hydra-python (:exit t :foreign-keys warn)
  "\npython  _b_ pdb trace  _l_  multi-line  _t_ pytest  _v_ariable"
  ("RET" nil nil)
  ("<escape>" nil nil)
  ("b" (my-python-insert-pdb-trace "pdb") nil)
  ("B" (my-python-insert-pdb-trace "ipdb") nil)
  ("t" my-python-pytest nil)
  ("T" (my-python-pytest "") nil)
  ("l" multi-line nil :exit nil)
  ("L" multi-line-single-line nil)
  ("v" my-python-refactor-make-variable nil))
(evil-define-key 'motion python-mode-map
  (kbd "SPC /") 'my-swiper-python-definitions
  (kbd "SPC TAB") 'my-easymotion-python
  (kbd "RET") 'hydra-python/body)
(evil-define-key 'normal python-mode-map
  [remap evil-join] 'my-evil-join-python)
(evil-define-key 'insert python-mode-map
  (kbd "C-l") 'multi-line)
(evil-define-key 'operator python-mode-map
  "l" 'my-evil-forward-char-or-python-statement)
(evil-define-key '(operator visual) python-mode-map
  "H" 'python-nav-backward-sexp-safe
  ;; "L" 'python-nav-forward-sexp-safe  ;; qwerty
  "I" 'python-nav-forward-sexp-safe
  "ul" 'evil-text-object-python-inner-statement
  "al" 'evil-text-object-python-outer-statement)

;; reStructuredText
(setq
 rst-default-indent 0
 rst-indent-comment 2
 rst-indent-field 2
 rst-indent-literal-normal 2
 rst-preferred-adornments '((?= over-and-under 0)
                            (?= simple 0)
                            (?- simple 0)
                            (?~ simple 0)
                            (?+ simple 0)
                            (?` simple 0)
                            (?# simple 0)
                            (?@ simple 0))
 rst-preferred-bullets '(?- ?*))
(defun my-rst-mode-hook ()
  (setq evil-shift-width 2)
  (modify-syntax-entry ?_ "w")
  (evil-swap-keys-swap-double-single-quotes)
  (make-variable-buffer-local 'typo-mode-map)
  (define-key typo-mode-map "`" nil)
  (add-to-list 'electric-pair-pairs '(?_ . ?_)))
(add-hook 'rst-mode-hook 'my-rst-mode-hook)
(evilem-make-motion
 my-easymotion-rst
 (list 'rst-forward-section 'rst-backward-section)
 :pre-hook (setq evil-this-type 'line))
(evil-define-key 'motion rst-mode-map
  (kbd "SPC TAB") 'my-easymotion-rst)
(evil-define-key 'normal rst-mode-map (kbd "RET")
  (defhydra hydra-rst (:exit t :foreign-keys warn)
    "\nrestructuredtext  _a_djust"
    ("RET" nil nil)
    ("<escape>" nil nil)
    ("a" rst-adjust nil)))

;; Shell
(defun my-sh-mode-hook ()
  (evil-swap-keys-swap-pipe-backslash))
(add-hook 'sh-mode-hook 'my-sh-mode-hook)
(add-to-list 'auto-mode-alist '("bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc-.*\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.envrc\\'" . sh-mode))

;; Yaml
(defun my-yaml-mode-hook ()
  (setq evil-shift-width yaml-indent-offset))
(add-hook 'yaml-mode-hook 'my-yaml-mode-hook)


;;;
;;; Local configuration (not in version control)
;;;

(load "~/.emacs.d/init-local" t)


(provide 'init)
;;; init.el ends here
