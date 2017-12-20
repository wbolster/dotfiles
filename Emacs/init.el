;;; init.el --- emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs configuration

;;; Code:

;;;; bootstrap

(require 'package)
(setq
 package-archives '(("melpa" . "https://melpa.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/")
                    ("gnu" . "https://elpa.gnu.org/packages/"))
 package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t)
(require 'use-package)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(use-package auto-compile
  :custom
  (auto-compile-update-autoloads t)
  :config
  (auto-compile-on-load-mode))

(use-package benchmark-init
  :config
  (benchmark-init/activate)
  (add-hook 'after-init-hook 'benchmark-init/deactivate))


;;;; helper libraries

(use-package dash
  :config
  (dash-enable-font-lock))

(use-package dash-functional)

(use-package fn)

(use-package general)

(use-package s)


;;;; basics

(use-package no-littering)

(use-package tls
  :custom
  (tls-checktrust 'ask))

(fset 'yes-or-no-p 'y-or-n-p)

(setq
 disabled-command-function nil
 echo-keystrokes 0.5
 inhibit-startup-screen t
 initial-scratch-message nil)

;; no gnu/agitprop kthxbye
(defun display-startup-echo-area-message ()
  "Do not display progaganda."
  ;; the 'inhibit-startup-echo-area-message' variable
  ;; requires hard-coding a user name for it to work.
  ;; annoying. resist the gnu/extremists by turning
  ;; the propaganda function into a no-op.
  (message ""))
(general-define-key  ;; unbind useless shortcuts to gpl etc.
 :prefix "C-h"
 "g" nil
 "C-c" nil
 "C-m" nil
 "C-o" nil
 "C-w" nil)


;;;; environment

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(use-package direnv
  :custom
  (direnv-always-show-summary t)
  (direnv-show-paths-in-summary nil)
  :config
  (direnv-mode))

;; os-x specific
(when (eq system-type 'darwin)
  (general-define-key "s-q" nil)
  (setq
   ns-right-alternate-modifier 'none
   ns-use-native-fullscreen nil))


;;;; server

(use-package server
  :if window-system
  :unless (server-running-p)
  :config
  (server-start))

(use-package edit-server
  ;; this is used by the ‘edit with emacs’ chrome extension:
  ;; https://chrome.google.com/webstore/detail/edit-with-emacs/ljobjlafonikaiipfkggjbhkghgicgoh
  :if window-system
  :config
  (edit-server-start)
  (add-to-list
   'edit-server-url-major-mode-alist
   '("github\\.com" . markdown-mode)))


;;;; hydra

(use-package hydra)

(defvar w--hydra-hint-delay 1
  "Delay before showing help.")

;; fixme: maybe use a registry pattern with an alist that maps major
;; modes (and derived modes) to a hydra, instead of buffer-local variables?
(defvar w--major-mode-hydra nil
  "Hydra body for the current major mode.")
(make-variable-buffer-local 'w--major-mode-hydra)

(defun w--set-major-mode-hydra (hydra-body)
  "Set the buffer-local major-mode specific hydra to HYDRA-BODY."
  (setq w--major-mode-hydra hydra-body))

(defun w--major-mode-hydra ()
  "Run major mode hydra, if any."
  (interactive)
  (if w--major-mode-hydra
      (call-interactively w--major-mode-hydra)
    (user-error "No major-mode specific hydra defined for %s" major-mode)))

(defun w--hydra-evil-repeat-record-command ()
  "Record last command from the hydra in evil's repeat system."
  (evil-repeat-start)
  (setq evil-repeat-info `((call-interactively ,real-this-command)))
  (evil-repeat-stop))

(defun w--hydra-make-docstring (args)
  "Make a docstring for a hydra from ARGS."
  (setq args (--map-when (not (string-match-p "_" it))
                         (format "  %s:" it)
                         args))
  (s-concat "\n" (s-trim (s-join "  " args))))

(defun w--hydra-set-defaults (body)
  "Add defaults to a hydra BODY list."
  (unless (plist-member body :exit)
    (setq body (plist-put body :exit t)))
  (unless (plist-member body :hint)
    (setq body (plist-put body :hint nil)))
  (unless (plist-member body :foreign-keys)
    (setq body (plist-put body :foreign-keys 'warn)))
  (setq body (plist-put body :idle w--hydra-hint-delay))
  body)

(defun w--hydra-missing-uppercase-heads (heads)
  "Return missing uppercase hydra heads.

This creates uppercase versions for all lowercase HEADS that are only
defined as lowercase."
  (let* ((case-fold-search nil)
         (uppercase-keys
          (--filter (s-matches-p "^[A-Z]$" it) (-map #'car heads))))
    (--map
     (-replace-at 0 (upcase (car it)) it)
     (--filter
      (and (s-matches? "^[a-z]$" (car it))
           (not (-contains-p uppercase-keys (upcase (car it)))))
      heads))))

(defmacro w--make-hydra (name body &rest args)
  "Make a hydra NAME with BODY, using ARGS for heads and docstrings."
  (declare (indent 2))
  (-let [(docstrings heads) (-separate #'stringp args)]
    `(defhydra
       ,name
       ,(w--hydra-set-defaults body)
       ,(w--hydra-make-docstring docstrings)
       ,@(w--hydra-missing-uppercase-heads heads)
       ,@heads
       ("<escape>" nil :exit t))))


;;;; buffers, files, directories

(setq
 create-lockfiles nil
 find-file-visit-truename t
 make-backup-files nil)
(setq
 auto-save-file-name-transforms
 `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq backup-directory-alist
      `((".*" ,(no-littering-expand-var-file-name "backup/") t)))

(use-package desktop
  :config
  (desktop-save-mode)
  :custom
  (desktop-restore-eager 5))

(defvar
  w--recentf-ignore-dirs
  (list
   no-littering-etc-directory
   no-littering-var-directory)
  "Directories to ignore in recentf listings.")

(use-package recentf
  :custom
  (recentf-auto-cleanup 300)
  (recentf-max-saved-items 500)
  :config
  (dolist (dir w--recentf-ignore-dirs)
    (add-to-list 'recentf-exclude (concat (regexp-quote dir) ".*")))
  (recentf-mode))

(use-package sudo-edit
  :defer t)

(use-package terminal-here
  :defer t
  :custom
  (terminal-here-project-root-function 'projectile-project-root))

(defun w--buffer-worth-saving-p (name)
  "Does the buffer NAME indicate it may be worth saving?"
  (cond
   ((string-equal "*scratch*" name) t)
   ((string-prefix-p "*new*" name) t)  ;; evil-mode template
   ((string-match-p "\*" name) nil) ;; e.g. magit, help
   ((string-match-p "^ " name) nil) ;; hidden buffers
   (t t)))

(defun w--ask-confirmation-for-unsaved-buffers ()
  "Ask for confirmation for modified but unsaved buffers."
  (if (and (buffer-modified-p)
           (not (buffer-file-name))
           (w--buffer-worth-saving-p (buffer-name)))
      (y-or-n-p
       (format
        "Buffer %s modified but not saved; kill anyway? "
        (buffer-name)))
    t))

(add-hook
 'kill-buffer-query-functions
 #'w--ask-confirmation-for-unsaved-buffers)

(defun w--evil-buffer-new-other-window ()
  "Open a new window in another window."
  (interactive)
  (w--evil-window-next-or-vsplit)
  (call-interactively #'evil-buffer-new))

(defun w--counsel-recentf-other-window ()
  "Like `w--counsel-recentf', but opens the file in another window."
  (interactive)
  (let ((ivy-inhibit-action t))
    (find-file-other-window (counsel-recentf))))

(w--make-hydra w--hydra-buffer nil
  "buffer"
  "_b_uffer"
  ("b" ivy-switch-buffer)
  ("B" ivy-switch-buffer-other-window)
  "_h_ide"
  ("h" bury-buffer)
  ("H" unbury-buffer)
  "_k_ill"
  ("k" kill-this-buffer)
  ("K" kill-buffer-and-window)
  "_m_ajor mode"
  ("m" w--switch-major-mode)
  "_n_ew"
  ("n" evil-buffer-new)
  ("N" w--evil-buffer-new-other-window)
  "_r_evert"
  ("r" revert-buffer))

(w--make-hydra w--hydra-find-file nil
  "open"
  "_d_irectory"
  ("d" dired-jump)
  ("D" dired-jump-other-window)
  "_f_ile"
  ("f" counsel-find-file)
  ("F" find-file-other-window)
  "_n_ew"
  ("n" evil-buffer-new)
  ("N" w--evil-buffer-new-other-window)
  "_r_ecent"
  ("r" counsel-recentf)
  ("R" w--counsel-recentf-other-window)
  "_s_udo"
  ("s" sudo-edit)
  ("S" (sudo-edit t))
  "_!_ terminal"
  ("!" terminal-here)
  ("1" terminal-here))


;;;; theme

(use-package solarized-theme
  :custom
  (solarized-emphasize-indicators nil)
  (solarized-scale-org-headlines nil)
  (solarized-use-less-bold t)
  (solarized-use-variable-pitch nil)
  (solarized-height-minus-1 1.0)
  (solarized-height-plus-1 1.0)
  (solarized-height-plus-2 1.0)
  (solarized-height-plus-3 1.0)
  (solarized-height-plus-4 1.0))

(defvar w--dark-theme 'solarized-dark "The preferred dark theme.")
(defvar w--light-theme 'solarized-light "The preferred light theme.")

(load-theme w--dark-theme t t)
(load-theme w--light-theme t t)

(defun w--toggle-dark-light-theme ()
  "Toggle between a dark and light theme."
  (interactive)
  (w--activate-theme (eq (first custom-enabled-themes) w--light-theme)))

(defun w--activate-theme (dark)
  "Load configured theme. When DARK is nil, load a light theme."
  (setq frame-background-mode (if dark 'dark 'light))
  (mapc 'frame-set-background-mode (frame-list))
  (let ((theme (if dark w--dark-theme w--light-theme)))
    (enable-theme theme))
  (w--tweak-faces))

(defun w--disable-themes-advice (theme)
  "Disable all enabled themes except THEME."
  (unless (eq theme 'user)
    (--each custom-enabled-themes
      (disable-theme it))))

(advice-add 'enable-theme :before #'w--disable-themes-advice)

(defun w--tweak-faces ()
  "Tweak some font faces."
  (set-face-attribute  ;; less contrasting region (evil visual state)
   'region nil
   :background nil :foreground nil
   :inherit 'secondary-selection))

(defun w--set-theme-from-environment ()
  "Set the theme based on presence/absence of a configuration file."
  (interactive)
  (w--activate-theme (file-exists-p "~/.config/dark-theme")))

(w--set-theme-from-environment)

(setq
 solarized-color-yellow  "#b58900"
 solarized-color-orange  "#cb4b16"
 solarized-color-red     "#dc322f"
 solarized-color-magenta "#d33682"
 solarized-color-violet  "#6c71c4"
 solarized-color-blue    "#268bd2"
 solarized-color-cyan    "#2aa198"
 solarized-color-green   "#859900")

(setq
 evil-normal-state-cursor (list solarized-color-yellow 'box)
 evil-visual-state-cursor (list solarized-color-orange 'hollow)
 evil-insert-state-cursor  (list solarized-color-yellow 'bar)
 evil-replace-state-cursor (list solarized-color-red 'hbar)
 evil-operator-state-cursor (list solarized-color-magenta 'hollow))


;;;; fonts

(use-package default-text-scale
  :demand t
  :general
  (:states 'motion
   "C-0" 'w--default-text-scale-reset
   "C--" 'default-text-scale-decrease
   "C-=" 'default-text-scale-increase)
  :config
  (when (display-graphic-p)
    (add-hook 'after-init-hook #'w--default-text-scale-reset)))

(defvar w--default-text-scale-height
  (face-attribute 'default :height)  ;; inherit from startup environment
  "The default text scale height.")

(if (<= w--default-text-scale-height 60)
    ;; when started as an emacs daemon process, the default face's
    ;; height attribute is bogus. use a sane default in that case.
    (setq w--default-text-scale-height 100))

(defun w--default-text-scale-reset ()
  "Reset default text scale."
  (interactive)
  (w--default-text-scale-set w--default-text-scale-height))

(defun w--default-text-scale-set (height)
  "Set default text scale to HEIGHT."
  (interactive "nHeight (e.g. 110) ")
  (default-text-scale-increment (- height (face-attribute 'default :height))))

(defvar w--faces-bold '(magit-popup-argument)
  "Faces that may retain their bold appearance.")

(defun w--make-faces-boring ()
  "Remove unwanted attributes from font faces."
  (interactive)
  (dolist (face (face-list))
    (set-face-attribute face nil :underline nil)
    (unless (member face w--faces-bold)
      (set-face-attribute face nil :weight 'normal))))

(w--make-faces-boring)

(advice-add
 'load-theme
 :after (fn: w--make-faces-boring))

(w--make-hydra w--hydra-zoom nil
  "zoom"
  "_i_n"
  ("i" default-text-scale-increase :exit nil)
  ("+" default-text-scale-increase :exit nil)
  ("=" default-text-scale-increase :exit nil)
  "_o_ut"
  ("o" default-text-scale-decrease :exit nil)
  ("-" default-text-scale-decrease :exit nil)
  "_z_ normal"
  ("z" w--default-text-scale-reset)
  ("0" w--default-text-scale-reset)
  "writeroom"
  "_n_arrower"
  ("n" w--writeroom-narrower :exit nil)
  "_w_ider"
  ("w" w--writeroom-wider :exit nil)
  "_r_eset"
  ("r" w--writeroom-reset))


;;;; mode line

(use-package delight)

(use-package powerline
  :config
  (powerline-center-evil-theme)
  :custom
  (powerline-default-separator nil))


;;;; evil

(use-package evil
  :demand t
  :init
  (setq
   evil-want-C-u-scroll t
   evil-want-C-w-in-emacs-state t)
  :custom
  (evil-insert-state-message nil)
  (evil-cross-lines t)
  :config
  (evil-mode)
  ;; use Y to copy to the end of the line; see evil-want-Y-yank-to-eol
  (evil-add-command-properties 'evil-yank-line :motion 'evil-end-of-line)
  :general
  (:states '(motion normal)
   [escape] #'w--evil-force-normal-state)
  (:states 'insert
   (general-chord "qw") #'evil-normal-state
   (general-chord "qq") #'evil-normal-state
   (general-chord "wq") #'evil-normal-state))

(defun w--evil-force-normal-state ()
  "Like `evil-force-normal-state', with some extra cleanups."
  (interactive)
  (lazy-highlight-cleanup t)
  (remove-overlays nil nil 'category 'evil-snipe)
  (evil-force-normal-state))

(use-package key-chord
  :config
  (key-chord-mode +1))

(use-package undo-tree
  :delight)

(use-package evil-colemak-basics
  :after evil-snipe
  :init
  (setq evil-colemak-basics-char-jump-commands 'evil-snipe)
  :config
  (global-evil-colemak-basics-mode)
  :delight)

(defun w--disable-colemak ()
  "Disable colemak overrides."
  (evil-colemak-basics-mode -1))

(use-package evil-snipe
  ;; evil-colemak-basics takes care of the basic key bindings.
  :custom
  (evil-snipe-override-evil-repeat-keys nil)
  (evil-snipe-scope 'line)
  (evil-snipe-repeat-scope 'line)
  (evil-snipe-smart-case t)
  (evil-snipe-tab-increment t)
  :custom-face
  (evil-snipe-matches-face ((t (:inherit lazy-highlight)))))

(use-package evil-swap-keys
  :config
  (global-evil-swap-keys-mode)
  :delight)

(use-package evil-commentary
  :config
  (evil-commentary-mode)
  :delight)

(use-package evil-easymotion
  :config
  (defun w--avy-evil-change-region ()
    "Select two lines and change the lines between them."
    (interactive)
    (avy-with w--avy-evil-change-region
      (let* ((beg (progn (avy-goto-line) (point)))
             (end (save-excursion (goto-char (avy--line)) (forward-line) (point))))
        (evil-change beg end 'line nil nil))))

  (defun w--avy-evil-delete-line ()
    "Select a line and delete it."
    (interactive)
    (avy-with w--avy-evil-delete-line
      (save-excursion
        (let ((line (avy--line)))
          (unless (eq line t)
            (goto-char line)
            (evil-delete-whole-line
             (point)
             (line-beginning-position 2)
             'line nil nil))))))

  (defun w--avy-evil-delete-region ()
    "Select two lines and delete the lines between them."
    (interactive)
    (avy-with w--avy-evil-delete-region
      (let* ((beg (avy--line))
             (end (save-excursion (goto-char (avy--line)) (forward-line) (point))))
        (evil-delete beg end 'line nil nil))))

  (defun w--avy-goto-line-any-window ()
    "Go to line in any visible window."
    (interactive)
    (setq current-prefix-arg 4)
    (call-interactively 'avy-goto-line))

  (defun w--evil-end-of-next-line ()
    (interactive)
    (evil-next-line)
    (end-of-line))

  (evilem-make-motion-plain
   w--avy-evil-goto-end-of-line
   (list 'evil-end-of-line 'w--evil-end-of-next-line)
   :pre-hook (setq evil-this-type 'line)
   :bind ((scroll-margin 0))
   :initial-point (goto-char (window-start)))

  ;; todo: all of this could use some rethinking and cleaning up
  ;; (evil-define-key* 'normal global-map
  ;;   (kbd "SPC a") (lambda () (interactive) (avy-goto-char-timer) (call-interactively 'evil-append))
  ;;   (kbd "SPC A") (lambda () (interactive) (w--avy-evil-goto-end-of-line) (call-interactively 'evil-append-line))
  ;;   (kbd "SPC c") (lambda () (interactive) (avy-goto-line) (evil-first-non-blank) (call-interactively 'evil-change-line))
  ;;   (kbd "SPC C") 'w--avy-evil-change-region
  ;;   (kbd "SPC i") (lambda () (interactive) (avy-goto-char-timer) (call-interactively 'evil-insert))
  ;;   (kbd "SPC I") (lambda () (interactive) (avy-goto-line) (call-interactively 'evil-insert-line))
  ;;   (kbd "SPC $") 'w--avy-evil-goto-end-of-line)

  (evilem-default-keybindings "C-M-S-s-<f12>")  ;; fixme: for side effects only
  (w--make-hydra w--hydra-teleport nil
    "teleport"
    "_w_,_f_,_b_,_gf_ word"
    ("w" evilem--motion-function-evil-forward-word-begin)
    ("W" evilem--motion-function-evil-forward-WORD-begin)
    ("f" evilem--motion-function-evil-forward-word-end)
    ("F" evilem--motion-function-evil-forward-WORD-end)
    ("b" evilem--motion-function-evil-backward-word-begin)
    ("B" evilem--motion-function-evil-backward-WORD-begin)
    ("gf" evilem--motion-function-evil-backward-word-end)
    ("gF" evilem--motion-function-evil-backward-WORD-end)
    "_n_,_e_,_l_ line"
    ("e" evilem--motion-function-previous-line)
    ("E" avy-goto-line-above)
    ("n" evilem--motion-function-next-line)
    ("N" avy-goto-line-below)
    ("l" avy-goto-line)
    ("L" w--avy-goto-line-any-window)
    "_t_,_j_,_SPC_ char"
    ("SPC" avy-goto-char-timer)
    ("S-SPC" (avy-goto-char-timer t))
    ("t" evilem--motion-evil-find-char)
    ("T" evilem--motion-evil-find-char-to)
    ("j" evilem--motion-evil-find-char-backward)
    ("J" evilem--motion-evil-find-char-to-backward)
    "_k_,_K_,_/_,_?_ search"
    ("k" evilem--motion-function-evil-search-next)
    ("K" evilem--motion-function-evil-search-previous)
    ("/" evilem--motion-function-evil-search-next)
    ("?" evilem--motion-function-evil-search-previous)
    "_o_ new line"
    ("o" (progn (avy-goto-line) (call-interactively 'evil-open-below)))
    ("O" (progn (avy-goto-line) (call-interactively 'evil-open-above)))
    "_d_ delete"
    ("d" w--avy-evil-delete-line)
    ("D" w--avy-evil-delete-region)
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
  (evil-define-key* 'motion global-map
    (kbd "SPC") 'w--hydra-teleport/body))

(use-package evil-exchange
  :general
  (:states '(normal visual)
   "gx" 'evil-exchange
   "gX" 'evil-exchange-cancel)
  ;; quickly swap two text objects using "gx"; the empty text object is
  ;; a trick to make "gxp" work to move previously marked text without
  ;; moving anything back to the original location.
  (:states 'operator
   "p" #'w--evil-empty-text-object))

(use-package evil-goggles
  :config
  (evil-goggles-mode)
  :delight
  :custom
  (evil-goggles-duration 1)
  (evil-goggles-async-duration evil-goggles-duration)
  (evil-goggles-blocking-duration .2)
  (evil-goggles-pulse t)
  :custom-face
  (evil-goggles-default-face ((t (:inherit highlight)))))

(use-package evil-numbers
  :general
  (:states 'normal
   "+" #'evil-numbers/inc-at-pt
   "-" #'evil-numbers/dec-at-pt))

(use-package evil-surround
  :config
  (evil-add-to-alist
   'evil-surround-pairs-alist
   ;; overwrite defaults to not put spaces inside braces:
   ?\( '("(" . ")")
   ?\[ '("[" . "]")
   ?\{ '("{" . "}")
   ;; without shift key:
   ?\0 '("(" . ")")
   ?\9 '("(" . ")")
   ;; nice quotation marks:
   ?\‘ '("‘" . "’")
   ?\’ '("‘" . "’")
   ?\q '("‘" . "’")
   ?\“ '("“" . "”")
   ?\” '("“" . "”")
   ?\Q '("“" . "”"))
  (setq-default evil-surround-pairs-alist evil-surround-pairs-alist)
  :general
  (:states 'operator
   "s" 'evil-surround-edit
   "S" 'evil-Surround-edit)
  (:states 'visual
   "S" 'evil-surround-region
   "gS" 'evil-Surround-region))

(use-package evil-visualstar
  :general
  (:states 'visual
   "*" #'evil-visualstar/begin-search-forward
   "#" #'evil-visualstar/begin-search-backward))


;;;; text objects

(use-package evil-args
  :general
  (:keymaps 'evil-inner-text-objects-map
   "a" #'evil-inner-arg)
  (:keymaps 'evil-outer-text-objects-map
   "a" #'evil-outer-arg))

(use-package evil-indent-plus
  :general
  (:keymaps 'evil-inner-text-objects-map
   "i" #'evil-indent-plus-i-indent
   "i" #'evil-indent-plus-i-indent
   "J" #'evil-indent-plus-i-indent-up-down
   "TAB" #'evil-indent-plus-i-indent)
  (:keymaps 'evil-outer-text-objects-map
   "i" #'evil-indent-plus-a-indent
   "I" #'evil-indent-plus-a-indent-up
   "J" #'evil-indent-plus-a-indent-up-down
   "TAB" #'evil-indent-plus-a-indent-up))

(use-package evil-textobj-anyblock
  ;; todo perhaps replace with https://github.com/noctuid/targets.el
  :general
  (:keymaps 'evil-inner-text-objects-map
   "b" #'evil-textobj-anyblock-inner-block)
  (:keymaps 'evil-outer-text-objects-map
   "b" #'evil-textobj-anyblock-a-block))

(evil-define-text-object w--evil-text-object-whole-buffer (count &optional beg end type)
  "Text object for the whole buffer."
  (evil-range (point-min) (point-max) 'line))

(evil-define-text-object w--evil-empty-text-object (count &optional beg end type)
  "Empty text object."
  (evil-range (point) (point)))

(evil-define-text-object w--evil-text-object-symbol-dwim (count &optional beg end type)
  "Intelligently pick evil-inner-symbol or evil-a-symbol."
  (if (eq this-command 'evil-delete)
      (evil-a-symbol count)
    (evil-inner-symbol count)))

(use-package evil
  :general
  (:states '(operator visual)
   "o" #'w--evil-text-object-symbol-dwim
   "C-a" #'w--evil-text-object-whole-buffer)
  (:keymaps 'evil-outer-text-objects-map
   "g" #'w--evil-text-object-whole-buffer))


;;;; scrolling

(setq
 indicate-buffer-boundaries 'left
 scroll-conservatively 101
 scroll-margin 5)

(w--make-hydra w--hydra-recenter (:foreign-keys nil)
  "recenter"
  "_b_ottom"
  ("b" evil-scroll-line-to-bottom)
  "_c_enter"
  ("c" evil-scroll-line-to-center)
  "_t_op"
  ("t" evil-scroll-line-to-top)
  "_z_ cycle"
  ("z" recenter-top-bottom nil :exit nil))

(general-define-key :states 'motion
  "z z" #'w--hydra-recenter/recenter-top-bottom)


;;;; whitespace

(setq
 require-final-newline 'visit-save
 sentence-end-double-space nil)

(setq-default
 indent-tabs-mode nil
 show-trailing-whitespace t
 tab-width 4)

(use-package whitespace)

(use-package whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode)
  :delight)

(defun w--hide-trailing-whitespace ()
  "Helper to hide trailing whitespace, intended for mode hooks."
  (setq show-trailing-whitespace nil))

(add-hook 'buffer-menu-mode-hook 'w--hide-trailing-whitespace)

(defun w--toggle-show-trailing-whitespace ()
  "Toggle `show-trailing-whitespace`."
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

(use-package indent-guide
  :config
  (indent-guide-global-mode)
  (face-spec-reset-face 'indent-guide-face)
  (set-face-attribute
   'indent-guide-face nil
   :inherit 'font-lock-comment-face)
  :delight
  :custom
  (indent-guide-char "·")
  (indent-guide-delay 0)
  (indent-guide-recursive t)
  (indent-guide-threshold 7))


;;;; minibuffer

(use-package minibuffer
  :ensure nil
  :config
  (add-hook 'minibuffer-setup-hook #'w--hide-trailing-whitespace)
  (bind-keys
   :map minibuffer-local-map
   ("C-w" . backward-kill-word)
   ("C-u" . kill-whole-line))
  (--each (list minibuffer-local-map
                minibuffer-local-ns-map
                minibuffer-local-completion-map
                minibuffer-local-must-match-map
                minibuffer-local-isearch-map)
    (define-key it [escape] 'minibuffer-keyboard-quit)))


;;;; line navigation

;; fixme: https://github.com/melpa/melpa/pull/4814
;;(use-package relative-line-numbers
;;  :config
;;  (defun w--relative-line-numbers-format (offset)
;;    "Format relative line number for OFFSET."
;;    (number-to-string (abs (if (= offset 0) (line-number-at-pos) offset))))
;;  (setq relative-line-numbers-format 'w--relative-line-numbers-format))
(use-package nlinum)

(use-package nlinum-relative
  :config
  (set-face-attribute
   'nlinum-relative-current-face nil
   :foreground nil
   :background nil
   :weight 'normal
   :inherit 'nlinum))

(evil-define-motion w--evil-next-line (count)
  (if visual-line-mode
      (progn
        (setq evil-this-type 'exclusive)
        (evil-next-visual-line count))
    (setq evil-this-type 'line)
    (evil-next-line count)))

(evil-define-motion w--evil-previous-line (count)
  (if visual-line-mode
      (progn
        (setq evil-this-type 'exclusive)
        (evil-previous-visual-line count))
    (setq evil-this-type 'line)
    (evil-previous-line count)))

(evil-define-motion w--evil-end-of-line (count)
  :type inclusive
  (if visual-line-mode
      (evil-end-of-visual-line count)
    (evil-end-of-line count)))

(evil-define-motion w--evil-first-non-blank ()
  :type exclusive
  (if visual-line-mode
      (evil-first-non-blank-of-visual-line)
    (evil-first-non-blank)))

;; todo: make "0" work visually in visual line mode. maybe using
;; something like this:
;; (evil-redirect-digit-argument evil-motion-state-map "0" 'evil-beginning-of-line)

(evil-define-key* '(normal visual) global-map
  [remap evil-next-line] 'w--evil-next-line
  [remap evil-previous-line] 'w--evil-previous-line
  [remap evil-end-of-line] 'w--evil-end-of-line
  [remap evil-first-non-blank] 'w--evil-first-non-blank)


;;;; search

(use-package isearch
  :ensure nil
  :init
  (provide 'isearch)  ; fake feature for emacs 25
  :config
  (setq
   isearch-allow-prefix nil
   isearch-forward t  ;; initial direction; useful after swiper
   lazy-highlight-cleanup nil
   lazy-highlight-initial-delay 0.5
   lazy-highlight-max-at-a-time nil
   search-default-mode t))

(use-package thingatpt
  :config
  (defun w--thing-at-point-dwim (&optional deactivate-selection move-to-beginning)
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

(use-package replace
  :ensure nil
  :init
  (provide 'replace)  ; fake feature for emacs 25
  :config
  (defun w--query-replace-thing-at-point-dwim ()
    "Return 'query-replace' for the active region or the symbol at point."
    (interactive)
    (let* ((use-boundaries (not (region-active-p)))
           (thing (regexp-quote (w--thing-at-point-dwim t t)))
           (replacement
            (read-from-minibuffer
             (format "Replace ‘%s’ with: " thing)
             thing nil nil
             query-replace-to-history-variable)))
      (when use-boundaries
        (setq thing (format "\\_<%s\\_>" thing)))
      (query-replace-regexp thing replacement)))
  (w--make-hydra w--hydra-replace nil
    "replace"
    "_p_roject"
    ("p" projectile-replace)
    ("P" projectile-replace-regexp)
    "_r_ dwim"
    ("r" w--query-replace-thing-at-point-dwim)
    "_s_ymbol"
    ("s" w--query-replace-thing-at-point-dwim)
    "_q_uery"
    ("q" query-replace)
    ("Q" query-replace-regexp)))

(use-package occur
  :ensure nil
  :init
  (provide 'occur)  ; fake feature since it is actually inside replace.el
  :config
  (evil-set-initial-state 'occur-mode 'motion)
  (evil-define-key* '(motion normal) occur-mode-map
    (kbd "RET") 'occur-mode-goto-occurrence
    (kbd "C-e") 'occur-prev
    (kbd "C-n") 'occur-next
    (kbd "C-p") 'occur-prev)

  (defun w--occur-mode-hook ()
    (toggle-truncate-lines t)
    (next-error-follow-minor-mode)
    (w--set-major-mode-hydra #'w--hydra-occur/body))
  (add-hook 'occur-mode-hook #'w--occur-mode-hook)

  (defun w--occur-dwim (&optional nlines)
    "Call `occur' with a sane default."
    (interactive "P")
    (let ((thing (read-string
                  "Open occur for regexp: "
                  (regexp-quote (or (w--thing-at-point-dwim) ""))
                  'regexp-history)))
      (occur thing nlines)
      (evil-force-normal-state)))

  (w--make-hydra w--hydra-occur nil
    "occur"
    "_n__e_ nav"
    ("n" occur-next :exit nil)
    ("e" occur-prev :exit nil)
    "_f_ollow"
    ("f" next-error-follow-minor-mode)))

(use-package swiper
  :custom
  (swiper-action-recenter t)
  (swiper-goto-start-of-match t)
  :general
  (:states 'motion
   "/" 'swiper
   "g/" 'evil-search-forward)
  (:states 'visual
   "/" 'w--swiper-thing-at-point-dwim))

(defun w--swiper-thing-at-point-dwim ()
  "Start `swiper` searching for the thing at point."
  (interactive)
  (let ((query (w--thing-at-point-dwim)))
    (evil-force-normal-state)  ; do not expand region in visual mode
    (swiper query)))

(use-package ag
  :config
  (setq
   ag-project-root-function 'w--ag-project-root
   ag-reuse-buffers t)
  (add-hook 'ag-mode-hook (fn: toggle-truncate-lines t))

  (defun w--ag-project-root (directory)
    "Find project root for DIRECTORY; used for ag-project-root-function."
    (let ((default-directory directory))
      (projectile-project-root)))

  (defun w--counsel-ag-project (&optional unrestricted)
    "Run counsel-ag on the current project, defaulting to the symbol at point."
    (interactive)
    (counsel-ag
     (w--thing-at-point-dwim)
     (projectile-project-root)
     (if unrestricted "--unrestricted" "")
     (if unrestricted "search all project files" "search project files")))

  (defun w--counsel-ag-project-all-files ()
    "Run counsel-ag on all files within the project root."
    (interactive)
    (w--counsel-ag-project t))

  (w--make-hydra w--hydra-ag nil
    "ag"
    "_a_ project"
    ("a" ag-project)
    "_f_iles"
    ("f" ag-project-files)
    ("F" ag-files)
    "_g_ project"
    ("g" ag-project)
    ("G" ag)
    "_r_egex"
    ("r" ag-project-regexp)
    ("R" ag-regexp)))

;; todo: switch to rg/ripgrep
;; (use-package rg)
;; use / for swiper
;; use ? for w--swiper-thing-at-point-dwim
;; use ,/ for rg
;; use ,? for counsel-rg

(use-package highlight-symbol
  :custom
  (highlight-symbol-idle-delay 1.0)
  (highlight-symbol-on-navigation-p t)
  :delight
  :general
  (:states 'motion
   "C-p" #'highlight-symbol-prev
   "C-n" #'highlight-symbol-next)
  (:states 'normal
   "C-p" #'w--evil-paste-pop-or-highlight-symbol-prev
   "C-n" #'w--evil-paste-pop-next-or-highlight-symbol-next))

(defun w--evil-paste-pop-or-highlight-symbol-prev (count)
  "Either paste-pop (with COUNT) or jump to previous symbol occurence."
  (interactive "p")
  (condition-case nil
      (evil-paste-pop count)
    (user-error
      (highlight-symbol-prev))))

(defun w--evil-paste-pop-next-or-highlight-symbol-next (count)
  "Either paste-pop-next (with COUNT) or jump to next symbol occurence."
  (interactive "p")
  (condition-case nil
      (evil-paste-pop-next count)
    (user-error
      (highlight-symbol-next))))


;;;; previous/next navigation

;; previous/next thing (inspired by vim unimpaired)
;; todo: this should become a fancy hydra

(defun w--last-error ()
  "Jump to the last error; similar to 'first-error'."
  (interactive)
  (condition-case err (while t (next-error)) (user-error nil)))

(evil-define-key* '(motion normal) global-map
  (kbd "[ SPC") (lambda () (interactive) (save-excursion (evil-insert-newline-above)))
  (kbd "] SPC") (lambda () (interactive) (save-excursion (evil-insert-newline-below)))
  "[b" 'evil-prev-buffer
  "]b" 'evil-next-buffer
  "[c" 'flycheck-previous-error
  "]c" 'flycheck-next-error
  "[C" 'flycheck-first-error
  "]C" 'w--flycheck-last-error
  "[d" (lambda () (interactive) (diff-hl-mode) (diff-hl-previous-hunk))
  "]d" (lambda () (interactive) (diff-hl-mode) (diff-hl-next-hunk))
  "[e" 'previous-error
  "]e" 'next-error
  "[E" 'first-error
  "]E" 'w--last-error
  "[m" 'smerge-prev
  "]m" 'smerge-next
  "[s" 'highlight-symbol-prev
  "]s" 'highlight-symbol-next
  "[S" 'highlight-symbol-prev-in-defun
  "]S" 'highlight-symbol-next-in-defun
  "[w" 'evil-window-prev
  "]w" 'evil-window-next
  "[z" 'outline-previous-visible-heading
  "]z" 'outline-next-visible-heading
  (kbd "C-,") 'evil-prev-buffer
  (kbd "C-.") 'evil-next-buffer)


;;;; parens

(use-package smartparens
  :delight
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (show-smartparens-global-mode))

(use-package rainbow-delimiters
  :defer t)

(use-package highlight-parentheses
  :defer t
  :delight)

(use-package evil-cleverparens
  :defer t
  :config
  (setq
   evil-cleverparens-swap-move-by-word-and-symbol t))

(use-package syntactic-close
  :general
  (:state 'insert
   ;; this is a zero, i.e. C-) without shift
   (kbd "C-0") #'syntactic-close))


;;;; filling

(use-package emacs
  :delight auto-fill-function)

(defun w--evil-fill-paragraph-dwim ()
  "Fill the current paragraph."
  (interactive)
  ;; move point after comment marker; useful for multi-line comments.
  (save-excursion
    (end-of-line)
    (fill-paragraph)))

(defun w--use-very-long-lines ()
  "Use very long lines so that `fill-paragraph' and related functions do not add newlines."
  (interactive)
  (setq fill-column most-positive-fixnum)
  (auto-fill-mode -1))

(evil-define-key* 'normal global-map
  "Q" 'w--evil-fill-paragraph-dwim)

(use-package fill-column-indicator
  :config
  (setq fci-rule-width 2))

(use-package multi-line)

(use-package visual-fill-column)


;;;; outline

(use-package outline
  :delight
  (outline-minor-mode " ‣"))


;;;; move lines

(use-package drag-stuff
  :config
  (evil-define-key* 'normal global-map
    (kbd "M-n") 'drag-stuff-down
    (kbd "M-e") 'drag-stuff-up
    (kbd "M-h") 'evil-shift-left-line
    (kbd "M-i") 'evil-shift-right-line)
  ;; todo: C-[hnei] in visual mode?
  (evil-define-key* 'visual global-map
    (kbd "M-h") (lambda (beg end)
                  (interactive "r")
                  (evil-shift-left beg end)
                  (evil-force-normal-state)
                  (call-interactively 'evil-visual-restore))
    (kbd "M-i") (lambda (beg end)
                  (interactive "r")
                  (evil-shift-right beg end)
                  (evil-force-normal-state)
                  (call-interactively 'evil-visual-restore))))


;;;; expand-region

(use-package expand-region
  :config
  (setq expand-region-fast-keys-enabled nil)
  (evil-define-key* 'visual global-map
    (kbd "TAB") 'w--hydra-expand-region/er/expand-region)
  (w--make-hydra w--hydra-expand-region nil
    "expand-region"
    "_<tab>_ expand"
    ("<tab>" er/expand-region :exit nil)
    "_u_ndo"
    ("u" (er/expand-region -1) :exit nil)
    "_r_eset"
    ("r" (er/expand-region 0) :exit t)))


;;;; narrowing

(defun w--narrow-dwim ()
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


;;;; copy-as-format

;; todo https://github.com/sshaw/copy-as-format/issues/2
(use-package copy-as-format
  :config
  (setq
   copy-as-format-default "slack"
   copy-as-format-format-alist  ;; only retain formats i use
   '(("github" copy-as-format--github)
     ("markdown" copy-as-format--markdown)
     ("rst" copy-as-format--rst)
     ("slack" copy-as-format--slack)))
  (evil-define-operator w--evil-copy-as-format (beg end type)
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
      (pop-mark)))
  (evil-define-key* 'visual global-map
    "Y" #'w--evil-copy-as-format))


;;;; insert state

(defun w--evil-transpose-chars ()
  "Invoke 'transpose-chars' on the right chars in insert state."
  (interactive)
  (backward-char)
  (transpose-chars nil)
  (unless (eolp) (forward-char)))

(defun w--kill-line-dwim ()
  "Kill line, or join the next line when at eolp."
  (interactive)
  (let ((was-at-eol (eolp)))
    (kill-line)
    (when was-at-eol
      (fixup-whitespace))))

(evil-define-key* 'insert global-map
  (kbd "C-a") 'w--evil-first-non-blank
  (kbd "C-c") 'evil-normal-state
  (kbd "C-d") 'delete-char
  (kbd "C-g") 'evil-normal-state
  (kbd "C-e") 'end-of-visual-line
  (kbd "C-h") [backspace]
  (kbd "C-k") 'w--kill-line-dwim
  (kbd "C-n") 'next-line  ;; fixme: completion trigger?
  (kbd "C-o") 'evil-normal-state
  (kbd "C-p") 'previous-line
  (kbd "C-t") 'w--evil-transpose-chars)

(evil-define-key* 'insert global-map
  ;; during typing, ctrl-v is "paste", like everywhere else
  (kbd "C-v") 'yank)

(evil-define-key* 'insert global-map
  (kbd "C-SPC") 'fixup-whitespace)

(evil-define-key* 'insert global-map
  ;; shift line with < and > (same chars as in normal mode;
  ;; used instead of standard vim bindings C-d and C-t.
  (kbd "C-,") 'evil-shift-left-line
  (kbd "C-<") 'evil-shift-left-line
  (kbd "C-.") 'evil-shift-right-line
  (kbd "C->") 'evil-shift-right-line)

;; indent on enter, keeping comments open (if any)
(evil-define-key* 'insert global-map
  (kbd "RET") 'comment-indent-new-line)

;; type numbers by holding alt using home row keys and by having a
;; "numpad overlay" starting at the home position for my right hand.
(--each (-zip-pair (split-string "arstdhneio'luy7890km.," "" t)
                   (split-string "87659012345456789000.," "" t))
  (-let [(key . num) it]
    (evil-define-key*
     'insert global-map
     (kbd (concat "M-" key))
     (lambda () (interactive) (insert num)))))


;;;; text case

(use-package string-inflection
  :config
  (w--make-hydra w--hydra-text-case
      (:post w--hydra-evil-repeat-record-command)
    "text case"
    "_c_ycle"
    ("c" string-inflection-all-cycle)
    ("`" string-inflection-all-cycle)
    ("~" string-inflection-all-cycle)
    "_a_ camel"
    ("a" string-inflection-camelcase)
    ("A" string-inflection-lower-camelcase)
    "_l_isp"
    ("l" string-inflection-lisp)
    "_s_nake"
    ("s" string-inflection-underscore)
    ("S" string-inflection-upcase)
    "_u_pper"
    ("u" string-inflection-upcase)))


;;;; projects

(use-package projectile
  :defer t
  :hook find-file-hook
  :delight

  :custom
  (projectile-completion-system 'ivy)
  (projectile-ignored-projects '("/usr/local/" "~/"))
  ;; (projectile-mode-line nil)  ;; causes eager loading, :delight has same effect
  (projectile-require-project-root nil)
  (projectile-sort-order 'recently-active)
  (projectile-switch-project-action 'projectile-vc)

  :commands
  w--hydra-project/body

  :config
  (projectile-mode)

  (defun w--projectile-find-file-all (&optional pattern)
    "Find any file in the current project, including ignored files."
    (require 'projectile)
    (interactive
     (list
      (read-string
       "file name pattern (empty means all): "
       (if buffer-file-name
           (concat (file-name-extension buffer-file-name) "$")
         "")
       ".")))
    (ivy-read
     "Find file in complete project: "
     (projectile-make-relative-to-root
      (directory-files-recursively (projectile-project-root) pattern))
     :action
     (lambda (filename)
       (find-file (concat
                   (file-name-as-directory (projectile-project-root))
                   filename)))
     :require-match t
     :history 'file-name-history))

  (defun w--projectile-project-bury-buffers ()
    "Quit all windows and bury all buffers for the current project."
    (interactive)
    (require 'projectile)
    (-each (projectile-project-buffers)
      (lambda (buffer)
        (-each (get-buffer-window-list buffer)
          (lambda (window)
            (quit-window nil window)))
        (bury-buffer buffer))))

  (w--make-hydra w--hydra-project nil
    "project"
    "_a_ny file"
    ("a" w--projectile-find-file-all)
    "_b_uffer"
    ("b" projectile-switch-to-buffer)
    ("B" projectile-switch-to-buffer-other-window)
    "_d_ir"
    ("d" projectile-find-dir)
    ("D" projectile-find-dir-other-window)
    "_f_ile"
    ("f" projectile-find-file)
    ("F" projectile-find-file-other-window)
    "_k_ill"
    ("k" projectile-kill-buffers)
    "_o_ccur"
    ("o" projectile-multi-occur)
    "_p_roject"
    ("p" projectile-switch-project)
    ("P" projectile-switch-open-project)
    "_q_ bury"
    ("q" w--projectile-project-bury-buffers)
    "_r_eplace"
    ("r" projectile-replace)
    ("R" projectile-replace-regexp)
    "_s_ave"
    ("s" projectile-save-project-buffers)
    "_t_est/impl"
    ("t" projectile-toggle-between-implementation-and-test)
    ("T" projectile-find-implementation-or-test-other-window)
    "_-_ top dir"
    ("-" projectile-dired)
    "_/__?_ counsel-ag"
    ("/" w--counsel-ag-project)
    ("?" w--counsel-ag-project-all-files)
    "_!_ terminal"
    ("!" terminal-here-project-launch)
    ("1" terminal-here-project-launch)))


;;;; jumping around

(use-package avy
  :config
  (setq
   avy-all-windows nil
   avy-all-windows-alt t
   avy-background t
   avy-keys (string-to-list "arstneio"))
  (avy-setup-default))

(use-package dired
  :ensure nil
  :config
  (evil-define-key* '(motion normal) dired-mode-map
    "-" 'dired-jump)) ;; inspired by vim vinagre

(defvar w--jump-commands
  '(evil-backward-paragraph
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
    ivy-done
    recenter-top-bottom
    switch-to-buffer))

(defvar w--jump-hooks
  '(evil-jumps-post-jump-hook
    focus-in-hook
    next-error-hook))

(defun w--mark-as-jump-commands (&rest commands)
  "Mark COMMANDS as jump commands."
  (setq w--jump-commands (-union w--jump-commands commands)))

(use-package nav-flash
  :config
  (setq nav-flash-delay 5)
  (add-hook 'post-command-hook #'w--maybe-nav-flash)
  (dolist (hook w--jump-hooks)
    (add-hook hook #'w--maybe-nav-flash))

  (defun w--maybe-nav-flash ()
    "Highlight point when run after a jump command."
    (when (member this-command w--jump-commands)
      (nav-flash-show))))

(use-package dumb-jump
  :custom
  (dumb-jump-selector 'ivy)
  :general
  (:states 'motion
    "gd" #'dumb-jump-go-current-window
    "gD" #'dumb-jump-go-other-window)
  :config
  (defun w--jump-around-advice (fn &rest args)
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
  (advice-add 'dumb-jump-go :around #'w--jump-around-advice))


;;;; frames and windows

;; my preferred window layout is multiple full-height windows,
;; next to each other in a horizontal fashion, i.e. screen
;; divided into columns.

(use-package winner
  :custom
  (winner-dont-bind-my-keys t)
  :config
  (winner-mode))

(setq
 default-frame-alist '((width . 160) (height . 48))
 evil-split-window-below t
 evil-vsplit-window-right t
 frame-resize-pixelwise t
 frame-title-format "%b — emacs"
 help-window-select t
 split-height-threshold nil
 split-width-threshold 120
 split-window-preferred-function 'visual-fill-column-split-window-sensibly)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(defvar w--balanced-windows-functions
  '(delete-window quit-window split-window)
  "Functions needing advice to keep windows balanced.")

(defun w--balance-windows-advice (&rest _ignored)
  "Balance windows (intended as ;after advice); ARGS are ignored."
  (balance-windows))

(define-minor-mode w--balanced-windows-mode
  "Global minor mode to keep windows balanced at all times."
  :global t
  (setq evil-auto-balance-windows w--balanced-windows-mode)
  (dolist (fn w--balanced-windows-functions)
    (if w--balanced-windows-mode
        (advice-add fn :after #'w--balance-windows-advice)
      (advice-remove fn #'w--balance-windows-advice)))
  (when w--balanced-windows-mode
    (balance-windows)))

(w--balanced-windows-mode)

(defun w--evil-window-next-or-vsplit ()
  "Focus next window, or vsplit if it is the only window in this frame."
  (interactive)
  (if (> (count-windows) 1)
      (evil-window-next nil)
    (evil-window-vsplit)))

(defun w--evil-goto-window (n)
  "Go to window N."
  (evil-window-top-left)
  (evil-window-next n))

(defun w--goto-window-1 ()
  "Go to the first window."
  (interactive)
  (w--evil-goto-window 1))

(defun w--goto-window-2 ()
  "Go to the second window."
  (interactive)
  (w--evil-goto-window 2))

(defun w--goto-window-3 ()
  "Go to the third window."
  (interactive)
  (w--evil-goto-window 3))

(defun w--goto-window-4 ()
  "Go to the fourth window."
  (interactive)
  (w--evil-goto-window 4))

(defun w--set-as-window-1 ()
  "Make this the first window."
  (interactive)
  (evil-window-move-far-left))

(defun w--set-as-window-2 ()
  "Make this the second window."
  (interactive)
  (evil-window-move-far-left)
  (buf-move-right))

(defun w--set-as-window-3 ()
  "Make this the third window."
  (interactive)
  (evil-window-move-far-left)
  (buf-move-right)
  (buf-move-right))

(defun w--set-as-window-4 ()
  "Make this the fourth window."
  (interactive)
  (evil-window-move-far-left)
  (buf-move-right)
  (buf-move-right)
  (buf-move-right))

(w--mark-as-jump-commands
 'w--evil-window-next-or-vsplit
 'w--goto-window-1
 'w--goto-window-2
 'w--goto-window-3
 'w--goto-window-4)

;; todo: write these bindings in a more concise way
(cond
 ((eq system-type 'darwin)  ;; osx: command key
  (evil-define-key*
   'motion global-map
   (kbd "s-1") 'w--goto-window-1
   (kbd "s-2") 'w--goto-window-2
   (kbd "s-3") 'w--goto-window-3
   (kbd "s-4") 'w--goto-window-4)
  (bind-keys
   ("s-1" . w--goto-window-1)
   ("s-2" . w--goto-window-2)
   ("s-3" . w--goto-window-3)
   ("s-4" . w--goto-window-4)))
 (t  ;; others: control key
  (evil-define-key*
   'motion global-map
   (kbd "C-SPC") 'evil-window-next
   (kbd "C-S-SPC") 'evil-window-next
   (kbd "C-`") 'evil-window-next
   (kbd "C-~") 'evil-window-prev
   (kbd "C-1") 'w--goto-window-1
   (kbd "C-2") 'w--goto-window-2
   (kbd "C-3") 'w--goto-window-3
   (kbd "C-4") 'w--goto-window-4
   (kbd "C-!") 'w--set-as-window-1
   (kbd "C-@") 'w--set-as-window-2
   (kbd "C-#") 'w--set-as-window-3
   (kbd "C-$") 'w--set-as-window-4)
  (bind-keys
   ("C-SPC" . evil-window-next)
   ("C-S-SPC" . evil-window-prev)
   ("C-`" . evil-window-next)
   ("C-~" . evil-window-prev)
   ("C-1" . w--goto-window-1)
   ("C-2" . w--goto-window-2)
   ("C-3" . w--goto-window-3)
   ("C-4" . w--goto-window-4)
   ("C-!" . w--set-as-window-1)
   ("C-@" . w--set-as-window-2)
   ("C-#" . w--set-as-window-3)
   ("C-$" . w--set-as-window-4))))

(use-package buffer-move)

(defun w--make-frame ()
  "Make a new frame."
  (interactive)
  (let ((frame (make-frame)))
    (select-frame-set-input-focus frame)
    frame))

(defun w--make-frame-new-buffer ()
  "Make a new frame with a new buffer."
  (interactive)
  (with-selected-frame (w--make-frame)
    (call-interactively #'evil-buffer-new)))

(define-minor-mode w--pinned-buffer-mode
  "Pin the current buffer to the selected window."
  nil " ▣" nil
  (set-window-dedicated-p (selected-window) w--pinned-buffer-mode))

(w--make-hydra w--hydra-window nil
  "window"
  "_h__n__e__i_ _1__2__3__4_ navigate"
  ("h" buf-move-left)
  ("n" buf-move-down)
  ("e" buf-move-up)
  ("i" buf-move-right)
  ("H" evil-window-left)
  ("N" evil-window-down)
  ("E" evil-window-up)
  ("I" evil-window-right)
  ("1" w--goto-window-1)
  ("2" w--goto-window-2)
  ("3" w--goto-window-3)
  ("4" w--goto-window-4)
  ("!" w--set-as-window-1)
  ("@" w--set-as-window-2)
  ("#" w--set-as-window-3)
  ("$" w--set-as-window-4)
  "_b_alance"
  ("b" balance-windows)
  ("=" balance-windows)  ;; evil/vim style
  "_c_lose"
  ("c" evil-window-delete)
  "_f_rame"
  ("f" (w--make-frame))
  ("F" (w--make-frame-new-buffer))
  "_o_nly"
  ("o" delete-other-windows)
  "_p_in"
  ("p" w--pinned-buffer-mode)
  "_r_otate"
  ("r" evil-window-rotate-downwards nil :exit nil)
  ("R" evil-window-rotate-upwards nil :exit nil)
  "_s_plit"
  ("s" evil-window-split)
  ("S" evil-window-new)
  "_u_ndo"
  ("u" winner-undo)
  ("U" winner-redo)
  "_v_split"
  ("v" evil-window-vsplit)
  ("V" evil-window-vnew)
  "_w_ cycle"
  ("w" w--evil-window-next-or-vsplit)
  ("C-w" w--evil-window-next-or-vsplit)
  "_+_/_-_ width"
  ("+" evil-window-increase-width nil :exit nil)
  ("-" evil-window-decrease-width nil :exit nil))

;; replace evil-window-map completely
(evil-define-key* '(emacs motion) global-map
  (kbd "C-w") 'w--hydra-window/body)


;;;; spelling

(use-package ispell
  :defer t
  :config
  (setq ispell-dictionary "english"))

;; todo https://github.com/d12frosted/flyspell-correct
(use-package flyspell
  :delight " ∼")
;; (use-package flyspell-correct-ivy)

(use-package guess-language
  :config
  (setq guess-language-languages '(en de fr nl sv)))


;;;; completion

(defvar w--ivy-height-percentage 30
  "Percentage of the screen height that ivy should use.")

(use-package emacs
  :delight (abbrev-mode " ⋯"))

(use-package flx)

(use-package amx
  :config
  (amx-mode +1))

(use-package ivy
  :demand t
  :bind
  (:map
   ivy-minibuffer-map
   ("C-h" . ivy-backward-delete-char)
   ("C-w" . ivy-backward-kill-word)
   ("C-u" . kill-whole-line)
   ("C-SPC" . ivy-avy)
   ("C-<return>" . ivy-dispatching-done-hydra))
  :config
  (setq
   ivy-count-format "(%d/%d) "
   ivy-height 20
   ivy-initial-inputs-alist nil
   ivy-wrap t)

  (ivy-mode 1)
  :delight)

(define-key ivy-minibuffer-map
  [escape] 'minibuffer-keyboard-quit) ;; fixme: use :bind perhaps?

(add-hook 'window-size-change-functions #'w--adjust-ivy-height)

(defun w--clamp-number (num low high)
  "Clamp NUM between LOW and HIGH."
  (min high (max num low)))

(defun w--adjust-ivy-height (frame)
  "Adjust ivy-height based on the current FRAME height."
  (let* ((total-lines (frame-text-lines frame))
          (lines (truncate (* total-lines w--ivy-height-percentage 0.01)))
          (new-height (w--clamp-number lines 10 20)))
    (setq ivy-height new-height)))

(use-package ivy-hydra)

(use-package ivy-rich
  :config
  (dolist (command '(ivy-switch-buffer ivy-switch-buffer-other-window))
    (ivy-set-display-transformer
     command 'ivy-rich-switch-buffer-transformer)))

(use-package counsel
  :config
  (counsel-mode)
  :delight)

(use-package company
  :delight
  :general
  (:states 'insert
   "C-<return>" #'company-manual-begin
   "<tab>" #'w--indent-or-complete)
  (:keymaps 'company-active-map
   "C-n" #'company-select-next
   "C-p" #'company-select-previous
   "C-<return>" #'company-select-next
   "<tab>" #'company-complete-common-or-cycle)
  :custom
  (company-auto-complete 'company-explicit-action-p)
  (company-dabbrev-code-everywhere t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  (company-idle-delay nil)
  (company-occurrence-weight-function 'company-occurrence-prefer-any-closest)
  (company-require-match nil)
  (company-selection-wrap-around t)
  (company-transformers '(company-sort-by-occurrence))
  :config
  (add-to-list 'company-auto-complete-chars ?\( )
  (add-to-list 'company-backends 'company-files)
  (global-company-mode)
  (defun w--indent-or-complete ()
    (interactive)
    (if (looking-at "\\_>")
        (company-manual-begin)
      (call-interactively #'indent-for-tab-command))))


;;;; version control

(use-package autorevert
  :config
  (global-auto-revert-mode)
  :custom
  (auto-revert-check-vc-info t)
  :delight (auto-revert-mode" ⇤"))

(use-package ediff
  :config
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package magit
  :config
  (magit-auto-revert-mode)
  (magit-wip-after-save-mode)
  (magit-wip-after-apply-mode)
  (magit-wip-before-change-mode)
  (add-to-list 'magit-repository-directories '("~/Projects/" . 2))
  (add-to-list 'evil-overriding-maps '(magit-blame-mode-map . nil))
  (add-hook 'magit-popup-mode-hook 'w--hide-trailing-whitespace)
  :custom
  (magit-branch-prefer-remote-upstream '("master"))
  (magit-branch-read-upstream-first 'fallback)
  (magit-cherry-pick-arguments '("-x"))
  (magit-completing-read-function 'ivy-completing-read)
  (magit-display-file-buffer-function 'magit-display-file-buffer-other-window)
  (magit-fetch-arguments '("--prune"))
  (magit-list-refs-sortby '("-creatordate"))
  (magit-log-arguments '("--graph" "--color" "--decorate" "--follow" "-n256"))
  (magit-merge-arguments '("--no-ff"))
  (magit-popup-show-help-echo nil)
  (magit-prefer-remote-upstream t)
  (magit-process-popup-time 10)
  (magit-rebase-arguments '("--autostash"))
  (magit-show-refs-arguments '("--sort=-committerdate"))
  (magit-tag-arguments '("--annotate"))
  :delight
  (magit-wip-after-save-local-mode)
  (magit-wip-after-apply-mode)
  (magit-wip-before-change-mode))

(use-package evil-magit
  :after magit
  :config
  (add-hook 'magit-log-mode-hook #'w--disable-colemak)
  (add-hook 'magit-status-mode-hook #'w--disable-colemak)
  ;; todo: make ,q use the various magit-*-bury-buffer functions, then
  ;; unbind q to force ,q usage.
  (evil-define-key*
   '(normal visual) magit-mode-map
   [escape] nil
   "n" #'evil-next-visual-line
   "e" #'evil-previous-visual-line
   (kbd "C-n") #'magit-section-forward
   (kbd "C-e") #'magit-section-backward
   (kbd "C-p") #'magit-section-backward
   (kbd "TAB") #'magit-section-cycle
   (kbd "C-TAB") #'magit-section-toggle
   (kbd "C-w") 'w--hydra-window/body)
  (general-define-key
   :keymaps 'magit-status-mode-map
   "q" nil)
  (general-define-key
   :keymaps 'magit-diff-mode-map
   "SPC" nil
   "DEL" nil)
  (general-define-key
   :keymaps 'magit-blame-mode-map
   :states '(motion normal)
   (kbd "C-n") 'magit-blame-next-chunk
   (kbd "C-e") 'magit-blame-previous-chunk
   (kbd "C-p") 'magit-blame-previous-chunk))

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t)
  :custom
  (magithub-api-timeout 10)
  (magithub-pull-request-arguments '("-o"))
  :general
  (:keymaps 'magithub-map  ;; colemak tweaks
   "e" nil
   "c" #'magithub-edit-thing))

(use-package git-link
  :defer t
  :custom
  (git-link-open-in-browser t))

(use-package diff-hl
  :hook
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  :config
  (w--mark-as-jump-commands
    'diff-hl-next-hunk
    'diff-hl-previous-hunk))

(use-package vdiff
  :defer t)

(use-package vdiff-magit
  :defer t)

(defun w--magit-status-other-repository ()
  "Open git status for another repository."
  (interactive)
  (setq current-prefix-arg t)
  (call-interactively 'magit-status))

(defun w--git-web-browse ()
  "Open a web browser for the current git repo or file."
  (interactive)
  (if (region-active-p)
      (let ((git-link-open-in-browser t))
        (call-interactively #'git-link)
        (setq kill-ring (cdr kill-ring)))
    (magithub-browse)))

(w--make-hydra w--hydra-git nil
  "git"
  "_b_lame"
  ("b" magit-blame)
  ("B" magit-log-buffer-file)
  "_c_ommit"
  ("c" magit-commit)
  ("C" magit-commit-popup)
  "_d_iff"
  ("d" magit-diff)
  ("D" magit-diff-popup)
  "_f_ile"
  ("f" magit-find-file)
  ("F" magit-find-file-other-window)
  "_g_ popup"
  ("g" magit-dispatch-popup)
  "_l_og"
  ("l" magit-log-current)
  ("L" magit-log-all)
  "_r_efs"
  ("r" magit-show-refs-popup)
  "_s_tatus"
  ("s" magit-status)
  ("S" w--magit-status-other-repository)
  "_t_ lock"
  ("t" magit-toggle-buffer-lock)
  "_w_eb"
  ("w" w--git-web-browse)
  "_!_ command"
  ("!" magit-git-command))

(w--make-hydra w--hydra-merge nil
  "merge"
  "_c_urrent"
  ("c" smerge-keep-current)
  "_m_ine"
  ("m" smerge-keep-mine)
  "_b_ase"
  ("b" smerge-keep-base)
  "_o_ther"
  ("o" smerge-keep-other)
  "_a_ll"
  ("a" smerge-keep-all)
  "go to"
  "_n_ext"
  ("n" smerge-next nil :exit nil)
  "_p_revious"
  ("e" smerge-prev nil :exit nil)
  ("p" smerge-prev nil :exit nil))


;;;; writeroom

(use-package writeroom-mode
  :defer t
  :custom
  (writeroom-global-effects nil)
  (writeroom-maximize-window nil))

(defun w--writeroom-narrower ()
  "Make the writeroom column narrower."
  (interactive)
  (unless (bound-and-true-p writeroom-mode)
    (writeroom-mode))
  (writeroom-decrease-width))

(defun w--writeroom-wider ()
  "Make the writeroom column wider."
  (interactive)
  (unless (bound-and-true-p writeroom-mode)
    (writeroom-mode))
  (writeroom-increase-width))

(defun w--writeroom-reset ()
  "Reset the writeroom column width."
  (interactive)
  (unless (bound-and-true-p writeroom-mode)
    (writeroom-mode))
  (writeroom-adjust-width nil))


;;;; flycheck

(use-package flycheck
  :config
  (global-flycheck-mode)
  :custom
  (flycheck-checker-error-threshold 1000)
  (flycheck-display-errors-delay 1.0)
  (flycheck-idle-change-delay 3)
  (flycheck-mode-line-prefix "✓"))

(w--make-hydra w--hydra-flycheck nil
  "flycheck"
  "_c_ errors"
  ("c" flycheck-list-errors)
  ("o" flycheck-list-errors)
  "_n_/_e_/_p_ nav"
  ("n" flycheck-next-error nil :exit nil)
  ("e" flycheck-previous-error nil :exit nil)
  ("p" flycheck-previous-error nil :exit nil)
  "_t_oggle"
  ("t" flycheck-mode))

(defun w--flycheck-last-error ()
  "Jump to the last flycheck error."
  (interactive)
  (goto-char (point-max))
  (flycheck-previous-error))


;;;; toggles

(defun w--line-numbers-cycle ()
  "Cycle between no, absolute, and relative line numbers."
  (interactive)
  (cond
   ((and nlinum-mode nlinum-relative-mode)
    (nlinum-mode -1)
    (nlinum-relative-off))
   (nlinum-mode
    (nlinum-relative-mode +1)
    (nlinum-relative-reflush))
   (t
    (nlinum-mode +1)
    (nlinum-relative-mode -1))))

(w--make-hydra w--hydra-toggle nil
  "toggle"
  "_b_ackgound"
  ("b" w--toggle-dark-light-theme)
  ("B" w--set-theme-from-environment)
  "_d_iff"
  ("d" diff-hl-mode)
  "_f_ill"
  ("f" auto-fill-mode)
  ("F" fci-mode)
  "_l_ine"
  ("l" hl-line-mode)
  ("L" global-hl-line-mode)
  "_m_aximize"
  ("m" toggle-frame-maximized)
  ("M" toggle-frame-fullscreen)
  "_n_umber"
  ("n" w--line-numbers-cycle)
  ("N" (progn
         (line-number-mode 'toggle)
         (column-number-mode 'toggle)))
  "_o_utline"
  ("o" outline-minor-mode)
  "_t_runcate"
  ("t" toggle-truncate-lines)
  "_v_isual-line"
  ("V" toggle-word-wrap)
  ("v" visual-line-mode)
  "_w_riteroom"
  ("w" writeroom-mode)
  ("W" (progn
         (delete-other-windows)
         (writeroom-mode 'toggle)))
  "_SPC_ whitespace"
  ("SPC" whitespace-mode)
  ("S-SPC" w--toggle-show-trailing-whitespace)
  "_1_ num/sym"
  ("1" global-evil-swap-keys-mode)
  ("!" global-evil-swap-keys-mode)
  "_=_ balanced-windows"
  ("=" w--balanced-windows-mode))


;;;; leader key

(w--make-hydra w--hydra-leader nil
  "_1__2__3__4_ window"
  ("1" w--goto-window-1)
  ("2" w--goto-window-2)
  ("3" w--goto-window-3)
  ("4" w--goto-window-4)
  ("!" w--set-as-window-1)
  ("@" w--set-as-window-2)
  ("#" w--set-as-window-3)
  ("$" w--set-as-window-4)
  "_a_g"
  ("a" w--hydra-ag/body)
  "_b_uffer"
  ("b" w--hydra-buffer/body)
  "_c_heck"
  ("c" w--hydra-flycheck/body)
  "_f_ind"
  ("f" w--hydra-find-file/body)
  "_g_it"
  ("g" w--hydra-git/body)
  "_h_ighlight"
  ("h" (highlight-symbol (w--thing-at-point-dwim)))
  ("H" highlight-symbol-remove-all)
  "_m_erge"
  ("m" w--hydra-merge/body)
  "_n_arrow"
  ("n" w--narrow-dwim)
  "_o_ccur"
  ("o" w--occur-dwim)
  "_p_roject"
  ("p" w--hydra-project/body)
  "_q_ bury buffer"
  ("q" bury-buffer)
  ("Q" unbury-buffer)
  "_r_eplace"
  ("r" w--hydra-replace/body)
  "_s_ave"
  ("s" save-buffer)
  ("S" save-some-buffers)
  "_t_oggle"
  ("t" w--hydra-toggle/body)
  "_u_niversal arg"
  ("u" universal-argument)
  "_w_indow"
  ("w" w--hydra-window/body)
  "M-_x_"
  ("x" amx)
  "_y_ copy format"
  ("y" w--evil-copy-as-format)
  "_z_oom"
  ("z" w--hydra-zoom/body)
  "_SPC_ whitespace"
  ("SPC" whitespace-cleanup)
  "_,_ major mode"
  ("," w--major-mode-hydra)
  ("\\" w--major-mode-hydra)
  "_/_ search"
  ("/" w--swiper-thing-at-point-dwim)
  "_~_ case"
  ("~" w--hydra-text-case/body)
  ("`" w--hydra-text-case/body))

(evil-define-key* 'motion global-map
  "," #'w--hydra-leader/body
  "\\" #'w--hydra-leader/body)


;;;; help

(use-package helpful
  :general
  ("C-h f" 'helpful-callable)
  ("C-h v" 'helpful-variable))


;;;; custom

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)


;;;; major modes

(setq-default major-mode 'text-mode)
(defvar w--major-modes
  '(("normal" . normal-mode)
    ("fundamental" . fundamental-mode)
    ("text (txt)" . text-mode)
    ("c" . c-mode)
    ("elisp" . emacs-lisp-mode)
    ("jinja (j2)" . jinja2-mode)
    ("json" . json-mode)
    ("markdown (md)" . markdown-mode)
    ("org" . org-mode)
    ("python" . python-mode)
    ("restructuredtext (rst)" . rst-mode)
    ("shell" . sh-mode)
    ("sql" . sql-mode)
    ("yaml (yml)" . yaml-mode))
  "Commonly used major modes.")

(defun w--switch-major-mode ()
  "Switch major mode."
  (interactive)
  (let* ((choice
          (ivy-read
           "Switch major mode: "
           w--major-modes
           :require-match t))
         (fn (cdr (assoc choice w--major-modes))))
    (funcall fn)))


;;;; major mode: text (generic)

(use-package typo
  :config
  (setq-default typo-language "prefer-single")
  (add-to-list 'typo-quotation-marks '("prefer-single" "‘" "’" "“" "”"))
  (define-typo-cycle w--typo-cycle-quotation-marks
    "Cycle through various quotation marks."
    ("'" "‘" "’" "“" "”" "\"")))

(use-package text-mode
  :ensure nil
  :defer t
  :config
  ;; todo: use adaptive-wrap-prefix-mode in addition to visual-line-mode
  (defun w--text-mode-hook ()
    (auto-fill-mode)
    (guess-language-mode)
    (visual-line-mode))
  (add-hook 'text-mode-hook 'w--text-mode-hook))


;;;; major mode: programming (generic)

(modify-syntax-entry ?_ "w")

(use-package fic-mode
  :defer t
  :init
  (setq
   fic-highlighted-words
   '("FIXME" "fixme"
     "TODO" "todo"
     "BUG" "bug"
     "XXX" "xxx")))

(use-package prog-mode
  :ensure nil
  :defer t
  :config
  (defun w--prog-mode-hook ()
    (abbrev-mode)
    (evil-swap-keys-swap-number-row)
    (auto-fill-mode)
    (column-number-mode)
    (fic-mode)
    (flyspell-prog-mode)
    ;; (show-paren-mode)  ; fixme: needed?
    (highlight-parentheses-mode)
    (highlight-symbol-mode)
    (which-function-mode))
  (add-hook 'prog-mode-hook 'w--prog-mode-hook))


;;;; major-mode: c

(use-package cc-mode
  :defer t
  :config
  (defun w--c-mode-hook ()
    (setq evil-shift-width 2)
    (evil-swap-keys-swap-double-single-quotes)
    (evil-swap-keys-swap-square-curly-brackets)
    (evil-swap-keys-swap-underscore-dash))
  (add-hook 'c-mode-hook 'w--c-mode-hook))


;;;; major-mode: compilation and comint

(use-package compile
  :defer t
  :config
  (setq compilation-always-kill t)
  (evil-define-key* '(motion normal) compilation-mode-map
    (kbd "C-e") 'compilation-previous-error
    (kbd "C-n") 'compilation-next-error
    (kbd "C-p") 'compilation-previous-error)

  (defun w--compilation-mode-hook ()
    (w--hide-trailing-whitespace)
    (w--set-major-mode-hydra #'w--hydra-compilation/body))
  (add-hook 'compilation-mode-hook #'w--compilation-mode-hook)

  (defun w--compilation-finished (buffer status)
    (with-current-buffer buffer
      (evil-normal-state)))
  (add-hook 'compilation-finish-functions #'w--compilation-finished)

  (w--make-hydra w--hydra-compilation nil
    "compilation"
    "_r_ecompile"
    ("r" recompile)))

(use-package comint
  :defer t
  :ensure nil
  :config
  (setq comint-move-point-for-output 'all)
  (add-hook 'comint-mode-hook #'w--compilation-mode-hook)
  (evil-set-initial-state 'comint-mode 'insert)
  ;; fixme use :bind
  (defun w--comint-goto-end ()
    (interactive)
    (goto-char (point-max))
    (call-interactively #'evil-append))
  (define-key comint-mode-map
    (kbd "ESC") #'evil-normal-state)
  (evil-define-key*
   'normal comint-mode-map
   (kbd "RET") 'w--comint-goto-end
   (kbd "C-e") 'comint-previous-prompt
   (kbd "C-n") 'comint-next-prompt
   (kbd "C-p") 'comint-previous-prompt)
  (evil-define-key*
   'insert comint-mode-map
   (kbd "RET") 'comint-send-input
   (kbd "C-n") 'comint-next-input
   (kbd "C-p") 'comint-previous-input))


;;;; major mode: docker

(use-package dockerfile-mode
  :defer t
  :mode "Dockerfile[-_\\.].*")


;;;; major mode: emacs lisp

(use-package elisp-mode
  :defer t
  :ensure nil
  :config
  (require 'flycheck-package)
  (defun w--emacs-lisp-mode-hook ()
    (setq evil-shift-width 2)
    (w--set-major-mode-hydra #'w--hydra-emacs-lisp/body)
    ;; (evil-cleverparens-mode)  ;; fixme: useless with colemak
    (rainbow-delimiters-mode))
  (add-hook 'emacs-lisp-mode-hook 'w--emacs-lisp-mode-hook)
  (w--make-hydra w--hydra-emacs-lisp nil
    "elisp"
    "_b_ eval-buffer"
    ("b" eval-buffer)
    "_d_ eval-defun"
    ("d" eval-defun)
    "_e_val-last-sexp"
    ("e" eval-last-sexp)
    "_h_elp"
    ("h" helpful-at-point)
    "_m_acro-expand"
    ("m" pp-macroexpand-last-sexp)
    "_r_ eval-region"
    ("r" eval-region)))

(use-package eldoc
  :defer t
  :delight)

(use-package flycheck-package
  :defer t
  :config
  (flycheck-package-setup))


;;;; major mode: git related

(use-package gitattributes-mode
  :defer t)

(use-package gitconfig-mode
  :defer t)

(use-package gitignore-mode
  :defer t)


;;;; major mode: help

(use-package help-mode
  :defer t
  :ensure nil
  :config
  (define-key help-mode-map "q" nil))

;;;; major mode: jinja

(use-package jinja2-mode
  :defer t
  :mode "\\.j2\\'")


;;;; major mode: json

(use-package json-mode
  :defer t
  :config
  (defun w--json-mode-hook ()
    (setq
     tab-width 2
     json-reformat:indent-width tab-width
     js-indent-level 2
     evil-shift-width tab-width)
    (w--set-major-mode-hydra #'w--hydra-json/body)
    (evil-swap-keys-swap-colon-semicolon)
    (evil-swap-keys-swap-double-single-quotes))
  (add-hook 'json-mode-hook #'w--json-mode-hook)
  (w--make-hydra w--hydra-json nil
    "json"
    "_p_ pretty-print"
    ("p" (progn
           (json-pretty-print-buffer-ordered)
           (goto-char (point-min))))))


;;;; major mode: markdown

(use-package markdown-mode
  :defer t
  :config
  (setq markdown-asymmetric-header t)
  (defun w--markdown-mode-hook ()
    (setq
     evil-shift-width 2
     fill-column 999999)
    (w--set-major-mode-hydra #'w--hydra-markdown/body)
    (evil-swap-keys-swap-question-mark-slash)
    (evil-define-key*
     'insert markdown-mode-map
     "'" #'w--typo-cycle-quotation-marks)
    (make-variable-buffer-local 'typo-mode-map)
    (define-key typo-mode-map "`" nil))
  (add-hook 'markdown-mode-hook 'w--markdown-mode-hook)
  (evil-declare-repeat 'markdown-promote)
  (evil-declare-repeat 'markdown-demote)
  (w--make-hydra w--hydra-markdown nil
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
    ("n" markdown-cleanup-list-numbers)))


;;;; major mode: latex

;; fixme: optional auctex?
(setq TeX-engine 'xetex)


;;;; major mode: org

(use-package org
  :defer t
  :config
  (defun w--org-mode-hook ()
    (evil-org-mode))
  (add-hook 'org-mode-hook 'w--org-mode-hook)
  :custom
  (org-ellipsis " [...]"))

(use-package evil-org
  :defer t
  :after org
  :delight
  :config
  (setq
   evil-org-movement-bindings
   '((left . "h")
     (down . "n")
     (up . "e")
     (right . "i")))
  (setq evil-org-retain-visual-state-on-shift t)
  (evil-org-set-key-theme))


;;;; major mode: python

(use-package python
  :defer t
  :interpreter ("python" . python-mode)
  :config

  (dolist (open '("(" "{" "["))
    (sp-local-pair
     'python-mode open nil
     :unless '(sp-point-before-word-p)))

  (defun w--python-mode-hook ()
    (setq fill-column 72)
    (w--set-major-mode-hydra #'w--hydra-python/body)
    (evil-swap-keys-swap-colon-semicolon)
    (evil-swap-keys-swap-underscore-dash)
    (outline-minor-mode)
    (python-docstring-mode))

  (add-hook 'python-mode-hook 'w--python-mode-hook)

  (evilem-make-motion
   w--easymotion-python
   (list
    ;; Collect interesting positions around point, and all visible
    ;; blocks in the window. Results are ordered: forward after point,
    ;; then backward from point.
    'python-nav-end-of-statement 'python-nav-end-of-block 'python-nav-forward-block
    'python-nav-beginning-of-statement 'python-nav-beginning-of-block 'python-nav-backward-block)
   :pre-hook (setq evil-this-type 'line))

  ;; (evil-define-key* 'motion python-mode-map
  ;;   (kbd "SPC TAB") 'w--easymotion-python)

  (defun w--swiper-python-definitions ()
    (interactive)
    (swiper "^\\s-*\\(def\\|class\\)\\s- "))

  ;; (evil-define-key* 'motion python-mode-map
  ;;   (kbd "SPC /") 'w--swiper-python-definitions)

  (evil-define-operator w--evil-join-python (beg end)
    "Like 'evil-join', but handles comments and some continuation styles sensibly."
    :motion evil-line
    (evil-join beg end)
    (let ((first-line-is-comment (save-excursion
                                   (evil-first-non-blank)
                                   (looking-at-p "#")))
          (joined-line-is-comment (looking-at " #")))
      (cond
       (joined-line-is-comment
        (if first-line-is-comment
            ;; remove # when joining two comment lines
            (delete-region (point) (match-end 0))
          ;; pep8 mandates two spaces before inline comments
          (insert " ")
          (forward-char)))
       ((looking-at-p " \\\.")
        ;; remove space when the joined line starts with period, which
        ;; is a sensible style for long chained api calls, such as
        ;; sqlalchemy queries:
        ;;   query = (
        ;;       query
        ;;       .where(...)
        ;;       .limit(...)
        ;;       .offset(...))
        (delete-region (point) (1+ (point))))
       ((and (looking-at-p "[)}\\]") (looking-back ","))
        ;; remove trailing comma (e.g. after last function argument)
        ;; when joining any closing paren from the next line.
        (delete-char -1)))))

  (evil-define-key* 'normal python-mode-map
    [remap evil-join] 'w--evil-join-python)

  (use-package evil-text-object-python
    :config
    (defun w--evil-forward-char-or-python-statement (count)
      "Intelligently pick a statement or a character."
      (interactive "p")
      (cond
       ((eq this-command 'evil-change)
        (evil-text-object-python-inner-statement count))
       ((memq this-command '(evil-delete evil-shift-left evil-shift-right))
        (evil-text-object-python-outer-statement count))
       (t (evil-forward-char count))))
    (evil-define-key* '(operator visual) python-mode-map
      "ul" 'evil-text-object-python-inner-statement
      "al" 'evil-text-object-python-outer-statement)
    (evil-define-key* 'operator python-mode-map
      [remap evil-forward-char] 'w--evil-forward-char-or-python-statement))

  (defun w--python-insert-statement (position statement)
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

  (defun w--python-insert-pdb-trace (pdb-module)
    "Insert a pdb trace statement using PDB-MODULE before the current statement."
    (w--python-insert-statement
     'before
     (format "import %s; %s.set_trace()  # FIXME" pdb-module pdb-module)))

  (defun w--python-insert-ipython-repl (position)
    "Insert an IPython repl statement before the current statement."
    (w--python-insert-statement
     position
     (format "import IPython; IPython.embed()  # FIXME")))

  (defun w--python-refactor-make-variable (beg end)
    "Refactor the current region into a named variable."
    (interactive "r")
    (let ((name (read-string "Variable name: "))
          (code (delete-and-extract-region beg end)))
      (insert name)
      (w--python-insert-statement
       'before
       (format "%s = %s" name code))))

  (defun w--python-insert-import-statement ()
    "Add an import statement for the thing at point."
    (interactive)
    (let ((thing (w--thing-at-point-dwim)))
      (w--python-insert-statement
       'before
       (format "import %s" thing))))

  (require 'w--pytest)
  (evil-set-initial-state 'w--pytest-mode 'insert)
  (add-hook 'w--pytest-finished-hooks #'evil-force-normal-state)

  (w--make-hydra w--hydra-python nil
    "python"
    "_b_reakpoint"
    ("b" (w--python-insert-pdb-trace "pdb") nil)
    ("B" (w--python-insert-pdb-trace "ipdb") nil)
    "_i_mport"
    ("i" w--python-insert-import-statement nil)
    "_l_ multi-line"
    ("l" multi-line nil)
    ("L" multi-line-single-line nil)
    "_r_epl"
    ("r" (w--python-insert-ipython-repl 'after) nil)
    ("R" (w--python-insert-ipython-repl 'before) nil)
    "_t_ pytest"
    ("t" w--pytest nil)
    ("T" (w--pytest t) nil)
    "_v_ariable"
    ("v" w--python-refactor-make-variable nil))

  (evil-define-key* 'insert python-mode-map
    (kbd "C-l") 'multi-line)

  (evil-define-key* '(operator visual) python-mode-map
    "H" 'python-nav-backward-sexp-safe
    ;; "L" 'python-nav-forward-sexp-safe  ;; qwerty
    "I" 'python-nav-forward-sexp-safe)

  (evil-define-key* 'normal python-mode-map
    [backspace] 'python-nav-backward-up-list))

(use-package python-docstring
  :defer t
  :delight
  :custom
  (python-fill-docstring-style 'symmetric))

(use-package pip-requirements
  :defer t
  :config
  ;; avoid network traffic when opening a requirements.txt file
  (setq pip-packages '(this is a fake package listing)))

(use-package cython-mode
  :defer t
  :config
  (require 'flycheck-cython))

(use-package flycheck-cython
  :defer t)


;;;; major-mode: restructuredtext

(use-package rst
  :defer t
  :config
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

  (defun w--rst-mode-hook ()
    (setq
     evil-shift-width 2
     rst-mode-abbrev-table nil)
    (w--set-major-mode-hydra #'w--hydra-rst/body)
    (modify-syntax-entry ?_ "w")
    (evil-swap-keys-swap-question-mark-slash)
    (typo-mode)
    (evil-define-key*
     'insert rst-mode-map
     "'" #'w--typo-cycle-quotation-marks)
    (make-variable-buffer-local 'typo-mode-map)
    (define-key typo-mode-map "`" nil))
  (add-hook 'rst-mode-hook 'w--rst-mode-hook)

  (evilem-make-motion
   w--easymotion-rst
   (list 'rst-forward-section 'rst-backward-section)
   :pre-hook (setq evil-this-type 'line))
  ;; (evil-define-key* 'motion rst-mode-map
  ;;   (kbd "SPC TAB") 'w--easymotion-rst)

  (w--make-hydra w--hydra-rst nil
    "restructuredtext"
    "_a_djust"
    ("a" rst-adjust)))


;;;; major-mode: shell

(use-package sh-script
  :defer t
  :mode
  ("bashrc\\'" . sh-mode)
  ("\\.bashrc-.*\\'" . sh-mode)
  :config
  (defun w--sh-mode-hook ()
    (evil-swap-keys-swap-pipe-backslash))
  (add-hook 'sh-mode-hook 'w--sh-mode-hook))


;;;; major-mode: yaml

(use-package yaml-mode
  :defer t
  :config
  (defun w--yaml-mode-hook ()
    (setq evil-shift-width yaml-indent-offset)
    (evil-swap-keys-swap-colon-semicolon)
    (evil-swap-keys-swap-double-single-quotes))
  (add-hook 'yaml-mode-hook 'w--yaml-mode-hook))


;;;; local configuration (not in version control)

(load "~/.emacs.d/init-local" t)


(provide 'init)
;;; init.el ends here
