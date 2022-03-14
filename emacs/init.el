;;; init.el --- emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs configuration

;;; Code:

;;; Startup

(setq load-prefer-newer t)

;; Reduce garbage collection during startup.
(defvar w--original-gc-cons-threshold gc-cons-threshold
  "Original ‘gc-cons-threshold’ value.")

(defun w--reset-gc-cons-threshold ()
  "Reset the original ‘gc-cons-threshold’ value."
  (setq gc-cons-threshold w--original-gc-cons-threshold))

(setq gc-cons-threshold (* 100 1024 1024))
(add-hook 'emacs-startup-hook #'w--reset-gc-cons-threshold)


;;; Packages

;; Make everything relative to where this file actually lives.
(setq user-emacs-directory
      (concat "~/" (file-name-directory (file-symlink-p user-init-file))))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(eval-and-compile
  (require 'package))

(setq
 package-archives '(("melpa" . "https://melpa.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/")
                    ("gnu" . "https://elpa.gnu.org/packages/"))
 package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package use-package
  :custom
  (use-package-always-ensure t)
  (use-package-compute-statistics t)
  (use-package-enable-imenu-support t)
  (use-package-hook-name-suffix nil))

(use-package auto-compile
  :custom
  (auto-compile-update-autoloads t)
  :config
  (auto-compile-on-load-mode))

(use-package quelpa
  :custom
  (quelpa-update-melpa-p nil))

(use-package quelpa-use-package)


;;; Helpers

(use-package benchmark-init
  :demand
  :hook (after-init-hook . benchmark-init/deactivate)
  :config
  (benchmark-init/activate))

(use-package crux)

(use-package dash
  :config
  (global-dash-fontify-mode))

(use-package fn)

(use-package general)

(use-package no-littering)

(use-package s)

(use-package emacs
  :custom
  (tls-checktrust 'ask))

(eval-and-compile
  (defmacro w--ilambda (&rest body)
    "Concisely create a lambda with an ‘(interactive)’ spec.

BODY is normal function body. However, if the first expression is
a string literal, it will be used as an argument for (interactive),
and BODY can refer to it as ‘arg’."
    (let ((interactive-spec))
      (when (stringp (car body))
        (setq interactive-spec (list (car body))
              body (cdr body)))
      `(lambda (&optional arg)
         (interactive ,@interactive-spec)
         ,@body))))


;;; Environment

(use-package direnv
  :after exec-path-from-shell
  :config
  (direnv-mode))

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(use-package gsettings
  :config
  (gsettings-apply-gnome-settings))

(defvar w--ui-font-family "Sans"
  "Name of the font-family used by the desktop environment's user interface.")

(use-package emacs
  ;; gnome specific
  :if (and window-system (string-equal (getenv "XDG_CURRENT_DESKTOP") "GNOME"))
  :config
  (let* ((font-name
          (gsettings-get "org.gnome.desktop.interface" "font-name"))
         (font-name-without-size
          (s-replace-regexp "\\(.*\\) [0-9.]+" "\\1" font-name)))
    (setq w--ui-font-family font-name-without-size)))

(use-package emacs
  ;; os-x specific
  :if (and window-system (eq system-type 'darwin))
  :general
  ("s-q" nil)
  :custom
  (ns-right-alternate-modifier 'none)
  (ns-use-native-fullscreen nil))

(use-package server
  :if window-system
  :config
  (unless (server-running-p)
    (server-start)))


;;; Basics

(use-package emacs
  :hook (emacs-startup-hook . w--load-custom-file)
  :custom
  (disabled-command-function nil)
  (echo-keystrokes 0.5)
  (inhibit-startup-screen t)
  (initial-major-mode 'text-mode)
  (initial-scratch-message nil)

  :config
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))

  (defun w--load-custom-file ()
    "Load the file with automatically saved customization settings."
    (load custom-file 'noerror)))

(use-package agitprop
  :load-path "lisp/"
  :config
  (agitprop-resist))

(use-package edit-server
  :disabled
  ;; this is used by the ‘edit with emacs’ chrome extension:
  ;; https://chrome.google.com/webstore/detail/edit-with-emacs/ljobjlafonikaiipfkggjbhkghgicgoh
  :if window-system
  :config
  (edit-server-start)
  (add-to-list
   'edit-server-url-major-mode-alist
   '("github\\.com" . markdown-mode)))

(use-package savehist
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

;;; Key bindings and menus

(use-package hydra
  :after ivy
  :demand t
  :preface
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
         ("C-g" nil :exit t)
         ("<escape>" nil :exit t)))))

(use-package magit-popup
  :custom
  (magit-popup-show-help-echo nil))

(use-package transient
  :demand t
  :custom
  (transient-show-popup 3)
  (transient-display-buffer-action '(display-buffer-below-selected))
  :general
  ;; Key mapping approach is based on what transient-bind-q-to-quit does.
  (:keymaps 'transient-base-map
   "<escape>" #'transient-quit-one
   "C-p" #'transient-history-prev
   "C-n" #'transient-history-next)
  (:keymaps 'transient-sticky-map
   "<escape>" #'transient-quit-seq)
  (:keymaps 'transient-map
   "<tab>" #'transient-show))

(use-package which-key
  :delight
  :config
  (which-key-mode))


;;; Buffers, files, directories

(use-package emacs
  :custom
  (create-lockfiles nil)
  (find-file-visit-truename t)
  (make-backup-files nil)
  (use-file-dialog nil)
  :config
  (setq
   auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-directory-alist
        `((".*" ,(no-littering-expand-var-file-name "backup/") t))))

(use-package desktop
  :custom
  (desktop-restore-eager 5)
  (desktop-auto-save-timeout 10)
  :config
  (desktop-save-mode)
  (add-to-list 'desktop-globals-to-save 'swiper-history)
  (add-to-list 'desktop-globals-to-clear 'swiper-history))

(use-package recentf
  :custom
  (recentf-auto-cleanup 300)
  (recentf-max-saved-items 500)
  :config
  (defvar
    w--recentf-ignore-dirs
    (list
     no-littering-etc-directory
     no-littering-var-directory)
    "Directories to ignore in recentf listings.")

  (--each w--recentf-ignore-dirs
    (add-to-list 'recentf-exclude (concat (regexp-quote it) ".*")))

  (defun w--counsel-recentf-other-window ()
    "Like `counsel-recentf', but opens the file in another window."
    (interactive)
    (require 'ivy)
    (defvar ivy-inhibit-action)
    (let ((ivy-inhibit-action t))
      (find-file-other-window (counsel-recentf)))))

(use-package ranger
  :after (dired evil)
  :demand t

  :custom
  (ranger-cleanup-eagerly t)
  (ranger-deer-show-details nil)
  (ranger-excluded-extensions nil)
  (ranger-override-dired 'deer)
  (ranger-max-tabs 1)
  (ranger-show-hidden t)

  :general
  (:keymaps 'ranger-mode-map
   "h" #'ranger-up-directory
   "n" #'ranger-next-file
   "e" #'ranger-prev-file
   ;; "i" #'ranger-find-file
   "i" #'w--ranger-find-directory
   "k" #'ranger-search-next
   "K" #'ranger-search-previous
   "q" nil
   "'" nil
   "/" #'ranger-search)

  :hook (ranger-mode-hook . w--evil-colemak-basics-disable)

  :config
  (with-eval-after-load 'direnv
    (add-to-list 'direnv-non-file-modes 'ranger-mode))

  ;; fixme: is using auxiliary keymap correct?
  (evil-set-auxiliary-keymap ranger-mode-map 'motion ranger-mode-map)
  (defun w--ranger-find-directory ()
    (interactive)
    (let ((name (dired-get-filename nil t)))
      (if (file-directory-p name)
          (ranger-find-file name)
        (user-error "Not a directory")))))

(use-package sudo-edit
  :defer t
  :commands w--sudo-find-file
  :config
  (defun w--sudo-find-file ()
    (interactive)
    (setq current-prefix-arg t)
    (call-interactively 'sudo-edit)))

(use-package terminal-here
  :defer t
  :custom
  (terminal-here-project-root-function 'projectile-project-root)
  (terminal-here-terminal-command 'w--terminal-here-terminal-command)
  (terminal-here-command-flag "-x")

  :config
  (defun w--terminal-here-terminal-command (dir)
    (if (string-equal (getenv "XDG_CURRENT_DESKTOP") "GNOME")
        '("gnome-terminal")
      (terminal-here-default-terminal-command dir))))

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
           (not (member major-mode '(dired-mode ranger-mode)))
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

(defun w--open-gui-file-browser ()
  "Open a GUI browser for the directory containing the current file."
  (interactive)
  (when-let ((file-name (buffer-file-name))
             (directory-name (file-name-directory file-name))
             (file-exists (file-exists-p directory-name)))
    (call-process "xdg-open" nil 0 nil directory-name)))

(transient-define-prefix w--buffer-dispatch ()
  ["buffer"
   [("b" "switch" ivy-switch-buffer)
    ("B" "switch ↗" ivy-switch-buffer-other-window)]
   [("n" "new" evil-buffer-new)
    ("N" "new ↗" w--evil-buffer-new-other-window) ]
   [("c" "clone" clone-indirect-buffer)
    ("C" "clone ↗" clone-indirect-buffer-other-window)]
   [("e" "rename" crux-rename-file-and-buffer)
    ("E" "rename buf only" rename-buffer)]]
  ["hiding/closing"
   [("h" "hide" bury-buffer)
    ("H" "unhide" unbury-buffer)]
   [("k" "kill" kill-this-buffer)
    ("K" "kill+window" kill-buffer-and-window)]]
  ["misc"
   [("m" "switch major mode" w--switch-major-mode)
    ("r" "revert" revert-buffer)]])

(transient-define-prefix w--file-dispatch ()
  ["file"
   [("f" "file" counsel-find-file)
    ("F" "file ↗" find-file-other-window)]
   [("n" "new" evil-buffer-new)
    ("N" "new ↗" w--evil-buffer-new-other-window)]
   [("r" "recent" counsel-recentf)
    ("R"  "recent ↗" w--counsel-recentf-other-window)]
   [("s" "sudoedit" sudo-edit)
    ("S" "sudoedit other" w--sudo-find-file)]]
  ["misc"
   [("d" "directory" deer)
    ("D" "directory ↗" deer-jump-other-window)]
   [("!" "terminal" terminal-here)
    ("1" "terminal" terminal-here)]
   [("g" "gui browser" w--open-gui-file-browser)
    ("i" "insert" insert-file)]])


;;;; theme

(defvar w--dark-theme 'solarized-selenized-dark "The preferred dark theme.")
(defvar w--light-theme 'solarized-selenized-light "The preferred light theme.")

(use-package solarized-theme
  :demand t
  ;; TODO
  ;; :if (display-graphic-p)
  :hook (emacs-startup-hook . w--set-theme-from-environment)
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

  :config
  (load-theme w--dark-theme t t)
  (load-theme w--light-theme t t)
  (enable-theme w--light-theme)
  (defvar solarized-color-yellow    "#b58900")
  (defvar solarized-color-orange    "#cb4b16")
  (defvar solarized-color-red       "#dc322f")
  (defvar solarized-color-magenta   "#d33682")
  (defvar solarized-color-violet    "#6c71c4")
  (defvar solarized-color-blue      "#268bd2")
  (defvar solarized-color-cyan      "#2aa198")
  (defvar solarized-color-green     "#859900")
  (defvar solarized-color-yellow-d  "#7B6000")
  (defvar solarized-color-yellow-l  "#DEB542")
  (defvar solarized-color-orange-d  "#8B2C02")
  (defvar solarized-color-orange-l  "#F2804F")
  (defvar solarized-color-red-d     "#990A1B")
  (defvar solarized-color-red-l     "#FF6E64")
  (defvar solarized-color-magenta-d "#93115C")
  (defvar solarized-color-magenta-l "#F771AC")
  (defvar solarized-color-violet-d  "#3F4D91")
  (defvar solarized-color-violet-l  "#9EA0E5")
  (defvar solarized-color-blue-d    "#00629D")
  (defvar solarized-color-blue-l    "#69B7F0")
  (defvar solarized-color-cyan-d    "#00736F")
  (defvar solarized-color-cyan-l    "#69CABF")
  (defvar solarized-color-green-d   "#546E00")
  (defvar solarized-color-green-l   "#B4C342"))

(defun w--toggle-dark-light-theme ()
  "Toggle between a dark and light theme."
  (interactive)
  (w--activate-theme (eq (-first-item custom-enabled-themes) w--light-theme)))

(defvar w--theme-changed-hook nil
  "Hook to run after the theme has changed. Useful for patching font faces.")

(defun w--activate-theme (dark)
  "Load configured theme. When DARK is nil, load a light theme."
  (setq frame-background-mode (if dark 'dark 'light))
  (mapc 'frame-set-background-mode (frame-list))
  ;; (set-frame-parameter nil 'background-mode (if dark 'dark 'light))
  (--each custom-enabled-themes
    (disable-theme it))
  (let ((theme (if dark w--dark-theme w--light-theme)))
    (enable-theme theme))
  (run-hooks 'w--theme-changed-hook)
  (run-with-idle-timer .5 nil #'redraw-display))

(defvar w--faces-bold '(magit-popup-argument)
  "Faces that may retain their bold appearance.")

(defun w--tweak-faces ()
  "Tweak some font faces."
  (set-face-attribute
   'fixed-pitch nil
   :family (face-attribute 'default :family))
  (dolist (face (face-list))
    (unless (member face w--faces-bold)
      (set-face-attribute face nil :weight 'normal))))

(add-hook 'w--theme-changed-hook #'w--tweak-faces)

(defun w--set-theme-from-environment ()
  "Set the theme based on presence/absence of a configuration file."
  (interactive)
  (w--activate-theme (file-exists-p "~/.config/dark-theme")))

(defun w--tweak-evil-cursor ()
  "Tweak the appearance of the evil cursors"
  (setq
   evil-motion-state-cursor (list solarized-color-yellow 'box)
   evil-normal-state-cursor (list solarized-color-yellow 'box)
   evil-visual-state-cursor (list solarized-color-yellow 'hollow)
   evil-insert-state-cursor  (list solarized-color-yellow 'bar)
   evil-replace-state-cursor (list solarized-color-magenta 'hbar)
   evil-operator-state-cursor (list solarized-color-magenta 'hollow)))

(add-hook 'w--theme-changed-hook #'w--tweak-evil-cursor)


;;;; fonts

(use-package default-text-scale
  :demand t
  :if (display-graphic-p)
  :hook (after-init-hook . w--default-text-scale-reset)
  :general
  (:states 'motion
   "C-0" 'w--default-text-scale-reset
   "C--" 'default-text-scale-decrease
   "C-=" 'default-text-scale-increase
   "C-<mouse-4>" #'default-text-scale-increase
   "C-<mouse-5>" #'default-text-scale-decrease)

  :config
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
    (default-text-scale-increment (- height (face-attribute 'default :height)))))

(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))


;;;; mode line

(use-package delight)

(use-package nyan-mode
  :defer t)

(use-package smart-mode-line
  :custom
  (sml/line-number-format "%l")
  (sml/col-number-format "%c")
  (sml/mode-width 0)
  (sml/shorten-modes nil)
  (sml/modified-char "‼")
  (sml/name-width '(1 . 40))
  (sml/projectile-replacement-format "%s:")
  (sml/use-projectile-p 'before-prefixes)

  :hook (w--theme-changed-hook . w--smart-mode-line-tweak-faces)

  :config
  (sml/setup)

  (defun w--smart-mode-line-tweak-faces ()
    (set-face-attribute 'mode-line nil :family w--ui-font-family :height 0.9)
    (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)
    (set-face-attribute 'header-line nil :background 'unspecified :inherit 'mode-line)
    (set-face-attribute 'sml/modified nil :foreground solarized-color-red)
    (set-face-attribute 'sml/filename nil :foreground solarized-color-blue)))

(use-package which-func
  :ensure nil
  :defer t
  :custom
  (which-func-unknown "")
  (which-func-modes nil)
  :custom-face
  (which-func ((t (:foreground unspecified))))
  :config
  (dolist (s '("[" "]"))
    (setq which-func-format (remove s which-func-format)))
  (which-function-mode))


;;;; evil and editing

(use-package undo-tree
  :delight
  :config
  (global-undo-tree-mode))

(use-package evil
  :demand t

  :init
  (setq
   evil-respect-visual-line-mode t
   evil-want-C-u-scroll t
   evil-want-C-w-in-emacs-state t
   evil-want-keybinding nil
   evil-want-integration t)

  :hook (evil-local-mode-hook . turn-on-undo-tree-mode)

  :custom
  (evil-cross-lines t)
  (evil-insert-state-message nil)
  (evil-shift-round nil)
  (evil-split-window-below t)
  (evil-undo-system 'undo-tree)
  (evil-vsplit-window-right t)

  ;; small state tag before position info, which smart-mode-line put elsewhere
  (evil-mode-line-format '(before . mode-line-front-space))
  (evil-emacs-state-tag "e ")
  (evil-insert-state-tag "i ")
  (evil-motion-state-tag "m ")
  (evil-normal-state-tag "  ")
  (evil-operator-state-tag "o ")
  (evil-replace-state-tag "r ")
  (evil-visual-state-tag "v ")

  :general
  (:states 'motion
   "<tab>" 'evil-toggle-fold
   "C-<tab>" 'evil-jump-forward
   ";" #'evil-ex
   "z e" #'evil-scroll-line-up
   "z n" #'evil-scroll-line-down
   "<mouse-6>" (w--ilambda "P" (evil-scroll-column-left (or arg 4)))
   "<mouse-7>" (w--ilambda "P" (evil-scroll-column-right (or arg 4))))
  (:states '(motion normal)
   [escape] #'w--evil-normal-state-cleanup)
  (:states '(motion normal visual)
   [remap evil-next-line] #'w--evil-next-line
   [remap evil-previous-line] #'w--evil-previous-line
   [remap evil-end-of-line] #'w--evil-end-of-line
   [remap evil-first-non-blank] #'w--evil-first-non-blank)
  (:states 'normal
   ;; useful for keychron k7 keyboards that require a Fn modifier for
   ;; tilde; use shift-tab (key below it) it as a workaround.
   "<backtab>" #'evil-invert-char)
  (:states '(operator visual)
   "o" #'w--evil-text-object-symbol-dwim)
  (:states 'operator
   ;; the empty text object is a trick to make it possible to
   ;; quickly swap two text objects using evil-exchange "gx";
   ;; "gxp" move previously marked text without moving anything
   ;; back to the original location, or vice versa.
   "p" #'w--evil-empty-text-object)
  (:keymaps 'evil-outer-text-objects-map
   "g" #'w--evil-text-object-whole-buffer)
  (:states 'insert
   "<return>" #'comment-indent-new-line
   "C-a" #'w--evil-first-non-blank
   "C-c" #'evil-normal-state
   "C-d" #'delete-char
   "C-e" #'end-of-visual-line
   "C-h" [backspace]
   "C-k" #'w--kill-line-dwim
   "C-m" #'comment-indent-new-line
   "C-n" #'next-line
   "C-o" #'evil-normal-state
   "C-p" #'previous-line
   "C-t" #'w--evil-transpose-chars
   "C-v" #'yank  ;; during typing, ctrl-v is "paste", like everywhere else
   "C-SPC" #'fixup-whitespace
   "C-'" #'w--typo-cycle-quotation-marks
   "C-," #'evil-shift-left-line  ;; shift line with < and > (same
   "C-<" #'evil-shift-left-line  ;; chars as in normal mode);
   "C-." #'evil-shift-right-line ;; used instead of standard vim
   "C->" #'evil-shift-right-line ;; bindings C-d and C-t.
   "C-=" (w--ilambda (save-excursion (call-interactively #'evil-indent-line))))
  (:states '(insert replace)
   (general-chord "qw") #'evil-normal-state
   (general-chord "wq") #'evil-normal-state
   "C-g" #'evil-normal-state)

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

  (defun w--evil-normal-state-cleanup ()
    "Like `evil-force-normal-state', with some extra cleanups."
    (interactive)
    (when (eq last-command 'w--evil-normal-state-cleanup)
      ;; clean up some more when called twice in row
      (lazy-highlight-cleanup t)
      (remove-overlays nil nil 'category 'evil-snipe)
      (symbol-overlay-remove-all)
      (when (functionp 'evil-mc-undo-all-cursors)
        (evil-mc-undo-all-cursors))
      (let ((inhibit-message t))
        (evil-exchange-cancel)))
    (evil-force-normal-state)
    (when (eq (evil-initial-state-for-buffer) 'motion)
      (evil-change-to-initial-state)))

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

  (evil-define-motion w--evil-next-line (count)
    (when (null count)
      (setq count 1))
    (if visual-line-mode
        (progn
          (setq evil-this-type 'exclusive)
          (evil-next-visual-line count))
      (setq evil-this-type 'line)
      (evil-next-line count)))

  (evil-define-motion w--evil-previous-line (count)
    (when (null count)
      (setq count 1))
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

  (evil-define-operator w--evil-join-smart-backslash-eol (beg end)
    "Like 'evil-join', but handles continuation line endings in a smarter way."
    :motion evil-line
    (prog1 (evil-join beg end)
      ;; delete ‘\’, and potentially one space before it
      (when (looking-back "\\\\" nil)
        (delete-char -1))
      (when (looking-back " " nil)
        (delete-char -1))))

  (evil-define-command w--split-line-backslash ()
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

  (evil-define-text-object w--evil-text-object-whole-buffer (count &optional _beg _end _type)
    "Text object for the whole buffer."
    (evil-range (point-min) (point-max) 'line))

  (evil-define-text-object w--evil-empty-text-object (count &optional _beg _end _type)
    "Empty text object."
    (evil-range (point) (point)))

  (evil-define-text-object w--evil-text-object-symbol-dwim (count &optional beg end type)
    "Intelligently pick evil-inner-symbol or evil-a-symbol."
    (if (memq this-command '(evil-delete lispyville-delete))
        (evil-a-symbol count beg end type)
      (evil-inner-symbol count beg end type))))

(use-package aggressive-indent
  :defer t
  :delight " ⇤")

(use-package drag-stuff
  :general
  (:states 'visual
   "C-h" #'w--evil-visual-shift-left
   "C-n" #'drag-stuff-down
   "C-e" #'drag-stuff-up
   "C-i" #'w--evil-visual-shift-right)
  :config
  (defun w--evil-visual-restore-line-wise ()
    (evil-normal-state)
    (evil-visual-restore)
    (evil-visual-line))
  (evil-define-operator w--evil-visual-shift-left (beg end &optional count)
    :type line
    (interactive "<r><vc>")
    (evil-shift-left beg end count)
    (w--evil-visual-restore-line-wise))
  (evil-define-operator w--evil-visual-shift-right (beg end &optional count)
    :type line
    (interactive "<r><vc>")
    (evil-shift-right beg end count)
    (w--evil-visual-restore-line-wise)))

(use-package edit-indirect
  :hook
  (edit-indirect-after-creation-hook . w--edit-indirect-dedent)
  (edit-indirect-before-commit-hook . w--edit-indirect-reindent)

  :general
  (:states 'normal
   "gb" #'w--evil-edit-indirect)
  (:keymaps 'edit-indirect-mode-map
   :states 'normal
   [remap evil-save-modified-and-close] #'edit-indirect-commit
   [remap evil-quit] #'edit-indirect-abort)

  :preface
  (defvar w--edit-indirect-original-indentation 0
    "Original indentation of the edited region.")
  (make-variable-buffer-local 'w--edit-indirect-original-indentation)

  :config
  (defun w--edit-indirect-dedent ()
    (require 'rst)
    (let ((indentation (rst-find-leftmost-column (point-min) (point-max))))
      (setq w--edit-indirect-original-indentation indentation)
      (when (> indentation 0)
        (indent-rigidly (point-min) (point-max) (- indentation)))))

  (defun w--edit-indirect-reindent ()
    (when (> w--edit-indirect-original-indentation 0)
      (indent-rigidly (point-min) (point-max) w--edit-indirect-original-indentation)))

  (evil-define-operator w--evil-edit-indirect (beg end _type)
    (interactive "<R>")
    (edit-indirect-region beg end t)))

(use-package evil-args
  :general
  (:keymaps 'evil-inner-text-objects-map
   "a" #'evil-inner-arg)
  (:keymaps 'evil-outer-text-objects-map
   "a" #'evil-outer-arg))

(use-package evil-colemak-basics
  :after evil evil-snipe
  :demand t
  :delight
  :init
  (setq evil-colemak-basics-char-jump-commands 'evil-snipe)
  :commands
  w--evil-colemak-basics-disable
  :config
  (global-evil-colemak-basics-mode)
  (defun w--evil-colemak-basics-disable ()
    (evil-colemak-basics-mode -1)))

(use-package evil-commentary
  :general
  (:states 'normal
   "gc" #'evil-commentary
   "gy" #'evil-commentary-yank))

(use-package evil-easymotion
  :general
  (:states 'motion
   "SPC" #'w--hydra-teleport/body)
  :commands
  evilem-make-motion
  evilem-make-motion-plain
  evilem-create
  evilem-create-plain
  evilem-define

  :config
  (w--make-hydra w--hydra-teleport nil
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
    ("L" w--avy-goto-line-any-window)
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
    ("h" w--easymotion-symbol-overlay)
    ("/" evil-avy-goto-char-timer)
    "_o_ new line"
    ("o" (progn (evil-avy-goto-line) (call-interactively 'evil-open-below)))
    ("O" (progn (evil-avy-goto-line) (call-interactively 'evil-open-above)))
    "_d_ delete"
    ("d" w--avy-evil-delete-line)
    ("D" w--avy-evil-delete-lines)
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
   '(w--hydra-teleport/evilem-motion-forward-word-begin-and-exit
     w--hydra-teleport/evilem-motion-forward-WORD-begin-and-exit
     w--hydra-teleport/evilem-motion-forward-word-end-and-exit
     w--hydra-teleport/evilem-motion-forward-WORD-end-and-exit
     w--hydra-teleport/evilem-motion-backward-word-begin-and-exit
     w--hydra-teleport/evilem-motion-backward-WORD-begin-and-exit
     w--hydra-teleport/evilem-motion-backward-word-end-and-exit
     w--hydra-teleport/evilem-motion-backward-WORD-end-and-exit
     w--hydra-teleport/evilem-motion-previous-line-and-exit
     w--hydra-teleport/evil-avy-goto-line-above-and-exit
     w--hydra-teleport/evilem-motion-next-line-and-exit
     w--hydra-teleport/evil-avy-goto-line-below-and-exit
     w--hydra-teleport/evil-avy-goto-line-and-exit
     w--hydra-teleport/evil-avy-goto-char-timer-and-exit
     w--hydra-teleport/w--easymotion-symbol-overlay-and-exit
     w--hydra-teleport/evilem-motion-find-char-and-exit
     w--hydra-teleport/evilem-motion-find-char-to-and-exit
     w--hydra-teleport/evilem-motion-find-char-backward-and-exit
     w--hydra-teleport/evilem-motion-find-char-to-backward-and-exit
     w--hydra-teleport/evilem-motion-search-next-and-exit
     w--hydra-teleport/evilem-motion-search-previous-and-exit
     w--hydra-teleport/evilem-motion-search-next-and-exit
     w--hydra-teleport/evilem-motion-search-previous-and-exit))

  ;; make the basic motions also work in evil operator state
  (defvar w--teleport-map (make-sparse-keymap)
    "Keymap with basic avy/easymotion jumps.")
  (general-define-key
   :keymaps 'w--teleport-map
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
   "h" #'w--easymotion-symbol-overlay
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
   "SPC" w--teleport-map)

  (evilem-make-motion
   w--easymotion-symbol-overlay
   (list
    'w--symbol-overlay-jump-next-any
    'w--symbol-overlay-jump-previous-any))
  (evilem-make-motion
   w--easymotion-symbol-overlay
   '(w--symbol-overlay-jump-next-any w--symbol-overlay-jump-previous-any))

  ;; todo: commented stuff below needs rethinking and cleaning up
  ;; (evil-define-key* 'normal global-map
  ;;   (kbd "SPC a") (lambda () (interactive) (avy-goto-char-timer) (call-interactively 'evil-append))
  ;;   (kbd "SPC A") (lambda () (interactive) (w--avy-evil-goto-end-of-line) (call-interactively 'evil-append-line))
  ;;   (kbd "SPC c") (lambda () (interactive) (avy-goto-line) (evil-first-non-blank) (call-interactively 'evil-change-line))
  ;;   (kbd "SPC C") 'w--avy-evil-change-region
  ;;   (kbd "SPC i") (lambda () (interactive) (avy-goto-char-timer) (call-interactively 'evil-insert))
  ;;   (kbd "SPC I") (lambda () (interactive) (avy-goto-line) (call-interactively 'evil-insert-line))
  ;;   (kbd "SPC $") 'w--avy-evil-goto-end-of-line)
  ;; (defun w--evil-end-of-next-line ()
  ;;   (interactive)
  ;;   (evil-next-line)
  ;;   (end-of-line))
  ;; (evilem-make-motion-plain
  ;;  w--avy-evil-goto-end-of-line
  ;;  (list 'evil-end-of-line 'w--evil-end-of-next-line)
  ;;  :pre-hook (setq evil-this-type 'line)
  ;;  :bind ((scroll-margin 0))
  ;;  :initial-point (goto-char (window-start)))
  ;; (defun w--avy-evil-change-region ()
  ;;   "Select two lines and change the lines between them."
  ;;   (interactive)
  ;;   (avy-with w--avy-evil-change-region
  ;;     (let* ((beg (progn (avy-goto-line) (point)))
  ;;            (end (save-excursion (goto-char (avy--line)) (forward-line) (point))))
  ;;       (evil-change beg end 'line nil nil))))

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

  (defun w--avy-evil-delete-lines ()
    "Select two lines and delete the lines between them."
    (interactive)
    (avy-with w--avy-evil-delete-lines
      (let* ((beg (avy--line))
             (end (save-excursion (goto-char (avy--line)) (forward-line) (point))))
        (evil-delete beg end 'line nil nil))))

  (defun w--avy-goto-line-any-window ()
    "Go to line in any visible window."
    (interactive)
    (setq current-prefix-arg 4)
    (call-interactively 'avy-goto-line)))

(use-package evil-exchange
  :custom
  (evil-exchange-highlight-face 'magit-diff-base)
  :general
  (:states '(normal visual)
   "gx" 'evil-exchange
   "gX" 'evil-exchange-cancel))

(use-package evil-goggles
  :demand
  :after magit
  :delight

  :custom
  (evil-goggles-duration 1)
  (evil-goggles-blocking-duration .2)
  (evil-goggles-pulse t)

  :custom-face
  (evil-goggles-default-face ((t (:inherit highlight))))

  :config
  (evil-goggles-mode)
  (evil-goggles-use-magit-faces)

  ;; See https://github.com/edkolev/evil-goggles/pull/26
  (defvar w--evil-goggles-lispyville-extra
    '((lispyville-yank                 :face evil-goggles-yank-face           :switch evil-goggles-enable-yank           :advice evil-goggles--generic-async-advice)
      (lispyville-delete               :face evil-goggles-delete-face         :switch evil-goggles-enable-delete         :advice evil-goggles--generic-blocking-advice)
      (lispyville-change               :face evil-goggles-change-face         :switch evil-goggles-enable-change         :advice evil-goggles--generic-blocking-advice)
      (lispyville-yank-line            :face evil-goggles-yank-face           :switch evil-goggles-enable-yank           :advice evil-goggles--generic-async-advice)
      (lispyville-delete-line          :face evil-goggles-delete-face         :switch evil-goggles-enable-delete         :advice evil-goggles--delete-line-advice)
      (lispyville-change-line          :face evil-goggles-change-face         :switch evil-goggles-enable-change         :advice evil-goggles--generic-blocking-advice)
      (lispyville-change-whole-line    :face evil-goggles-change-face         :switch evil-goggles-enable-change         :advice evil-goggles--generic-blocking-advice)
      (lispyville-join                 :face evil-goggles-join-face           :switch evil-goggles-enable-join           :advice evil-goggles--join-advice)
      (lispyville-comment-or-uncomment :face evil-goggles-nerd-commenter-face :switch evil-goggles-enable-nerd-commenter :advice evil-goggles--generic-async-advice)
      (lispyville-prettify             :face evil-goggles-indent-face         :switch evil-goggles-enable-indent         :advice evil-goggles--generic-async-advice)))
  (--each w--evil-goggles-lispyville-extra
    (add-to-list 'evil-goggles--commands it))

  (define-minor-mode w--shoulder-surf-mode
    "Minor mode to make it easier for others to see what's happening on the screen."
    :global t
    :lighter " 👀"
    (let ((on w--shoulder-surf-mode))
      (global-hl-line-mode (if on 1 -1))
      (setq
       evil-goggles-duration (if on 5 1)
       evil-goggles-blocking-duration (if on 1 .2))
      (set-face-attribute
       'evil-goggles-default-face nil
       :inherit (if on 'magit-diff-base 'highlight)))))

(use-package evil-indent-plus
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

(use-package evil-mc
  :disabled
  :general
  (:keymaps 'evil-colemak-basics-keymap
   :states 'visual
   "A" #'evil-mc-make-cursor-in-visual-selection-end
   "U" #'evil-mc-make-cursor-in-visual-selection-beg))

(use-package evil-numbers
  :general
  (:states 'normal
   "+" #'evil-numbers/inc-at-pt
   "-" #'evil-numbers/dec-at-pt))

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

(use-package evil-string-inflection
  :general
  (:keymaps 'normal
   "g~" #'evil-operator-string-inflection
   "g`" #'evil-operator-string-inflection))

;; todo: try out evil-embrace
(use-package evil-surround
  :general
  (:states 'operator
   "s" 'evil-surround-edit
   "S" 'evil-Surround-edit)
  (:states 'normal
   "S" 'evil-surround-region)
  (:states 'visual
   "S" 'evil-surround-region
   "gS" 'evil-Surround-region)

  :commands
  w--add-evil-surround-pairs

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
   ?\Q '("“" . "”")
   ;; ¿question? ¡answer!
   ?\? '("¿" . "?")
   ?\! '("¡" . "!"))
  (setq-default evil-surround-pairs-alist evil-surround-pairs-alist)
  (make-variable-buffer-local 'evil-surround-pairs-alist)

  (defun w--add-evil-surround-pairs (&rest args)
    (apply 'evil-add-to-alist 'evil-surround-pairs-alist args)))

(use-package evil-swap-keys
  :config
  (global-evil-swap-keys-mode)
  :delight " ↧")

(use-package evil-textobj-anyblock
  ;; todo perhaps replace with https://github.com/noctuid/targets.el
  :general
  (:keymaps 'evil-inner-text-objects-map
   "b" #'evil-textobj-anyblock-inner-block)
  (:keymaps 'evil-outer-text-objects-map
   "b" #'evil-textobj-anyblock-a-block))

(use-package evil-visualstar
  :general
  (:states 'visual
   "*" #'evil-visualstar/begin-search-forward
   "#" #'evil-visualstar/begin-search-backward))

(use-package expand-region
  :defer t
  :custom
  (expand-region-fast-keys-enabled nil)
  :general
  (:states 'visual
   "<tab>" #'w--hydra-expand-region/er/expand-region)
  :config
  (w--make-hydra w--hydra-expand-region (:foreign-keys run)
    "expand-region"
    "_<tab>_ expand"
    ("<tab>" er/expand-region :exit nil)
    "_u_ndo"
    ("u" (er/expand-region -1) :exit nil)
    "_r_eset"
    ("r" (er/expand-region 0) :exit t)))

(use-package fancy-narrow
  :defer t
  :commands
  w--narrow-dwim
  w--fancy-narrow-dwim
  :config
  (defun w--narrow-dwim ()
    "Narrow (or widen) to defun or region."
    (interactive)
    (when (fancy-narrow-active-p)
      (fancy-widen))
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
  (defun w--fancy-narrow-dwim ()
    "Fancy narrow (or widen) to defun or region."
    (interactive)
    (cond
     ((region-active-p)
      (fancy-widen)
      (fancy-narrow-to-region (region-beginning) (region-end))
      (deactivate-mark)
      (message "Showing region only"))
     ((fancy-narrow-active-p)
      (fancy-widen)
      (message "Showing everything"))
     (t
      (fancy-narrow-to-defun)
      (message "Showing defun only")))))

(use-package key-chord
  :config
  (key-chord-mode +1))

(use-package keyfreq
  :config
  (setq  ;; https://github.com/emacscollective/no-littering/pull/74
   keyfreq-file (no-littering-expand-var-file-name "keyfreq.el")
   keyfreq-file-lock (no-littering-expand-var-file-name "keyfreq.lock"))
  (keyfreq-mode)
  (keyfreq-autosave-mode))


;;;; scrolling

(setq
 indicate-buffer-boundaries 'left
 recenter-positions '(top middle bottom)
 scroll-conservatively 101
 scroll-margin 5)


;;;; whitespace

(use-package emacs
  :custom
  (require-final-newline 'visit-save)
  (sentence-end-double-space nil)
  :config
  (setq-default
   indent-tabs-mode nil
   tab-width 4)
  (unless (functionp 'indent-tabs-mode)
    ;; For some reason, indent-tabs-mode is not a real minor mode, but
    ;; the variable of the same name can be set/unset at will. That's
    ;; what a minor mode does as well, so define one with she same name.
    (define-minor-mode indent-tabs-mode
      "Minor mode to make up for an Emacs oddity."
      nil " ⇥" nil)))

(use-package whitespace
  :defer t
  :config)

(define-minor-mode w--show-trailing-whitespace-mode
  "Show or hide trailing whitespace."
  nil nil nil
  (setq show-trailing-whitespace w--show-trailing-whitespace-mode))

(use-package whitespace-cleanup-mode
  :delight
  '(:eval (unless whitespace-cleanup-mode-initially-clean " ⎵"))
  :config
  (global-whitespace-cleanup-mode))

(use-package indent-guide
  :defer t
  :delight " ⋮"
  :custom
  (indent-guide-char "·")
  (indent-guide-delay 1)
  (indent-guide-recursive t)
  (indent-guide-threshold 7)
  :custom-face
  (indent-guide-face
   ((t (:inherit font-lock-comment-face
        :foreground unspecified)))))


;;;; minibuffer

(use-package minibuffer
  :ensure nil
  :general
  (:keymaps 'minibuffer-local-map
   "C-w" #'backward-kill-word
   "C-u" #'kill-whole-line)
  (:keymaps '(minibuffer-local-completion-map
              minibuffer-local-isearch-map
              minibuffer-local-map
              minibuffer-local-must-match-map
              minibuffer-local-ns-map)
   "<escape>" #'minibuffer-keyboard-quit))


;;;; line navigation

(use-package display-line-numbers
  :defer t
  :commands
  w--display-line-numbers-cycle
  :config
  (defun w--display-line-numbers-cycle ()
    (interactive)
    (let* ((options '(t visual relative))
           (new-index (mod (1+ (or (-elem-index display-line-numbers options) -1)) (length options)))
           (new-value (nth new-index options)))
      (unless display-line-numbers-mode
        (display-line-numbers-mode))
      (message "Line numbering style: %s" new-value)
      (setq display-line-numbers new-value))))

(use-package nlinum
  :disabled
  :defer t)

(use-package nlinum-relative
  :disabled
  :defer t
  :custom-face
  (nlinum-relative-current-face
   ((t (:inherit nlinum
        :foreground unspecified
        :background unspecified
        :weight unspecified))))
  :commands
  w--line-numbers-cycle
  :config
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
      (nlinum-relative-mode -1)))))


;;;; search

(use-package emacs  ;; isearch
  :custom
  (isearch-allow-prefix nil)
  (isearch-forward t)  ;; initial direction; useful after swiper
  (lazy-highlight-cleanup nil)
  (lazy-highlight-initial-delay 0.5)
  (lazy-highlight-max-at-a-time nil)
  (search-default-mode t)
  :general
  (:keymaps 'isearch-mode-map
   "C-'" 'avy-isearch
   "C-/" 'swiper-isearch-toggle))

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

(use-package emacs  ;; replace
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
  (transient-define-prefix w--replace-dispatch nil
    ["replace"
     [("r" "dwim" w--query-replace-thing-at-point-dwim)
      ("s" "dwim" w--query-replace-thing-at-point-dwim)]
     [("p" "project" projectile-replace)
      ("P" "project (regexp)" projectile-replace-regexp)]
     [("q" "replace" query-replace)
      ("Q" "replace regexp"query-replace-regexp)]]))

(use-package emacs  ;; occur
  :hook (occur-mode-hook . w--occur-mode-hook)
  :general
  (:keymaps 'occur-mode-map
   :states '(motion normal)
   "RET" #'occur-mode-goto-occurrence
   "C-e" #'occur-prev
   "C-n" #'occur-next
   "C-p" #'occur-prev
   "g r" #'revert-buffer)
  :config
  (evil-set-initial-state 'occur-mode 'motion)
  (defun w--occur-mode-hook ()
    (toggle-truncate-lines t)
    (w--set-major-mode-hydra #'w--hydra-occur/body))

  (w--make-hydra w--hydra-occur nil
    "occur"
    "_n__e_ nav"
    ("n" occur-next :exit nil)
    ("e" occur-prev :exit nil)
    "_f_ollow"
    ("f" next-error-follow-minor-mode))

  (defun w--occur-dwim (&optional nlines)
    "Call `occur' with a sane default."
    (interactive "P")
    (let ((thing (read-string
                  "Open occur for regexp: "
                  (regexp-quote (or (w--thing-at-point-dwim) ""))
                  'regexp-history)))
      (occur thing nlines)
      (evil-normal-state))))

(use-package swiper
  :after ivy
  :custom
  (swiper-action-recenter t)
  (swiper-goto-start-of-match t)
  :general
  (:states 'motion
   "/" 'swiper-isearch
   "?" 'swiper
   "C-/" 'swiper-all)
  (:states 'visual
   "/" 'swiper-isearch
   "C-/" 'swiper-all)
  (:keymaps 'swiper-map
   "C-s" 'swiper-isearch-toggle)
  :config
  (defun w--swiper-dwim ()
    "Start `swiper` searching for the thing at point."
    (interactive)
    (let ((query (w--thing-at-point-dwim)))
      (when evil-visual-state-minor-mode
        ;; do not expand region in visual mode
        (evil-normal-state))
      (swiper query))))

(use-package ag
  :defer t
  :custom
  (ag-project-root-function 'projectile-project-root)
  (ag-reuse-buffers t)
  :hook (ag-mode-hook . w--ag-mode-hook)
  :commands
  w--hydra-ag/body
  w--counsel-ag-project
  w--counsel-ag-project-all-files
  :general
  (:keymaps 'ag-mode-map
   :states 'motion
   "gr" #'recompile)
  :config
  (defun w--ag-mode-hook ()
    (toggle-truncate-lines t))
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
    ("R" ag-regexp))

  (defun w--counsel-ag-project (&optional unrestricted)
    "Run ‘counsel-ag’ on the current project."
    (interactive)
    (let ((extra-args (if unrestricted "--unrestricted" ""))
          (prompt (if unrestricted "search all project files: " "search project files: ")))
      (counsel-ag nil (projectile-project-root) extra-args prompt)))

  (defun w--counsel-ag-project-all-files ()
    "Run counsel-ag on all files within the project root."
    (interactive)
    (w--counsel-ag-project t)))

;; todo: switch to rg/ripgrep
;; (use-package rg)

;; todo: switch to deadgrep
;; perhaps use ,/ for hydra?
(use-package deadgrep
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
  (w--make-hydra w--hydra-search nil
    "search"
    "_/_ search"
    ("/" deadgrep)))

(use-package symbol-overlay
  :demand t
  :delight
  :after beacon
  :hook (w--theme-changed-hook . w--symbol-overlay-tweak-faces)
  :custom
  (symbol-overlay-idle-time 1.0)

  :general
  (:states 'motion
   "C-p" #'symbol-overlay-jump-prev
   "C-n" #'symbol-overlay-jump-next)
  (:states 'normal
   "C-p" #'w--evil-paste-pop-or-previous-symbol
   "C-n" #'w--evil-paste-pop-next-or-next-symbol)

  :config
  (setq symbol-overlay-map (make-sparse-keymap))

  (defun w--symbol-overlay-tweak-faces ()
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

  (defun w--symbol-overlay-put-dwim ()
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

  (defun w--symbol-overlay-jump-any (direction)
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

  (defun w--symbol-overlay-jump-next-any ()
    (interactive)
    (w--symbol-overlay-jump-any 'forward))
  (add-to-list 'beacon-dont-blink-commands 'w--symbol-overlay-jump-next-any)

  (defun w--symbol-overlay-jump-previous-any ()
    (interactive)
    (w--symbol-overlay-jump-any 'backward))
  (add-to-list 'beacon-dont-blink-commands 'w--symbol-overlay-jump-previous-any)

  (defun w--symbol-overlay-jump-first ()
    (interactive)
    (unless (symbol-overlay-get-list)
      (user-error "No highlighted symbols"))
    (goto-char (point-min))
    (w--symbol-overlay-jump-next-any))

  (defun w--symbol-overlay-jump-last ()
    (interactive)
    (unless (symbol-overlay-get-list)
      (user-error "No highlighted symbols"))
    (goto-char (point-max))
    (w--symbol-overlay-jump-previous-any))

  (defun w--evil-paste-pop-or-previous-symbol (count)
    "Either paste-pop (with COUNT) or jump to previous symbol occurrence."
    (interactive "p")
    (condition-case nil
        (evil-paste-pop count)
      (user-error
       (symbol-overlay-jump-prev))))

  (defun w--evil-paste-pop-next-or-next-symbol (count)
    "Either paste-pop-next (with COUNT) or jump to next symbol occurrence."
    (interactive "p")
    (condition-case nil
        (evil-paste-pop-next count)
      (user-error
       (symbol-overlay-jump-next)))))

(use-package zeal-at-point
  :defer
  :general
  (:states 'motion
   "g/" 'w--zeal-at-point-dwim)
  :commands
  w--zeal-at-point-dwim
  :config
  (defun w--zeal-at-point-dwim ()
    "Open ‘zeal’, defaulting to the thing at point."
    (interactive)
    (->> (or (w--thing-at-point-dwim) "")
         (zeal-at-point-maybe-add-docset)
         (read-string "Zeal search: ")
         (zeal-at-point-run-search))))

;;;; previous/next navigation

;; previous/next thing (inspired by vim unimpaired)
;; todo: this should become a fancy hydra

(defun w--last-error ()
  "Jump to the last error; similar to 'first-error'."
  (interactive)
  (condition-case err (while t (next-error)) (user-error nil)))

(general-define-key
 :states '(motion normal)
 "[ SPC" (w--ilambda (save-excursion (evil-insert-newline-above)))
 "] SPC" (w--ilambda (save-excursion (evil-insert-newline-below)))
 "[b" 'evil-prev-buffer
 "]b" 'evil-next-buffer
 "[c" 'flycheck-previous-error
 "]c" 'flycheck-next-error
 "[C" 'flycheck-first-error
 "]C" 'w--flycheck-last-error
 "[d" 'w--diff-hl-previous-hunk
 "]d" 'w--diff-hl-next-hunk
 "[e" 'previous-error
 "]e" 'next-error
 "[E" 'first-error
 "]E" 'w--last-error
 "[h" 'w--symbol-overlay-jump-previous-any
 "]h" 'w--symbol-overlay-jump-next-any
 "[H" 'w--symbol-overlay-jump-first
 "]H" 'w--symbol-overlay-jump-last
 "[m" 'smerge-prev
 "]m" 'smerge-next
 "[o" 'symbol-overlay-jump-prev
 "]o" 'symbol-overlay-jump-next
 "]s" (w--ilambda
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
(w--make-hydra w--hydra-navigation-forward nil
  "nav"
  ("[" w--hydra-navigation-backward/body)
  ("]" nil)
  "_b_uffer"
  ("b" evil-next-buffer)
  ("B" evil-next-buffer :exit nil)
  ("[" w--hydra-navigation-backward/body)
  "_z_ folds"
  ("z" origami-forward-fold)
  ("Z" origami-forward-fold :exit nil))

(w--make-hydra w--hydra-navigation-backward nil
  "nav"
  ("[" nil)
  ("]" w--hydra-navigation-forward/body)
  "_b_uffer"
  ("b" evil-prev-buffer)
  ("B" evil-prev-buffer :exit nil)
  "_z_ folds"
  ("z" origami-backward-fold-same-level)
  ("Z" origami-backward-fold-same-level :exit nil))

;;;; parens

(use-package smartparens
  :delight " )"
  :config
  (require 'smartparens-config)
  (smartparens-global-mode)
  (show-smartparens-global-mode))

(use-package rainbow-delimiters
  :defer t)

(use-package highlight-parentheses
  :defer t
  :delight)

(use-package syntactic-close
  :general
  (:states 'insert
   ;; this is a zero, i.e. C-) without shift
   "C-0" #'syntactic-close))


;;;; text wrapping and filling

(use-package emacs
  :delight
  (auto-fill-function " ↲")
  (visual-line-mode (:eval (unless w--wrap-lines-mode " ⇉"))))

(use-package adaptive-wrap
  :hook (visual-line-mode-hook . w--maybe-activate-adaptive-wrap-prefix-mode)
  :config
  (defun w--maybe-activate-adaptive-wrap-prefix-mode ()
    (when (derived-mode-p 'text-mode)
      (adaptive-wrap-prefix-mode (if visual-line-mode 1 -1)))))

(use-package multi-line
  :defer t)

(use-package visual-fill-column
  :defer t)

(defvar w--wrap-lines-saved-fill-column nil
  "Saved ‘fill-column’ value.")

(use-package virtual-auto-fill)

(define-minor-mode w--wrap-lines-mode
  "Smart combination of auto-fill, visual-line, and visual-fill-column."
  nil " ⇶" nil
  (if w--wrap-lines-mode
      (progn
        (setq w--wrap-lines-saved-fill-column fill-column
              visual-fill-column-width fill-column
              fill-column most-positive-fixnum)
        ;; (auto-fill-mode -1)
        ;; (visual-line-mode)
        ;; (visual-fill-column-mode)
        (virtual-auto-fill-mode))
    (setq fill-column w--wrap-lines-saved-fill-column
          visual-fill-column-width nil)
    ;; (visual-line-mode -1)
    ;; (auto-fill-mode)
    ;; (virtual-auto-fill-mode -1)
    (visual-fill-column-mode -1)))

(defun w--sensible-wrap-mode-1 ()
  (interactive)
  (let ((mode (if (derived-mode-p 'text-mode) 'w--wrap-lines-mode 'toggle-truncate-lines)))
    (call-interactively mode)))

(defun w--sensible-wrap-mode-2 ()
  (interactive)
  (let ((mode (if (derived-mode-p 'text-mode) 'toggle-truncate-lines 'visual-line-mode)))
    (call-interactively mode)))

(defun w--evil-fill-paragraph-dwim ()
  "Fill the current paragraph."
  (interactive)
  ;; move point after comment marker; useful for multi-line comments.
  (save-excursion
    (end-of-line)
    (fill-paragraph)))

(general-define-key :states 'normal
  "Q" #'w--evil-fill-paragraph-dwim)

(defun w--use-very-long-lines ()
  "Use very long lines so that `fill-paragraph' and related functions do not add newlines."
  (interactive)
  (setq fill-column most-positive-fixnum)
  (auto-fill-mode -1))


;;;; outline / folding

(use-package origami
  :custom
  (origami-show-fold-header t)

  :custom-face
  (origami-fold-replacement-face ((t (:inherit magit-diff-context-highlight))))
  (origami-fold-fringe-face ((t (:inherit magit-diff-context-highlight))))

  :commands
  w--origami-parser-imenu-flat

  :config
  (face-spec-reset-face 'origami-fold-header-face)

  (defun w--origami-mode-toggle ()
    (interactive)
    (origami-mode 'toggle)
    (when origami-mode
      (if (> (point) 1)
          (origami-show-only-node (current-buffer) (point))
        (origami-close-all-nodes (current-buffer)))))

  (defun w--origami-parser-imenu-flat (create)
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

(use-package outline
  :defer t
  :delight
  (outline-minor-mode " ‣"))


;;;; formatting

(use-package reformatter)

(use-package reformatter-dwim
  :demand t
  :load-path "lisp/"
  :general
  (:states '(normal visual)
   "g =" 'reformatter-dwim-evil)
  (:states 'normal
   "g +" 'reformatter-dwim-on-save-mode))

;; todo https://github.com/sshaw/copy-as-format/issues/2
(use-package copy-as-format
  :general
  (:states 'visual
   "Y" #'w--evil-copy-as-format)
  :custom
  (copy-as-format-default "slack")
  (copy-as-format-format-alist  ;; only retain formats i use
   '(("github" copy-as-format--github)
     ("jira" copy-as-format--jira)
     ("markdown" copy-as-format--markdown)
     ("rst" copy-as-format--rst)
     ("slack" copy-as-format--slack)))
  :config
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
      (pop-mark))))


;;;; projects

(use-package projectile
  :defer t
  :delight

  :custom
  (projectile-completion-system 'ivy)
  (projectile-ignored-projects '("/usr/local/" "~/"))
  ;; (projectile-mode-line nil)  ;; causes eager loading, :delight has same effect
  (projectile-require-project-root nil)
  (projectile-sort-order 'recently-active)
  (projectile-switch-project-action 'projectile-vc)

  :commands
  w--project-dispatch

  :init
  (add-hook 'find-file-hook (fn: require 'projectile))

  :config
  (projectile-mode)

  (defun w--projectile-find-file-all (&optional pattern)
    "Find any file in the current project, including ignored files."
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
     :action #'w--projectile-find-file-relative
     :require-match t
     :history 'file-name-history))

  (defun w--projectile-find-file-relative (name)
    (find-file
     (concat
      (file-name-as-directory (projectile-project-root))
      name)))

  (defun w--projectile-project-bury-buffers ()
    "Quit all windows and bury all buffers for the current project."
    (interactive)
    (-each (projectile-project-buffers)
      (lambda (buffer)
        (-each (get-buffer-window-list buffer)
          (lambda (window)
            (quit-window nil window)))
        (bury-buffer buffer))))

  (defun w--projectile-open-gui-file-browser ()
    "Open a GUI browser for the directory containing the current file."
    (interactive)
    (when-let ((directory-name (projectile-project-root)))
      (call-process "xdg-open" nil 0 nil directory-name)))

  (transient-define-prefix w--project-dispatch ()
    ["project"
     [("p" "switch" projectile-switch-project)
      ("P" "switch open" projectile-switch-open-project)]]
    ["buffers"
     [("b" "switch" projectile-switch-to-buffer)
      ("B" "switch ↗" projectile-switch-to-buffer-other-window)]
     [("k" "kill all" projectile-kill-buffers)
      ("q" "bury all" w--projectile-project-bury-buffers)]
     [("s" "save all" projectile-save-project-buffers)]]
    ["files/directories"
     [("f" "file" projectile-find-file)
      ("F" "file ↗" projectile-find-file-other-window)
      ("a" "all files" w--projectile-find-file-all)]
     [("d" "dir" projectile-find-dir)
      ("D" "dir ↗" projectile-find-dir-other-window)
      ("-" "top dir" projectile-dired)]
     [("t" "test" projectile-toggle-between-implementation-and-test)
      ("T" "test ↗" projectile-find-implementation-or-test-other-window)]
     [("g" "gui browser" w--projectile-open-gui-file-browser)]]
    ["search/replace"
     [("/" "search" w--counsel-ag-project)
      ("?" "search live" w--counsel-ag-project-all-files)]
     [("o" "occur" projectile-multi-occur)]
     [("r" "replace" projectile-replace)
      ("R" "replace regexp" projectile-replace-regexp)]]
    ["external tools"
     [("c" "compile" projectile-compile-project)]
     [("!" "terminal" terminal-here-project-launch)
      ("1" "terminal" terminal-here-project-launch)]]))

;;;; jumping around

(use-package avy
  :custom
  (avy-all-windows nil)
  (avy-all-windows-alt t)
  (avy-background t)
  (avy-style 'de-bruijn)
  (avy-keys (string-to-list "arstneiofu"))
  :commands
  avy-with) ;; used by evil-easymotion helpers

(use-package beacon
  :delight
  :custom
  (beacon-blink-when-point-moves-vertically 5)
  (beacon-size 20)
  :hook (w--theme-changed-hook . w--beacon-tweak-faces)
  :config
  (add-to-list 'beacon-dont-blink-predicates 'region-active-p)
  ;; (beacon-mode)
  (defun w--beacon-tweak-faces ()
    (setq beacon-color
          (face-attribute 'lazy-highlight :background nil t))))

(use-package dired
  :ensure nil
  :defer t
  :general
  (:keymaps 'dired-mode-map
   :states '(motion normal)
   "-" #'dired-jump))

(use-package dired-x
  :after dired
  :ensure nil)

(defun w--declare-jump (command)
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
          ivy-done
          recenter-top-bottom
          switch-to-buffer)
  (w--declare-jump it))

(use-package dumb-jump
  :custom
  (dumb-jump-selector 'ivy)
  :general
  (:states 'motion
   "gd" #'w--dumb-jump-go
   "gD" #'w--dumb-jump-go-other-window)

  :config

  (defun w--dumb-jump-go ()
    (interactive)
    (let ((s (w--thing-at-point-dwim t)))
      ;; dumb-jump does not have proper extensibility via
      ;; defcustom variable for this. :(
      (when (derived-mode-p 'rst-mode)
        (setq s (s-chop-suffix "_" s)))
      (dumb-jump-go nil nil s)))

  (defun w--dumb-jump-go-other-window ()
    (interactive)
    (let ((dumb-jump-window 'other))
      (w--dumb-jump-go)))

  (w--declare-jump 'w--dumb-jump-go)
  (w--declare-jump 'w--dumb-jump-go-other-window)

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
        (beacon-blink))))

  (advice-add 'dumb-jump-go :around #'w--jump-around-advice))


;;;; frames and windows

;; my preferred window layout is multiple full-height windows,
;; next to each other in a horizontal fashion, i.e. screen
;; divided into columns.

(use-package emacs
  :custom
  (blink-cursor-blinks 1)
  (blink-cursor-delay .5)
  (blink-cursor-interval .5)
  (default-frame-alist '((width . 160) (height . 48)))
  (fit-window-to-buffer-horizontally t)
  (frame-resize-pixelwise t)
  (frame-title-format "%b")
  (help-window-select t)
  (split-height-threshold nil)
  (split-width-threshold 120)
  (split-window-preferred-function 'visual-fill-column-split-window-sensibly)
  (switch-to-buffer-in-dedicated-window 'pop)
  (window-resize-pixelwise t)

  :config
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (blink-cursor-mode)

  (defun w--fit-bottom-error-window-to-buffer (window)
    "Size request for a small error window at the bottom."
    (fit-window-to-buffer window 10 5)))

(use-package balanced-windows
  :config
  (balanced-windows-mode))

(use-package winner
  :custom
  (winner-dont-bind-my-keys t)
  :config
  (winner-mode))

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

(defun w--goto-window-5 ()
  "Go to the fourth window."
  (interactive)
  (w--evil-goto-window 5))

(defun w--goto-window-6 ()
  "Go to the fourth window."
  (interactive)
  (w--evil-goto-window 6))

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
  (--dotimes 2
    (buf-move-right)))

(defun w--set-as-window-4 ()
  "Make this the fourth window."
  (interactive)
  (evil-window-move-far-left)
  (--dotimes 3
    (buf-move-right)))

(defun w--set-as-window-5 ()
  "Make this the fifth window."
  (interactive)
  (evil-window-move-far-left)
  (--dotimes 4
    (buf-move-right)))

(defun w--set-as-window-6 ()
  "Make this the sixth window."
  (interactive)
  (evil-window-move-far-left)
  (--dotimes 5
    (buf-move-right)))

(w--declare-jump 'w--evil-window-next-or-vsplit)
(w--declare-jump 'w--goto-window-1)
(w--declare-jump 'w--goto-window-2)
(w--declare-jump 'w--goto-window-3)
(w--declare-jump 'w--goto-window-4)
(w--declare-jump 'w--goto-window-5)
(w--declare-jump 'w--goto-window-6)

;; todo: write these bindings in a more concise way
(cond
 ((eq system-type 'darwin)  ;; osx: command key
  (evil-define-key*
    'motion global-map
    (kbd "s-1") 'w--goto-window-1
    (kbd "s-2") 'w--goto-window-2
    (kbd "s-3") 'w--goto-window-3
    (kbd "s-4") 'w--goto-window-4
    (kbd "s-5") 'w--goto-window-5
    (kbd "s-6") 'w--goto-window-6)
  (bind-keys
   ("s-1" . w--goto-window-1)
   ("s-2" . w--goto-window-2)
   ("s-3" . w--goto-window-3)
   ("s-4" . w--goto-window-4)
   ("s-5" . w--goto-window-5)
   ("s-6" . w--goto-window-6)
   ))
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
    (kbd "C-5") 'w--goto-window-5
    (kbd "C-6") 'w--goto-window-6
    (kbd "C-!") 'w--set-as-window-1
    (kbd "C-@") 'w--set-as-window-2
    (kbd "C-#") 'w--set-as-window-3
    (kbd "C-$") 'w--set-as-window-4
    (kbd "C-%") 'w--set-as-window-5
    (kbd "C-^") 'w--set-as-window-6)
  (bind-keys
   ("C-SPC" . evil-window-next)
   ("C-S-SPC" . evil-window-prev)
   ("C-`" . evil-window-next)
   ("C-~" . evil-window-prev)
   ("C-1" . w--goto-window-1)
   ("C-2" . w--goto-window-2)
   ("C-3" . w--goto-window-3)
   ("C-4" . w--goto-window-4)
   ("C-5" . w--goto-window-5)
   ("C-6" . w--goto-window-6)
   ("C-!" . w--set-as-window-1)
   ("C-@" . w--set-as-window-2)
   ("C-#" . w--set-as-window-3)
   ("C-$" . w--set-as-window-4)
   ("C-%" . w--set-as-window-5)
   ("C-^" . w--set-as-window-6))))

(use-package buffer-move
  :defer t)

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
  ("5" w--goto-window-5)
  ("6" w--goto-window-6)
  ("!" w--set-as-window-1)
  ("@" w--set-as-window-2)
  ("#" w--set-as-window-3)
  ("$" w--set-as-window-4)
  ("%" w--set-as-window-5)
  ("^" w--set-as-window-6)
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
  "_+_/_-_/C-hnei width/height"
  ("+" evil-window-increase-width nil :exit nil)
  ("-" evil-window-decrease-width nil :exit nil)
  ("C-h" evil-window-decrease-width nil :exit nil)
  ("C-n" evil-window-decrease-height nil :exit nil)
  ("C-e" evil-window-increase-height nil :exit nil)
  ("C-i" evil-window-increase-width nil :exit nil))

;; replace evil-window-map completely
(general-define-key
 :states '(emacs motion)
 (kbd "C-w") 'w--hydra-window/body)


;;;; spelling

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


;;;; completion

(defvar w--ivy-height-percentage 30
  "Percentage of the screen height that ivy should use.")

(use-package emacs
  :delight (abbrev-mode " ⋯"))

(use-package counsel
  :after ivy
  :delight
  :config
  (counsel-mode)
  (ivy-configure 'counsel-M-x :initial-input ""))

(use-package company
  :delight
  :defer t
  :hook (w--theme-changed-hook . w--company-tweak-faces)

  :general
  (:states 'insert
   "C-<return>" #'company-manual-begin
   "<tab>" #'w--indent-or-complete)
  (:keymaps 'company-active-map
   "C-n" #'company-select-next
   "C-p" #'company-select-previous
   "C-<return>" #'company-select-next
   "<tab>" #'company-complete-common-or-cycle
   "/" #'w--company-switch-to-counsel-company)

  :custom
  (company-auto-complete 'company-explicit-action-p)
  (company-dabbrev-code-everywhere t)
  (company-dabbrev-code-other-buffers 'code)
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

  (defun w--company-tweak-faces ()
    (set-face-attribute 'company-tooltip-selection nil :inherit 'region))

  (defun w--indent-or-complete ()
    (interactive)
    (if (or (looking-at "\\_>") (looking-back "/" nil))
        (company-manual-begin)
      (call-interactively #'indent-for-tab-command)))

  (defun w--company-switch-to-counsel-company ()
    (interactive)
    (company-abort)
    (counsel-company)))

(use-package company-lsp
  :disabled
  :demand t
  :after company
  :config
  (add-to-list 'company-backends 'company-lsp))

(use-package flx)

(use-package ivy
  :demand t
  :delight
  :hook (window-size-change-functions . w--adjust-ivy-height)
  :general
  (:keymaps 'ivy-minibuffer-map
   "C-h" 'ivy-backward-delete-char
   "C-w" 'ivy-backward-kill-word
   "C-u" 'kill-whole-line
   "C-<return>" 'ivy-immediate-done
   "<escape>" 'minibuffer-keyboard-quit)
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-height 20)
  (ivy-wrap t)
  :config
  (ivy-mode 1)
  (defun w--clamp-number (num low high)
    "Clamp NUM between LOW and HIGH."
    (min high (max num low)))
  (defun w--adjust-ivy-height (frame)
    "Adjust ivy-height based on the current FRAME height."
    (let* ((total-lines (frame-text-lines frame))
           (lines (truncate (* total-lines w--ivy-height-percentage 0.01)))
           (new-height (w--clamp-number lines 10 20)))
      (setq ivy-height new-height))))

(use-package ivy-hydra)

(use-package ivy-rich
  :after (ivy counsel)
  :custom
  (ivy-rich-parse-remote-buffer nil)
  :config
  (ivy-rich-mode 1))

(use-package smex)

;;;; git / version control

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
  (magit-wip-after-save-local-mode)
  (magit-wip-after-apply-mode)
  (magit-wip-before-change-mode)

  :hook
  (magit-log-mode-hook . w--evil-colemak-basics-disable)
  (magit-process-mode-hook . goto-address-mode)
  (magit-status-mode-hook . w--evil-colemak-basics-disable)

  :custom
  (magit-blame-heading-format "%C %-10a %s")
  (magit-blame-mode-lighter " annotate")
  (magit-blame-time-format "%Y%m%d")
  (magit-branch-prefer-remote-upstream '("master"))
  (magit-branch-read-upstream-first 'fallback)
  (magit-bury-buffer-function 'magit-mode-quit-window)
  (magit-completing-read-function 'ivy-completing-read)
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function 'display-buffer)
  (magit-list-refs-sortby '("-committerdate"))
  (magit-prefer-remote-upstream t)
  (magit-process-popup-time 10)
  (magit-status-goto-file-position t)

  :custom-face
  (magit-mode-line-process ((t (:inherit magit-mode-line-process-error))))

  :commands
  w--git-dispatch

  :init
  (add-hook 'find-file-hook (fn: require 'magit))

  :config
  ;; note: a :general stanza won't work because of execution order:
  ;; custom bindings go on top of what evil-collection-init does
  ;; todo: make ,q use the various magit-*-bury-buffer functions, then
  ;; unbind q to force ,q usage.
  (evil-collection-init 'magit)
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
    "C-w" 'w--hydra-window/body
    "/" 'swiper-isearch)
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

  (magit-wip-after-save-mode)
  (magit-wip-after-apply-mode)
  (magit-wip-before-change-mode)

  (--each '("~" "~/Projects/" "~/Documents/")
    (add-to-list 'magit-repository-directories (cons it 2) t))
  (add-to-list 'evil-overriding-maps '(magit-blame-mode-map . nil))

  (transient-replace-suffix 'magit-commit 'magit-commit-autofixup
    '(6 "x" "Absorb changes" magit-commit-absorb))
  (transient-append-suffix 'magit-push
    "-n" '("/c" "Skip Gitlab CI" "--push-option=ci.skip"))

  ;; hide author names from magit-blame annotations;
  ;; it's usually about why/what/when, not who.
  (setf (->> magit-blame-styles
             (alist-get 'headings)
             (alist-get 'heading-format))
        "%C %s\n")
  (setf (->> magit-blame-styles
             (alist-get 'margin)
             (alist-get 'margin-format)
             (cadr))
        " %C")

  ;; wider margin view; looks like split view
  (setf (->> magit-blame-styles
             (alist-get 'margin)
             (alist-get 'margin-width))
        70)

  (with-eval-after-load 'direnv
    (--each '(magit-blob-mode
              magit-diff-mode
              magit-log-mode
              magit-status-mode)
      (add-to-list 'direnv-non-file-modes it)))

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
      (call-interactively #'git-link)))

  (transient-define-prefix w--git-dispatch ()
    [[("a" "annotate" magit-blame-addition)
      ("A" "buffer log" magit-log-buffer-file)
      ("c" "commit" magit-commit-create)]
     [("d" "diff" magit-diff-dwim)
      ("f" "file-dispatch" magit-file-dispatch)
      ("g" "dispatch" magit-dispatch)]
     [("l" "log" magit-log-current)
      ("s" "status" magit-status)
      ("S" "status other" w--magit-status-other-repository)]
     [("t" "lock" magit-toggle-buffer-lock)
      ("w" "web" w--git-web-browse)
      ("!" "command" magit-git-command)]])
  )

(use-package blamer
  :quelpa (blamer :fetcher github :repo "artawower/blamer.el")
  :custom
  (blamer-idle-time .0)
  (blamer-min-offset 40)
  (blamer-author-formatter "")
  (blamer-datetime-formatter "")
  (blamer-commit-formatter " — %s")
  (blamer-uncommitted-changes-message "(uncommited)")
  (blamer-prettify-time-p nil)
  (blamer-max-commit-message-length 60))

(use-package evil-collection
  :custom
  evil-collection-want-unimpaired-p nil

  :config
  (defun w--colemak-hnei-rotation (_mode mode-keymaps &rest _rest)
    (evil-collection-translate-key 'normal mode-keymaps
      "n" "j"
      "e" "k"
      "i" "l"
      "j" "e"
      "k" "n"
      "l" "i"))

  ;; todo this messes up my own overrides somehow
  ;; (add-hook 'evil-collection-setup-hook #'w--colemak-hnei-rotation)
  (evil-collection-init))

(use-package git-rebase
  :demand t
  :ensure nil ;; included with magit
  :after magit evil-collection
  :hook (git-rebase-mode-hook . w--evil-colemak-basics-disable)
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
   "ZZ" #'with-editor-finish))

(use-package magit-imerge
  :after magit)

(use-package magithub
  :disabled
  :demand t
  :after magit
  :custom
  (magithub-api-timeout 10)
  (magithub-pull-request-arguments '("-o"))
  :general
  (:keymaps 'magithub-map
   ;; colemak tweaks
   "e" nil
   "c" #'magithub-edit-thing
   ;; do not override rebase key binding
   "r" nil
   "R" #'magithub-reply-thing)
  :config
  (magithub-feature-autoinject t)
  ;; https://github.com/vermiculus/magithub/issues/308
  (remove-hook 'magit-status-headers-hook #'magithub-maybe-insert-ci-status-header))

(use-package closql)

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

(use-package git-commit
  :defer t
  :custom
  (git-commit-fill-column 72)
  :hook (git-commit-mode-hook . w--git-commit-mode-hook)
  :config
  (defun w--git-commit-mode-hook ()
    (when git-commit-mode
      (virtual-auto-fill-mode -1)))

  (defun w--gitlab-insert-merge-request-template ()
    (interactive)
    (-if-let* ((template-dir ".gitlab/merge_request_templates/")
               (dir (locate-dominating-file (or (buffer-file-name) default-directory) template-dir))
               (template-file
                (read-file-name "Template: " (concat dir template-dir)) nil t 'file-regular-p))
        (insert-file-contents template-file)
      (user-error "No merge request templates found"))))

(use-package git-link
  :defer t
  :custom
  (git-link-open-in-browser t))

(use-package diff-hl
  :hook
  (magit-post-refresh-hook . diff-hl-magit-post-refresh)
  (diff-hl-mode-hook . diff-hl-update)
  :commands
  w--diff-hl-next-hunk
  w--diff-hl-previous-hunk
  :config
  (w--declare-jump 'w--diff-hl-next-hunk)
  (w--declare-jump 'w--diff-hl-previous-hunk)
  (defun w--diff-hl-previous-hunk ()
    "Jump to the previous hunk."
    (interactive)
    (diff-hl-mode)
    (diff-hl-previous-hunk))
  (defun w--diff-hl-next-hunk ()
    "Jump to the next hunk."
    (interactive)
    (diff-hl-mode)
    (diff-hl-next-hunk))
  (defun w--diff-hl-update-around-advice (fn &rest args)
    (let ((vc-handled-backends '(Git)))
      (apply fn args)))
  (advice-add 'diff-hl-update :around #'w--diff-hl-update-around-advice))

(use-package ediff
  :defer t
  :custom
  (ediff-split-window-function 'split-window-horizontally))

(use-package smerge-mode
  :delight " 🔀"
  :defer t
  :config
  (transient-define-prefix w--merge-dispatch ()
    ["merge"
     ("m" "smerge-mode" smerge-mode)]
    ["conflict"
     [("n" "next" smerge-next)]
     [("e" "prev" smerge-prev)]
     [("p" "prev" smerge-prev)]]
    ["keep"
     [("c" "current" smerge-keep-current)]
     [("b" "base" smerge-keep-base)]
     [("l" "lower" smerge-keep-lower)]
     [("u" "upper" smerge-keep-upper)]
     [("a" "all" smerge-keep-all)]]))

(use-package vc
  :config
  (setq vc-handled-backends nil))

(use-package vdiff
  :defer t
  :custom
  (vdiff-subtraction-style 'single)
  (vdiff-subtraction-fill-char ?·)
  (vdiff-3way-layout-function 'w--vdiff-3way-layout-function-vertical)
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
  :config
  (defun w--vdiff-3way-layout-function-vertical (buffer-a buffer-b buffer-c)
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
   "E" #'vdiff-magit-popup)
  :config
  (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
  (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
  (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
  (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit)

  (w--make-hydra w--hydra-vdiff nil
    "vdiff"
    "_n_/_e_/_p_ nav"
    ("n" vdiff-next-hunk :exit nil)
    ("e" vdiff-previous-hunk :exit nil)
    ("p" vdiff-previous-hunk :exit nil)
    ("N" vdiff-next-fold :exit nil)
    ("E" vdiff-previous-fold :exit nil)
    ("P" vdiff-previous-fold :exit nil)
    "_c_lose"
    ("c" vdiff-close-fold)
    ("C" vdiff-close-all-folds)
    "_f_/_x_ refine"
    ("f" vdiff-refine-this-hunk)
    ("F" vdiff-refine-all-hunks)
    ("x" vdiff-remove-refinements-in-hunk)
    "_o_pen"
    ("o" vdiff-open-fold)
    ("O" vdiff-open-all-folds)
    "_r_eceive"
    ("r" vdiff-receive-changes)
    ("R" vdiff-receive-changes-and-step :exit nil)
    "_s_end"
    ("s" vdiff-send-changes)
    ("S" vdiff-send-changes-and-step :exit nil)
    "_u_pdate"
    ("u" vdiff-refresh)
    "_d_ hydra"
    ("d" vdiff-hydra/body)))


;;;; writeroom

(use-package writeroom-mode
  :defer t
  :custom
  (writeroom-global-effects nil)
  (writeroom-maximize-window nil)
  (writeroom-mode-line t)
  :commands
  w--writeroom-narrower
  w--writeroom-wider
  w--writeroom-reset
  :config
  (defun w--writeroom-narrower ()
    "Make the writeroom column narrower."
    (interactive)
    (unless writeroom-mode
      (writeroom-mode))
    (writeroom-decrease-width))
  (defun w--writeroom-wider ()
    "Make the writeroom column wider."
    (interactive)
    (unless writeroom-mode
      (writeroom-mode))
    (writeroom-increase-width))
  (defun w--writeroom-reset ()
    "Reset the writeroom column width."
    (interactive)
    (unless writeroom-mode
      (writeroom-mode))
    (writeroom-adjust-width nil)))


;;;; flycheck

(use-package flycheck
  :demand t
  :after direnv

  :custom
  (flycheck-checker-error-threshold 1000)
  (flycheck-display-errors-delay 1.0)
  (flycheck-idle-change-delay 3)
  (flycheck-mode-line-prefix "✔")
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-python-flake8-executable "flake8")
  (flycheck-relevant-error-other-file-minimum-level nil)

  :general
  (:keymaps 'flycheck-error-list-mode-map
   :states 'motion
   "n" #'flycheck-error-list-next-error
   "e" #'flycheck-error-list-previous-error
   "p" #'flycheck-error-list-previous-error
   "<return>" #'flycheck-error-list-goto-error)

  :hook
  (flycheck-mode-hook . w--flycheck-show-error-other-file-mode)
  (flycheck-before-syntax-check-hook . direnv--maybe-update-environment)
  (flycheck-error-list-after-refresh-hook . w--flycheck-hide-error-list-header)

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
      (window-height . w--fit-bottom-error-window-to-buffer)
      (window-parameters . ((no-other-window . t)
                            (no-delete-other-windows . t))))))

  (transient-define-prefix w--flycheck-dispatch ()
    ["flycheck"
     [("b" "buffer" flycheck-buffer)
      ("m" "compile" w--flycheck-compile-current)
      ("M" "compile other" flycheck-compile)]
     [("c" "toggle error window" w--flycheck-toggle-error-window)
      ("C" "toggle checking" flycheck-mode)
      ("o" "toggle other file errors" w--flycheck-show-error-other-file-mode)]]
    ["setup"
     [("s" "select checker" flycheck-select-checker)]
     [("v" "verify setup" flycheck-verify-setup)]]
    ["navigation"
     [("n" "next" flycheck-next-error :transient t)]
     [("e" "previous" flycheck-previous-error :transient t)]
     [("p" "previous" flycheck-previous-error :transient t)]])

  (defun w--flycheck-compile-current ()
    "Run ‘flycheck-compile’ using the current checker."
    (interactive)
    (flycheck-compile (flycheck-get-checker-for-buffer)))

  (defun w--flycheck-last-error ()
    "Jump to the last flycheck error."
    (interactive)
    (goto-char (point-max))
    (flycheck-previous-error))

  (defun w--flycheck-toggle-error-window ()
    "Show or hide the flycheck error list."
    (interactive)
    (if-let* ((buffer (get-buffer flycheck-error-list-buffer))
              (window (get-buffer-window buffer)))
        (progn
          ;; for some reason this sometimes gets stuck when using
          ;; the more specific (quit-windows-on buffer)
          (delete-window window))
      (flycheck-list-errors)))

  (define-minor-mode w--flycheck-show-error-other-file-mode
    "Quickly toggle showing of errors from other files"
    nil nil nil
    (setq flycheck-relevant-error-other-file-show w--flycheck-show-error-other-file-mode)
    (when flycheck-mode
      (flycheck-buffer)))

  (defun w--flycheck-hide-error-list-header ()
    "Hide the error list header line."
    (when-let ((buffer (get-buffer "*Flycheck errors*")))
      (with-current-buffer buffer
        (setq header-line-format nil)))))

(use-package flycheck-color-mode-line
  :demand t
  :after flycheck
  :hook (flycheck-mode-hook . flycheck-color-mode-line-mode))

;;;; toggles

(w--make-hydra w--hydra-toggle nil
  "toggle"
  "_b_ackgound"
  ("b" w--toggle-dark-light-theme)
  ("B" w--set-theme-from-environment)
  "_c_ flycheck"
  ("c" flycheck-mode)
  "_d_iff"
  ("d" diff-hl-mode)
  "_f_ill"
  ("f" auto-fill-mode)
  ("F" display-fill-column-indicator-mode)
  "_h_ighlight"
  ("h" symbol-overlay-mode)
  "_l_ine"
  ("l" hl-line-mode)
  ("L" global-hl-line-mode)
  "_n_umber"
  ("n" display-line-numbers-mode)
  ("N" w--display-line-numbers-cycle)
  "_r_ writeroom"
  ("r" writeroom-mode)
  ("R" (progn
         (delete-other-windows)
         (writeroom-mode)))
  "_s_pell"
  ("s" flyspell-mode)
  "_w_rapping"
  ("w" w--sensible-wrap-mode-1)
  ("W" w--sensible-wrap-mode-2)
  "_z_ folding"
  ("z" (w--origami-mode-toggle))
  "_SPC_ whitespace"
  ("SPC" whitespace-mode)
  ("S-SPC" w--show-trailing-whitespace-mode)
  "_1_ num/sym"
  ("1" global-evil-swap-keys-mode)
  ("!" global-evil-swap-keys-mode)
  "_._ indent-guide"
  ("." indent-guide-mode)
  "_(_ smartparens"
  ("9" smartparens-mode)
  ("(" smartparens-mode)
  ("0" smartparens-mode)
  (")" smartparens-mode)
  "_=_ balanced-windows"
  ("=" balanced-windows-mode))


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
  ("b" w--buffer-dispatch)
  "_c_heck"
  ("c" w--flycheck-dispatch)
  "_d_iff"
  ("d" w--hydra-vdiff/body)
  "_f_ind"
  ("f" w--file-dispatch)
  "_g_it"
  ("g" w--git-dispatch)
  "_h_ighlight"
  ("h" w--symbol-overlay-put-dwim)
  ("H" symbol-overlay-remove-all)
  "_j_ump"
  ("j" counsel-imenu)
  "_m_erge"
  ("m" w--merge-dispatch)
  "_n_arrow"
  ("n" w--narrow-dwim)
  ("N" w--fancy-narrow-dwim)
  "_o_ccur"
  ("o" w--occur-dwim)
  "_p_roject"
  ("p" w--project-dispatch)
  "_q_ bury buffer"
  ("q" bury-buffer)
  ("Q" unbury-buffer)
  "_r_eplace"
  ("r" w--replace-dispatch)
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
  ("x" counsel-M-x)
  "_y_ copy format"
  ("y" w--evil-copy-as-format)
  "_SPC_ whitespace"
  ("SPC" whitespace-cleanup)
  "_/_ search"
  ("/" w--hydra-search/body)
  "_,_ major mode"
  ("," w--major-mode-hydra)
  ("'" w--major-mode-hydra))

(general-define-key
 :states 'motion
 "," #'w--hydra-leader/body
 "'" #'w--hydra-leader/body)


;;; Major modes

(setq-default major-mode 'text-mode)

(defvar w--major-modes
  '(("normal" . normal-mode)
    ("fundamental" . fundamental-mode)
    ("text (txt)" . text-mode)
    ("c" . c-mode)
    ("elisp" . emacs-lisp-mode)
    ("elisp interaction" . lisp-interaction-mode)
    ("javascript (js)" . js-mode)
    ("jinja (j2)" . jinja2-mode)
    ("json" . json-mode)
    ("markdown (md)" . markdown-mode)
    ("org" . org-mode)
    ("python" . python-mode)
    ("restructuredtext (rst)" . rst-mode)
    ("shell" . sh-mode)
    ("sql" . sql-mode)
    ("typescript (ts)" . typescript-mode)
    ("vue" . vue-mode)
    ("yaml (yml)" . yaml-mode)
    ("xml" . nxml-mode))
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

(use-package mmm-mode
  :custom-face
  (mmm-default-submode-face ((t (:background nil)))))


;;; Major mode: text (generic)

(use-package typo
  :defer t
  :delight " ”"
  :commands w--typo-cycle-quotation-marks
  :config
  (setq-default typo-language "prefer-single")
  (add-to-list 'typo-quotation-marks '("prefer-single" "‘" "’" "“" "”"))
  (define-typo-cycle w--typo-cycle-quotation-marks
    "Cycle through various quotation marks."
    ("'" "‘" "’" "“" "”" "\"")))

(use-package text-mode
  :ensure nil
  :defer t
  :hook (text-mode-hook . w--text-mode-hook)
  :config
  (defun w--text-mode-hook ()
    (w--show-trailing-whitespace-mode)
    (guess-language-mode)))


;;; Major mode: programming (generic)

(use-package fic-mode
  :defer t
  :custom
  (fic-highlighted-words
   '("FIXME" "fixme"
     "TODO" "todo"
     "BUG" "bug"
     "XXX" "xxx")))

(use-package lsp-mode
  :disabled)

(use-package prog-mode
  :ensure nil
  :defer t
  :hook (prog-mode-hook . w--prog-mode-hook)
  :config
  (defun w--prog-mode-hook ()
    (setq-local comment-auto-fill-only-comments t)
    (auto-fill-mode)
    (column-number-mode)
    (fic-mode)
    (flyspell-prog-mode)
    (highlight-parentheses-mode)
    (indent-guide-mode)
    (symbol-overlay-mode)
    (w--show-trailing-whitespace-mode)))

(use-package dash-docs
  :custom
  (dash-docs-docsets-path "~/.var/app/org.zealdocs.Zeal/data/Zeal/Zeal/docsets")
  (dash-docs-enable-debugging nil)
  (dash-docs-min-length 2))

(use-package counsel-dash
  :after dash-docs)


;;; Major mode: c

(use-package cc-mode
  :defer t
  :hook (c-mode-hook . w--c-mode-hook)
  :config
  (defun w--c-mode-hook ()
    (setq evil-shift-width 2)
    (evil-swap-keys-swap-double-single-quotes)
    (evil-swap-keys-swap-square-curly-brackets)
    (evil-swap-keys-swap-underscore-dash)))


;;; Major mode: compilation and comint

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
  (compilation-mode-hook . w--compilation-mode-hook)
  (compilation-finish-functions . w--compilation-finished)

  :config
  (w--make-hydra w--hydra-compilation nil
    "compilation"
    "_r_ecompile"
    ("r" recompile))

  (defun w--compilation-use-xterm-color-filter ()
    (remove-hook 'comint-output-filter-functions 'ansi-color-process-output t)
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter t))

  (defun w--compilation-mode-hook ()
    (smartparens-mode -1)
    (w--set-major-mode-hydra #'w--hydra-compilation/body)
    (w--compilation-use-xterm-color-filter)
    (remove-hook 'comint-output-filter-functions 'comint-postoutput-scroll-to-bottom))

  (defun w--compilation-finished (buffer _status)
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
   "<return>" 'w--comint-find-file-or-goto-end)
  (:keymaps 'comint-mode-map
   :states 'insert
   "<return>" #'comint-send-input
   "C-n" #'comint-next-input
   "C-p" #'comint-previous-input
   "C-r" #'comint-history-isearch-backward
   "C-/" #'w--comint-ivy-history)

  :hook
  (comint-mode-hook . w--compilation-mode-hook)

  :config
  (evil-set-initial-state 'comint-mode 'normal)

  (defun w--comint-find-file-or-goto-end ()
    (interactive)
    (condition-case nil
        (evil-find-file-at-point-with-line)
      (user-error
       (goto-char (point-max))
       (evil-append-line 0))))

  (defun w--comint-ivy-history ()
    (interactive)
    (insert (ivy-read
             "Command history: "
             (-uniq (ring-elements comint-input-ring))
             :require-match t))))

(use-package xterm-color
  :defer t)


;;; Major mode: customize

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


;;; Major mode: cython

(use-package cython-mode
  :defer t)

(use-package flycheck-cython
  :demand t
  :after (cython-mode flycheck))


;;; Major mode: docker

(use-package docker
  :defer t
  :custom
  (docker-compose-command "docker compose")
  :custom-face
  (docker-face-status-error ((t (:inherit error))))
  (docker-face-status-success ((t (:inherit success))))
  (docker-face-status-warning ((t (:inherit warning)))))

(use-package dockerfile-mode
  :defer t
  :mode
  (rx "Dockerfile" (any "-_.") (* any) string-end)
  :general
  (:keymaps 'dockerfile-mode-map
   :states 'normal
   "<return> "'w--split-line-backslash
   [remap evil-join] #'w--evil-join-smart-backslash-eol))


;;; Major mode: emacs lisp

(use-package elisp-mode
  :defer t
  :ensure nil
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
  :hook (emacs-lisp-mode-hook . w--emacs-lisp-mode-hook)

  :config
  (defun w--emacs-lisp-mode-hook ()
    (setq
     evil-lookup-func 'w--helpful-evil-lookup-func
     evil-shift-width 2)
    (w--set-major-mode-hydra #'w--hydra-emacs-lisp/body)
    (smartparens-mode -1)
    (lispy-mode)
    (lispyville-mode)
    (aggressive-indent-mode)
    (highlight-parentheses-mode -1)
    (rainbow-delimiters-mode))

  (require 'which-func)
  (add-to-list 'which-func-modes 'emacs-lisp-mode)

  (w--make-hydra w--hydra-emacs-lisp nil
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

(use-package el-patch
  :demand t
  :after elisp-mode)

(use-package lisp-indent-patch
  :load-path "lisp/"
  :demand t
  :after elisp-mode)

(use-package lispy
  :after elisp-mode
  :defer t
  :delight
  :config
  (lispy-set-key-theme '(lispy)))

(use-package lispyville
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

(use-package eldoc
  :defer t
  :delight)

(use-package flycheck-package
  :demand t
  :after (elisp-mode flycheck)
  :config
  (flycheck-package-setup))


;;; Major mode: git

(use-package gitattributes-mode
  :defer t)

(use-package gitconfig-mode
  :defer t
  :mode
  ((rx ".gitconfig" (* any) string-end) . gitconfig-mode)
  ((rx ".config/git/config" (* any) string-end) . gitconfig-mode))

(use-package gitignore-mode
  :defer t)


;;; Major mode: groovy

(use-package groovy-mode
  :defer t)


;;; Major mode: help

(use-package help-mode
  :defer t
  :ensure nil
  :general
  (:keymaps 'help-mode-map
   "q" nil)
  :hook (help-mode-hook . w--help-mode-hook)
  :config
  (defun w--help-mode-hook ()
    (setq evil-lookup-func 'w--helpful-evil-lookup-func)))


;;; Major mode: helpful

(use-package helpful
  :general
  (:prefix "C-h"
   "f" #'helpful-callable
   "v" #'helpful-variable
   "k" #'helpful-key
   "o" #'helpful-symbol)
  (:keymaps 'helpful-mode-map
   :states 'normal
   "gr" #'helpful-update
   "<tab>" #'forward-button
   "S-<tab>" #'backward-button)
  :commands
  w--helpful-evil-lookup-func
  :hook (helpful-mode-hook . w--helpful-mode-hook)
  :config
  (defun w--helpful-mode-hook ()
    (setq
     evil-lookup-func 'w--helpful-evil-lookup-func
     evil-shift-width 2))
  (defun w--helpful-evil-lookup-func ()
    (call-interactively #'helpful-symbol)))


;;; Major mode: html

(defgroup prettier nil
  "Formatting using Prettier"
  :group 'languages)
(reformatter-define prettier-format
  :program "prettier"
  :args `("--stdin-filepath" ,(file-name-nondirectory (buffer-file-name)))
  :lighter " Prettier"
  :group 'prettier)
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
  (html-mode-hook . w--html-mode-hook)
  (mhtml-mode-hook . w--html-mode-hook)
  :config
  (defun w--html-mode-hook ()
    (reformatter-dwim-select 'prettier-format-html)
    (setq evil-shift-width 2)))

;;; Major mode: jinja

(use-package jinja2-mode
  :defer t
  :mode
  (rx ".j2" string-end))


;;; Major mode: javascript

(use-package js2-mode
  :defer t
  :hook
  (js-mode-hook . w--js-mode-hook)
  (js-mode-hook . js2-minor-mode)
  :config
  (defun w--js-mode-hook ()
    (modify-syntax-entry ?_ "w")
    (setq
     tab-width 2
     evil-shift-width tab-width
     js-indent-level tab-width)
    (reformatter-dwim-select 'prettier-format-js)))

(use-package rjsx-mode)


;;; Major mode: json

(use-package json-mode
  :defer t
  :hook (json-mode-hook . w--json-mode-hook)
  :config
  (defun w--json-mode-hook ()
    (setq
     tab-width 2
     evil-shift-width tab-width
     js-indent-level tab-width)
    (reformatter-dwim-select 'jq-format-json)
    (evil-swap-keys-swap-colon-semicolon)
    (evil-swap-keys-swap-double-single-quotes)))

(use-package jq-format
  :demand t
  :after json-mode
  :delight
  (jq-format-json-on-save-mode " ❤")
  (jq-format-jsonlines-on-save-mode " ❤"))


;;; Major mode: markdown

(use-package markdown-mode
  :defer t
  :custom
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t)

  :custom-face
  (markdown-code-face ((t (:inherit unspecified))))
  (markdown-comment-face
   ((t (:inherit font-lock-comment-face
        :foreground unspecified
        :strike-through unspecified))))

  :hook
  (markdown-mode-hook . w--markdown-mode-hook)

  :config
  (defun w--markdown-mode-hook ()
    (setq evil-shift-width 2)
    (w--set-major-mode-hydra #'w--hydra-markdown/body)
    (flyspell-mode)
    (w--add-evil-surround-pairs
     ?b '("**" . "**")  ;; strong emphasiss
     ?c '("`" . "`")  ;; inline code
     ?e '("*" . "*"))) ;; emphasis


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
    ("n" markdown-cleanup-list-numbers)
    "_p_review"
    ("p" markdown-preview)
    ("P" markdown-live-preview-mode)
    "_q_uote"
    ("q" w--markdown-blockquote-dwim))

  (defun w--markdown-blockquote-dwim ()
    (interactive)
    (if (region-active-p)
        (call-interactively #'markdown-blockquote-region)
      (save-excursion
        (markdown-blockquote-region
         (line-beginning-position)
         (line-end-position))))))

(use-package markdown-toc
  :demand t
  :after markdown-mode
  :custom
  (markdown-toc-header-toc-title ""))


;;; Major mode: org

(use-package org
  :defer t
  :hook (org-mode-hook . w--org-mode-hook)
  :config
  (defun w--org-mode-hook ()
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
  (setq
   evil-org-movement-bindings
   '((left . "h")
     (down . "n")
     (up . "e")
     (right . "i")))
  (evil-org-set-key-theme))


;;; Major mode: pkgbuild

(use-package pkgbuild-mode
  :custom
  (pkgbuild-update-sums-on-save nil)
  :custom-face
  (pkgbuild-error-face ((t (:inherit error)))))


;;; Major mode: python

(use-package python
  :defer t
  :interpreter ("python" . python-mode)
  :hook (python-mode-hook . w--python-mode-hook)
  :mode
  ((rx ".bzl" string-end) . python-mode)  ;; starlark
  ((rx ".pyi" string-end) . python-mode)

  :general
  (:keymaps 'python-mode-map
   :states 'normal
   [remap evil-join] #'w--evil-join-python
   [backspace] 'python-nav-backward-up-list
   "<return>" 'python-black-partial-dwim)
  (:keymaps 'python-mode-map
   :states 'insert
   "C-l" 'multi-line)
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
  (dolist (open '("(" "{" "["))
    (sp-local-pair
     'python-mode open nil
     :unless '(sp-point-before-word-p)))

  (defun w--python-mode-hook ()
    (setq
     fill-column 79
     evil-lookup-func #'counsel-dash-at-point)
    (setq-local
     comment-fill-column 72
     counsel-dash-docsets '("Python_3" "SQLAlchemy" "Flask" "Jinja"))
    (python-isort-on-save-mode-enable-dwim)
    (python-black-on-save-mode-enable-dwim)
    (reformatter-dwim-select 'python-black)
    (modify-syntax-entry ?_ "w")
    (w--set-major-mode-hydra #'w--hydra-python/body)
    (evil-swap-keys-swap-colon-semicolon)
    (evil-swap-keys-swap-underscore-dash)
    (smartparens-mode) ;; todo
    ;; (lispyville-mode)
    (origami-mode)
    ;; (python-docstring-mode)
    (w--add-evil-surround-pairs
     ?` '("``" . "``")) ;; for reStructuredText literals in docstrings
    (evil-add-to-alist
     'origami-parser-alist
     'python-mode 'w--origami-parser-imenu-flat))

  ;; todo: integrate this with the global easymotion hydra
  ;; (evilem-make-motion
  ;;  w--easymotion-python
  ;;  (list
  ;;   ;; Collect interesting positions around point, and all visible
  ;;   ;; blocks in the window. Results are ordered: forward after point,
  ;;   ;; then backward from point.
  ;;   'python-nav-end-of-statement 'python-nav-end-of-block 'python-nav-forward-block
  ;;   'python-nav-beginning-of-statement 'python-nav-beginning-of-block 'python-nav-backward-block)
  ;;  :pre-hook (setq evil-this-type 'line))
  ;; (evil-define-key* 'motion python-mode-map
  ;;   (kbd "SPC TAB") 'w--easymotion-python)
  ;; (defun w--swiper-python-definitions ()
  ;;   (interactive)
  ;; (swiper "^\\s-*\\(def\\|class\\)\\s- "))
  ;; (evil-define-key* 'motion python-mode-map
  ;;   (kbd "SPC /") 'w--swiper-python-definitions)

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

  (evil-define-operator w--evil-join-python (beg end)
    "Like 'evil-join', but handles comments and some continuation styles sensibly."
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

  (defun w--python-insert-pdb-trace ()
    "Insert a pdb trace statement using PDB-MODULE before the current statement."
    (interactive)
    (w--python-insert-statement
     'before
     "__import__(\"pdb\").set_trace()  # FIXME"))

  (defun w--python-insert-ipython-repl (position)
    "Insert an IPython repl statement before or after the current statement."
    (w--python-insert-statement
     position
     (format "__import__(\"IPython\").embed()  # FIXME")))

  (evil-define-command w--python-print-expression ()
    (interactive)
    (let ((thing (w--thing-at-point-dwim)))
      (w--python-insert-statement
       'after
       (format "print(f\"{%s=}\")" thing))))

  (evil-define-operator w--python-refactor-make-variable (beg end _type)
    "Refactor the current region into a named variable."
    (interactive "<R>")
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
      (unless thing
        (user-error "No thing at point"))
      (w--python-insert-statement
       'before
       (format "import %s  # fixme: move to proper place" thing))))

  (evil-define-command w--python-split-string ()
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

  (evil-declare-repeat 'w--python-split-string)

  (w--make-hydra w--hydra-python nil
    "python"
    "_b_reakpoint"
    ("b" w--python-insert-pdb-trace)
    "_c_overage"
    ("c" python-coverage-overlay-mode)
    "_i_mport"
    ("i" w--python-insert-import-statement)
    "_l_ multi-line"
    ("l" multi-line)
    ("L" multi-line-single-line)
    "_p_ print"
    ("p" w--python-print-expression)
    "_r_epl"
    ("r" (w--python-insert-ipython-repl 'after))
    ("R" (w--python-insert-ipython-repl 'before))
    "_s_ split"
    ("s" w--python-split-string)
    "_t_ pytest"
    ("t" python-pytest-popup)
    ("T" python-pytest-repeat)
    "_v_ariable"
    ("v" w--python-refactor-make-variable)))

(use-package lsp-python
  :disabled)

(use-package python-black
  :demand t
  :after python
  :delight
  (python-black-on-save-mode " ❤"))

(use-package python-coverage
  :quelpa (python-coverage :fetcher github :repo "wbolster/emacs-python-coverage")
  :demand t
  :after python
  :delight
  (python-coverage-overlay-mode " 🚨"))

(use-package evil-text-object-python
  :demand t
  :after (evil python)
  :general
  (:keymaps 'python-mode-map
   :states '(operator visual)
   "ul" 'evil-text-object-python-inner-statement
   "al" 'evil-text-object-python-outer-statement
   "uf" 'evil-text-object-python-function)
  (:keymaps 'python-mode-map
   :states 'operator
   [remap evil-forward-char] #'w--evil-forward-char-or-python-statement)
  :config
  (defun w--evil-forward-char-or-python-statement (count)
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
  :after (evil python)
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
  :after company python
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
  (python-pytest-mode-hook . w--python-pytest-mode-hook)
  (python-pytest-finished-hook . evil-force-normal-state)

  :config
  (w--make-hydra w--hydra-python-pytest nil
    "python-pytest"
    "_r_epeat"
    ("r" python-pytest-repeat nil)
    "_t_ pytest"
    ("t" python-pytest-popup nil)
    ("T" python-pytest-repeat nil))
  (magit-define-popup-option 'python-pytest-popup
    ?n "count" "--count=")

  (defun w--python-pytest-mode-hook ()
    (setq-local company-backends '(company-dabbrev-code))
    (origami-mode)
    (w--compilation-use-xterm-color-filter)
    (remove-hook 'comint-output-filter-functions 'comint-postoutput-scroll-to-bottom t)
    (w--set-major-mode-hydra #'w--hydra-python-pytest/body)
    (modify-syntax-entry ?/ ".")
    (when-let ((project-root (projectile-project-root)))
      (add-to-list 'prettify-symbols-alist `(,(s-chop-suffix "/" project-root) . ?…)))
    (when-let ((venv-path (getenv "VIRTUAL_ENV")))
      (add-to-list 'prettify-symbols-alist `(,venv-path . ?…)))
    (prettify-symbols-mode))

  (add-to-list 'company-dabbrev-code-modes 'python-pytest-mode)

  (defun w--python-pytest-origami-parser (create)
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
  (evil-add-to-alist
   'origami-parser-alist
   'python-pytest-mode 'w--python-pytest-origami-parser))


;;; Major mode: profiling-report

(use-package profiler
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
  :hook
  (profiler-report-mode-hook . w--profiler-report-mode-hook)
  :config
  (defun w--profiler-report-mode-hook ()
    (setq evil-lookup-func 'w--helpful-evil-lookup-func))
  (evil-set-initial-state 'profiler-report-mode 'motion))


;;; Major mode: restructuredtext

(use-package rst
  :defer t
  :hook (rst-mode-hook . w--rst-mode-hook)
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
  (defun w--rst-mode-hook ()
    (setq
     evil-shift-width 2
     rst-mode-abbrev-table nil)
    (w--set-major-mode-hydra #'w--hydra-rst/body)
    (flyspell-mode)
    (origami-mode)
    (sphinx-mode)
    (evil-add-to-alist
     'origami-parser-alist
     'rst-mode 'w--origami-parser-imenu-flat)
    (w--add-evil-surround-pairs
     ?b '("**" . "**")  ;; strong
     ?c '("``" . "``")  ;; inline code
     ?C '(".. code-block::\n\n" . "")  ;; code-block
     ?d '(":doc:`" . " <...>`")  ;; doc link
     ?e '("*" . "*")  ;; emphasis
     ?l '("`" . " <...>`_")  ;; hyperlink
     ?t '(":term:`" . "`"))  ;; glossary term
    (make-local-variable 'evil-inner-text-objects-map)
    (general-define-key
     :keymaps 'evil-inner-text-objects-map
     "c" #'w--evil-rst-inner-code-block)
    (general-define-key
     :keymaps 'evil-outer-text-objects-map
     "c" #'w--evil-rst-a-code-block))

  (w--make-hydra w--hydra-rst nil
    "restructuredtext"
    "_a_djust"
    ("a" rst-adjust)
    "_c_ edit code block"
    ("c" w--rst-edit-code-block-dwim)
    "_e_mphasise"
    ("e" w--evil-rst-emphasise)
    "_l_ist"
    ("l" w--evil-rst-bullet-list)
    ("L" w--evil-rst-bullet-list-all)
    "_s_trong"
    ("s" w--evil-rst-strong)
    "_w_rap"
    ("w" w--evil-rst-wrap))

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

  (defvar w--rst-code-block-language-major-modes-mapping
    '(("elisp" . emacs-lisp-mode))
    "Mapping of code block languages to major modes.")

  (defun w--rst-goto-beginning-of-block ()
    (let ((language))
      (search-backward "::")
      (goto-char (match-end 0))
      (setq language
            (s-trim (buffer-substring-no-properties
                     (point) (line-end-position))))
      (when (string-empty-p language)
        (setq language nil))
      (forward-line)
      (while (and (looking-at-p "$") (not (eobp)))
        (forward-line))
      language))

  (defun w--evil-rst-code-block (type)
    "Find the range of an inner/outer code block."
    (let* (language
           (range
            (save-excursion
              (setq language (w--rst-goto-beginning-of-block))
              (if (eq type 'inner)
                  (evil-indent-plus-i-indent)
                (evil-indent-plus-a-indent)))))
      (unless (<= (first range) (point) (second range) )
        (user-error "Not in a block"))
      (cons language range)))

  (evil-define-text-object w--evil-rst-a-code-block (count &optional _beg _end _type)
    (cdr (w--evil-rst-code-block 'outer)))

  (evil-define-text-object w--evil-rst-inner-code-block (count &optional _beg _end _type)
    (cdr (w--evil-rst-code-block 'inner)))

  (defun w--rst-edit-code-block-dwim ()
    (interactive)
    (let* ((block (w--evil-rst-code-block 'inner))
           (language (car block))
           (range (cdr block))
           (block-major-mode
            (or
             (cdr (assoc language w--rst-code-block-language-major-modes-mapping))
             (when language (intern (s-concat language "-mode")))))
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
  ;;  w--easymotion-rst
  ;;  (list 'rst-forward-section 'rst-backward-section)
  ;;  :pre-hook (setq evil-this-type 'line))
  ;; (evil-define-key* 'motion rst-mode-map
  ;;   (kbd "SPC TAB") 'w--easymotion-rst)
  (evil-define-operator w--evil-rst-bullet-list (beg end &optional count)
    :type line
    (interactive "<r>P")
    (let ((all (not (null count))))
      (rst-bullet-list-region beg end all)))
  (evil-define-operator w--evil-rst-bullet-list-all (beg end)
    :type line
    (interactive "<r>")
    (w--evil-rst-bullet-list beg end t))
  (evil-define-operator w--evil-rst-wrap (beg end type open close)
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
  (evil-define-operator w--evil-rst-emphasise (beg end type)
    (interactive "<R>")
    (w--evil-rst-wrap beg end type "*"))
  (evil-define-operator w--evil-rst-strong (beg end type)
    (interactive "<R>")
    (w--evil-rst-wrap beg end type "**"))
  (defun w--rst-header-1 ()
    (interactive)
    ;; todo: this is incomplete
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      (save-restriction
        (narrow-to-region beg end)
        (rst-adjust-section-title nil)))))

(use-package sphinx-mode
  :demand t
  :delight sphinx-mode
  :after rst
  :config
  (setq sphinx-mode-map (make-sparse-keymap)))


;;; Major mode: rust

(use-package rust-mode
  :defer t
  :hook (rust-mode-hook . w--rust-mode-hook)
  :config
  (defun w--rust-mode-hook ()
    (evil-swap-keys-swap-underscore-dash)
    (evil-swap-keys-swap-double-single-quotes)
    (evil-swap-keys-swap-square-curly-brackets)
    (origami-mode)
    (evil-add-to-alist
     'origami-parser-alist
     'rust-mode 'w--origami-parser-imenu-flat)))


;;; Major mode: shell

(use-package sh-script
  :defer t
  :mode
  ((rx "bashrc" string-end) . sh-mode)
  ((rx ".bashrc-" (* any) string-end) . sh-mode)
  ((rx (* any) ".env" string-end) . sh-mode)
  :hook (sh-mode-hook . w--sh-mode-hook)
  :custom
  (sh-indent-after-continuation 'always)
  :general
  (:keymaps 'sh-mode-map
   :states 'normal
   "<return> "'w--split-line-backslash
   [remap evil-join] #'w--evil-join-smart-backslash-eol)
  :config
  (defun w--sh-mode-hook ()))


;;; Major mode: sql

(use-package sql
  :defer t
  :mode
  ((rx (? ".") "psqlrc" string-end) . sql-mode)
  :hook (sql-mode-hook . w--sql-mode-hook)
  :config
  (defun w--sql-mode-hook ()
    (setq evil-shift-width 2)
    (reformatter-dwim-select 'sqlformat)))

(use-package sqlformat
  :demand t
  :after sql
  :custom
  (sqlformat-command 'pgformatter))


;;; Major mode: toml

(use-package conf-mode
  :hook (conf-toml-mode-hook . w--conf-toml-mode-hook)
  :config
  (defun w--conf-toml-mode-hook ()
    (setq
     tab-width 2
     evil-shift-width tab-width)))


;;; Major mode: typescript

(use-package typescript-mode
  :custom
  (typescript-indent-level 2))


;;; Major mode: vue

(use-package vue-mode
  :config

  ;; somehow the default is not scss-mode for scss
  (--map-first
   (and (eq (plist-get it :name) 'scss)
        (eq (plist-get it :type) 'style))
   (plist-put it :mode 'scss-mode)
   vue-modes))


;;; Major mode: xml

(use-package nxml-mode
  :ensure nil
  :defer t
  :hook (nxml-mode-hook . w--nxml-mode-hook)
  :config
  (defun w--nxml-mode-hook ()
    (modify-syntax-entry ?< ".")
    (modify-syntax-entry ?> ".")
    (modify-syntax-entry ?/ ".")
    (modify-syntax-entry ?: "_")
    (reformatter-dwim-select 'xml-format)))

(use-package xml-format
  :load-path "lisp/"
  :demand t
  :after nxml-mode)


;;; Major mode: yaml

(use-package yaml-mode
  :defer t
  :hook (yaml-mode-hook . w--yaml-mode-hook)
  :config
  (defun w--yaml-mode-hook ()
    (setq evil-shift-width yaml-indent-offset)
    (evil-swap-keys-swap-colon-semicolon)
    (evil-swap-keys-swap-double-single-quotes)
    (origami-mode)
    (evil--add-to-alist
     'origami-parser-alist
     'yaml-mode 'w--origami-parser-imenu-flat)))


;;; Local configuration

(load (concat user-emacs-directory "init-local") t)


(provide 'init)
;;; init.el ends here
