;;; init.el --- emacs configuration

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

;; Backup and autosave files
(setq
 auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-saves/\\1" t))
 backup-directory-alist '((".*" . "~/.emacs.d/backups/"))
 make-backup-files nil)

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

;; TODO: highlight FIXME/TODO/XXX in comment strings

;; Feedback while typing
(setq echo-keystrokes 0.5)
(show-paren-mode t)

;; Reduce clutter
(setq
 frame-resize-pixelwise t
 inhibit-startup-screen t
 initial-scratch-message nil
 ns-use-native-fullscreen nil)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(toggle-frame-maximized)
(tool-bar-mode -1)

;; Theme
(setq
 solarized-scale-org-headlines nil
 solarized-use-less-bold t
 solarized-use-variable-pitch nil
 solarized-height-minus-1 1.0
 solarized-height-plus-1 1.0
 solarized-height-plus-2 1.0
 solarized-height-plus-3 1.0
 solarized-height-plus-4 1.0)
(load-theme 'solarized-dark t)
(defvar my-active-theme 'solarized-dark "The currently active theme.")
(defun toggle-dark-light-theme ()
  "Toggle between a dark and light theme."
  (interactive)
  (if (eq my-active-theme 'solarized-light)
      (setq my-active-theme 'solarized-dark)
    (setq my-active-theme 'solarized-light))
  (load-theme my-active-theme t))

;; Cursor
(blink-cursor-mode 0)
(setq
   evil-normal-state-cursor   '("#859900" box)     ; green
   evil-visual-state-cursor   '("#cb4b16" box)     ; orange
   evil-insert-state-cursor   '("#268bd2" bar)     ; blue
   evil-replace-state-cursor  '("#dc322f" bar)     ; red
   evil-operator-state-cursor '("#dc322f" hollow)  ; red
)

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
  " ARev"
  " Abbrev"
  " FlyC"
  " Undo-Tree"
  " company"
  " counsel"
  " hl-p"
  " hl-s"
  " ivy"
  " s-/"
  " snipe"
))
(setq
 sml/col-number-format "%c"
 sml/line-number-format "%l"
 sml/projectile-replacement-format "%s:")
(sml/setup)

;; Line numbering
(defun my-relative-line-numbers-format (offset)
  "Format relative line number for OFFSET."
  (number-to-string (abs (if (= offset 0) (line-number-at-pos) offset))))
(setq relative-line-numbers-format 'my-relative-line-numbers-format)


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


;;;
;;; Scrolling
;;;

(setq
 scroll-conservatively 101
 scroll-margin 5)


;;;
;;; Evil
;;;

(setq
 evil-cross-lines t
 evil-want-C-u-scroll t
 evil-want-C-w-in-emacs-state t)

(require 'evil)
(require 'evil-magit)

(evil-mode)
(evil-commentary-mode)
(global-evil-surround-mode)
(global-evil-visualstar-mode)

;; extra text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
(define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
(define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block)
(evil-indent-plus-default-bindings)

;; god-mode integration using ; as the prefix key
(evil-define-key 'motion global-map
  ";" 'evil-execute-in-god-state)

;; directory navigation (inspired by vim vinagre)
(evil-define-key 'motion global-map "-" 'dired-jump)
(define-key dired-mode-map "-" 'dired-jump)

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
  "[e" 'previous-error
  "]e" 'next-error
  "[E" 'first-error
  "]E" 'my-last-error
  "[m" 'move-text-up
  "]m" 'move-text-down
  "[s" 'highlight-symbol-prev
  "]s" 'highlight-symbol-next
  "[S" 'highlight-symbol-prev-in-defun
  "]S" 'highlight-symbol-next-in-defun
  "[w" 'evil-window-prev
  "]w" 'evil-window-next
)

;; Misc
(evil-define-key 'insert global-map
  (kbd "RET") 'evil-ret-and-indent)
(defun my-evil-fill-paragraph-dwim ()
  "Dwim helper to fill the current paragraph"
  (interactive)
  ;; Move point after comment marker; useful for multi-line comments.
  (end-of-line)
  (fill-paragraph)
  (evil-first-non-blank))
(evil-define-key 'motion global-map
  "Q" 'my-evil-fill-paragraph-dwim
  (kbd "M-j") 'move-text-down
  (kbd "M-k") 'move-text-up)


;;;
;;; leader key shortcuts
;;;

(defvar my-leader-map
  (make-sparse-keymap)
  "Keymap for 'leader key' shortcuts.")
(evil-define-key 'motion global-map "," my-leader-map)
(evil-define-key nil my-leader-map
  ;; augmented in various other places
  " " 'whitespace-cleanup
  "b" 'ivy-switch-buffer
  "B" 'ivy-switch-buffer-other-window
  "f" 'counsel-find-file
  "F" 'find-file-other-window
  "k" (lambda () (interactive) (kill-buffer nil))
  "K" 'kill-buffer-and-window
  "s" 'save-buffer
  "S" 'save-some-buffers
  "u" 'universal-argument
  "x" 'counsel-M-x
  "+" 'evil-numbers/inc-at-pt
  "=" 'evil-numbers/inc-at-pt  ;; without shift key
  "-" 'evil-numbers/dec-at-pt)


;;;
;;; movement
;;;

;; j/k should move visual lines. do not modify evil-motion-state,
;; since that will break operators taking a motion, e.g. 'dj' to
;; delete the current and next line.
(dolist (state '(normal visual))
  (evil-define-key state global-map
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line
    (kbd "C-j") 'evil-next-line
    (kbd "C-k") 'evil-previous-line))

;; some emacs bindings in insert mode
(evil-define-key 'insert global-map
  (kbd "C-a") 'evil-first-non-blank
  (kbd "C-e") 'end-of-line)

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

;; avy and evil-easymotion
(setq
 avy-all-windows nil
 avy-all-windows-alt t
 avy-background t)
(defun my-avy-move-region ()
  "Select two lines and move the text between them here."
  ;; Shamelessly taken from https://github.com/abo-abo/avy/pull/75
  (interactive)
  (avy-with avy-move-region
    (let* ((beg (avy--line))
           (end (save-excursion
                  (goto-char (avy--line))
                  (forward-line)
                  (point)))
           (text (buffer-substring beg end))
           (pad (if (bolp) "" "\n")))
      (move-beginning-of-line nil)
      (delete-region beg end)
      (insert text pad))))
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
(avy-setup-default)
(evilem-default-keybindings "SPC")
(evil-define-key 'motion global-map
  (kbd "SPC SPC") 'avy-goto-char-timer
  (kbd "SPC S-SPC") 'my-avy-goto-char-timer-any-window
  (kbd "S-SPC S-SPC") 'my-avy-goto-char-timer-any-window
  (kbd "SPC l") 'avy-goto-line
  (kbd "SPC L") 'my-avy-goto-line-any-window)
(evil-define-key 'normal global-map
  (kbd "SPC a") (lambda () (interactive) (avy-goto-char-timer) (call-interactively 'evil-append))
  (kbd "SPC A") (lambda () (interactive) (avy-goto-line) (call-interactively 'evil-append-line))
  (kbd "SPC c") (lambda () (interactive) (avy-goto-line) (evil-first-non-blank) (call-interactively 'evil-change-line))
  (kbd "SPC C") 'my-avy-evil-change-region
  (kbd "SPC d") 'my-avy-evil-delete-line
  (kbd "SPC D") 'my-avy-evil-delete-region
  (kbd "SPC i") (lambda () (interactive) (avy-goto-char-timer) (call-interactively 'evil-insert))
  (kbd "SPC I") (lambda () (interactive) (avy-goto-line) (call-interactively 'evil-insert-line))
  (kbd "SPC o") (lambda () (interactive) (avy-goto-line) (call-interactively 'evil-open-below))
  (kbd "SPC O") (lambda () (interactive) (avy-goto-line) (call-interactively 'evil-open-above))
  (kbd "SPC p d") (lambda () (interactive) (next-line) (call-interactively 'avy-move-line))
  (kbd "SPC p D") (lambda () (interactive) (next-line) (call-interactively 'my-avy-move-region))
  (kbd "SPC P d") 'avy-move-line
  (kbd "SPC P D") 'my-avy-move-region
  (kbd "SPC p y") (lambda () (interactive) (next-line) (call-interactively 'avy-copy-line))
  (kbd "SPC p Y") (lambda () (interactive) (next-line) (call-interactively 'avy-copy-region))
  (kbd "SPC P y") 'avy-copy-line
  (kbd "SPC P Y") 'avy-copy-region)

;; evil-snipe. the t/T/f/F overrides are the most important ones,
;; since avy/evil-easymotion already allows for fancy jumps, e.g. via
;; avy-goto-char-timer.
(setq
 evil-snipe-auto-disable-substitute nil
 evil-snipe-override-evil-repeat-keys nil
 evil-snipe-scope 'line
 evil-snipe-repeat-scope 'line
 evil-snipe-spillover-scope 'buffer
 evil-snipe-smart-case t
 evil-snipe-tab-increment t)
(evil-snipe-mode 1)
(evil-snipe-override-mode 1)
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


;;;
;;; toggles
;;;

(defhydra hydra-toggle (:exit t :foreign-keys warn) "
toggle  \
«_b_»ackgound  \
fill-«_c_»olumn  \
«_f_»ill  \
«_l_»ine  \
«_m_»aximize  \
«_n_»umber  \
«_r_»elative-number  \
«_t_»runcate  \
«_v_»isual-line  \
«_w_»riteroom  \
«_SPC_» whitespace"
  ("<escape>" nil nil)
  ("b" toggle-dark-light-theme nil)
  ("c" fci-mode nil)
  ("f" auto-fill-mode nil)
  ("l" hl-line-mode nil)
  ("m" toggle-frame-fullscreen nil)
  ("M" toggle-frame-maximized nil)
  ("n" (progn (relative-line-numbers-mode -1) (linum-mode 'toggle)) nil)
  ("N" (progn (line-number-mode 'toggle) (column-number-mode 'toggle)) nil)
  ("r" (progn (linum-mode -1) (relative-line-numbers-mode 'toggle)) nil)
  ("t" toggle-truncate-lines nil)
  ("v" visual-line-mode nil)
  ("SPC" whitespace-mode nil)
  ("w" writeroom-mode nil)
  ("W" (progn (delete-other-windows) (writeroom-mode 'toggle)) nil)
)
(define-key my-leader-map "t" 'hydra-toggle/body)


;;;
;;; Zooming / text size
;;;

(require 'default-text-scale)  ;; functions below use non-autoloaded functions
(setq my-default-text-scale-height 110)
(defun my-default-text-scale-set (height)
  (interactive "nHeight (e.g. 110) ")
  (default-text-scale-increment (- height (face-attribute 'default :height))))
(defun my-default-text-scale-reset ()
  (interactive)
  (my-default-text-scale-set my-default-text-scale-height))
(when (display-graphic-p)
  (my-default-text-scale-reset))
(defhydra hydra-zoom () "
zoom  \
«_i_»n  \
«_o_»ut  \
«_z_» normal  \
    \
writeroom  \
«_n_»arrower  \
«_w_»ider  \
«_r_»eset"
  ("<escape>" nil nil)
  ("i" default-text-scale-increase nil)
  ("o" default-text-scale-decrease nil)
  ("z" my-default-text-scale-reset nil :exit t)
  ("0" my-default-text-scale-reset nil :exit t)
  ("=" default-text-scale-increase nil)
  ("+" default-text-scale-increase nil)
  ("-" default-text-scale-decrease nil)
  ("." hydra-repeat nil)
  ("n" (writeroom-decrease-width) nil)
  ("w" (writeroom-increase-width) nil)
  ("r" (writeroom-adjust-width nil) nil :exit t)
)
(define-key my-leader-map "z" 'hydra-zoom/body)


;;
;; Window layout
;;
;; Preferred layout is full-height windows, up to three next to each
;; other in a horizontal fashion, i.e. screen divided into columns.
;;

(defun my-evil-window-next-or-vsplit ()
  "Focus next window, or vsplit if it is the only window in this frame."
  (interactive)
  (if (> (count-windows) 1)
      (call-interactively 'evil-window-next)
    (evil-window-vsplit)))
(setq
 help-window-select t
 split-height-threshold nil
 split-width-threshold 120
 split-window-preferred-function 'visual-fill-column-split-window-sensibly
 evil-split-window-below t
 evil-vsplit-window-right t
 writeroom-global-effects nil
 writeroom-maximize-window nil)
(defun my-evil-goto-window-1 ()
  "Go to the first window."
  (interactive) (evil-window-top-left))
(defun my-evil-goto-window-2 ()
  "Go to the second window."
  (interactive) (evil-window-top-left) (evil-window-next 2))
(defun my-evil-goto-window-3 ()
  "Go to the third window."
  (interactive) (evil-window-top-left) (evil-window-next 3))
(defun my-evil-goto-window-4 ()
  "Go to the fourth window."
  (interactive) (evil-window-top-left) (evil-window-next 4))
(evil-define-key 'motion global-map
  (kbd "C-1") 'my-evil-goto-window-1
  (kbd "C-2") 'my-evil-goto-window-2
  (kbd "C-3") 'my-evil-goto-window-3
  (kbd "C-4") 'my-evil-goto-window-4)
(evil-define-key nil my-leader-map
  "w" evil-window-map
  "1" 'my-evil-goto-window-1
  "2" 'my-evil-goto-window-2
  "3" 'my-evil-goto-window-3
  "4" 'my-evil-goto-window-4)
(evil-define-key nil evil-window-map ;; augment C-w map
  (kbd "m") 'hydra-window-move/body
  (kbd "C-m") 'hydra-window-move/body
  (kbd "n") 'evil-window-vnew
  (kbd "C-n") 'evil-window-vnew
  (kbd "q") 'evil-window-delete
  (kbd "C-q") 'evil-window-delete
  (kbd "w") 'my-evil-window-next-or-vsplit
  (kbd "C-w") 'my-evil-window-next-or-vsplit
  (kbd "1") 'my-evil-goto-window-1
  (kbd "C-1") 'my-evil-goto-window-1
  (kbd "2") 'my-evil-goto-window-2
  (kbd "C-2") 'my-evil-goto-window-2
  (kbd "3") 'my-evil-goto-window-3
  (kbd "C-3") 'my-evil-goto-window-3
  (kbd "4") 'my-evil-goto-window-4
  (kbd "C-4") 'my-evil-goto-window-4)
(defhydra hydra-window-move (:foreign-keys warn) "
window  \
«_h_» left  \
«_j_» down  \
«_k_» up  \
«_l_» right  \
«_r_»otate"
  ("<escape>" nil nil)
  ("<return>" nil nil)
  ("h" buf-move-left nil)
  ("H" evil-window-move-far-left nil :exit t)
  ("j" buf-move-down nil)
  ("J" evil-window-move-very-bottom nil :exit t)
  ("k" buf-move-up nil)
  ("K" evil-window-move-very-top nil :exit t)
  ("l" buf-move-right nil)
  ("L" evil-window-move-far-right nil :exit t)
  ("r" evil-window-rotate-downwards nil)
  ("R" evil-window-rotate-upwards nil)
  ("." hydra-repeat nil))
(advice-add 'delete-window :after '(lambda (&rest args) (balance-windows)))
(advice-add 'display-buffer :after '(lambda (&rest args) (balance-windows)))


;;
;; Projects
;;

(setq
 projectile-ignored-projects '("/usr/local/")
 projectile-mode-line nil
 projectile-require-project-root nil)
(projectile-global-mode)
(defhydra hydra-project (
  :exit t
  :foreign-keys warn
  ) "
project  \
«_b_»uffer  \
«_d_»ir  \
«_f_»ile  \
«_k_»ill  \
«_p_»roject  \
«_s_»ave  \
«_t_»est/impl  \
«_-_» top dir"
  ("<escape>" nil nil)
  ("b" projectile-switch-to-buffer nil)
  ("B" projectile-switch-to-buffer-other-window nil)
  ("-" projectile-dired nil)
  ("d" projectile-find-dir nil)
  ("D" projectile-find-dir-other-window nil)
  ("f" projectile-find-file nil)
  ("F" projectile-find-file-other-window nil)
  ("k" projectile-kill-buffers nil)
  ("p" projectile-switch-open-project nil)
  ("P" projectile-switch-project nil)
  ("s" projectile-save-project-buffers nil)
  ;; Use the "other window" variant for the lowercase version for
  ;; switching between test and implementation since that is generally
  ;; more useful.
  ("t" projectile-find-implementation-or-test-other-window nil)
  ("T" projectile-toggle-between-implementation-and-test nil)
)
(define-key my-leader-map "p" 'hydra-project/body)


;;;
;;; Version control
;;;

(setq
 auto-revert-check-vc-info t
 magit-branch-prefer-remote-upstream '("master")
 magit-branch-read-upstream-first nil
 magit-prefer-remote-upstream t
 magit-process-popup-time 10
 magit-tag-arguments '("--annotate"))

;; Pop-ups sometimes contain trailing whitespace.
(add-hook 'magit-popup-mode-hook 'my-hide-trailing-whitespace)

;; Magit shortcuts
(defhydra hydra-git (:exit t :foreign-keys warn) "
git  \
«_!_»command  \
«_b_»lame  \
«_c_»ommit  \
«_d_»iff  \
«_f_»ile  \
«_g_» popup  \
«_l_»og  \
«_s_»tatus  \
«_w_»eb"
  ("<escape>" nil nil)
  ("!" magit-git-command nil)
  ("b" magit-blame nil)
  ("c" magit-commit nil)
  ("d" magit-diff nil)
  ("f" counsel-git nil)
  ("g" magit-dispatch-popup nil)
  ("l" magit-log-current nil)
  ("L" magit-log-all nil)
  ("s" magit-status nil)
  ("S"
   (lambda ()
    "Open git status for another repository."
    (interactive)
    (setq current-prefix-arg t)
    (call-interactively 'magit-status))
   nil)
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
   nil)
)
(define-key my-leader-map "g" 'hydra-git/body)


;;;
;;; Search
;;;

;; isearch
(setq isearch-allow-prefix nil)

;; swiper
(define-key my-leader-map "/" 'swiper)

;; ag, the silver searcher
(setq ag-reuse-buffers t)
(defhydra hydra-ag (:exit t :foreign-keys warn) "
ag  \
«_g_» project  \
«_f_»iles  \
«_r_»egex"
  ("<escape>" nil nil)
  ("f" ag-project-files nil)
  ("F" ag-files nil)
  ("g" ag-project nil)
  ("G" ag nil)
  ("r" ag-project-regexp nil)
  ("R" ag-regexp nil)
)
(define-key my-leader-map "a" 'hydra-ag/body)
(add-hook 'ag-mode-hook (lambda ()
  (toggle-truncate-lines t)))

;; swiper style search using ag; uses shift-/, since it's conceptually
;; an alternative to swiper.
(defun my-thing-at-point-dwim ()
  "Returns the symbol at point, or the region contents if activated."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'symbol t)))
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
(evil-define-key nil my-leader-map
  "h" 'highlight-symbol
  "H" 'highlight-symbol-remove-all
  "r" 'highlight-symbol-query-replace)
(evil-define-key 'visual global-map
  (kbd ", h") (lambda (start end) (interactive "r")
    (highlight-symbol-add-symbol (buffer-substring start end))))

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

;; company
(require 'company)
(setq
 company-auto-complete 'company-explicit-action-p
 company-dabbrev-downcase nil
 company-dabbrev-ignore-case t
 company-idle-delay nil
 company-selection-wrap-around t
 company-require-match nil
 evil-complete-next-func (lambda (arg) (company-manual-begin))
 evil-complete-previous-func (lambda (arg) (call-interactively 'company-dabbrev))
)
(add-to-list 'company-auto-complete-chars ?\( )
(add-hook 'after-init-hook 'global-company-mode)
(evil-define-key nil company-active-map
  (kbd "C-n") 'company-select-next
  (kbd "C-p") 'company-select-previous)


;;;
;;; Programming
;;;

(evil-define-key 'insert prog-mode-map
  (kbd "RET") 'comment-indent-new-line)

;; Flycheck
(setq
 flycheck-checker-error-threshold 1000
 flycheck-display-errors-delay 1.0)
(global-flycheck-mode)


;;;
;;; Major modes
;;;

;; text editing
(add-hook 'text-mode-hook (lambda ()
  (auto-fill-mode)
  (visual-line-mode)))

;; programming languages
(add-hook 'prog-mode-hook (lambda ()
  (abbrev-mode)
  (auto-fill-mode)
  (column-number-mode)
  (highlight-symbol-mode)))

;; jinja
(add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode))

;; JSON
(setq json-reformat:indent-width 2)
(add-hook 'json-mode-hook (lambda ()
  (setq
   tab-width json-reformat:indent-width
   evil-shift-width tab-width)))

;; latex
(setq TeX-engine 'xetex)

;; Python
(add-hook
 'python-mode-hook
 (lambda ()
   (setq
    fill-column 72
    python-fill-docstring-style 'symmetric)))
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
(evil-define-key 'motion python-mode-map
  (kbd "SPC g d") 'my-swiper-python-definitions
  (kbd "SPC TAB") 'my-easymotion-python)

;; reStructuredText
(setq
 rst-default-indent 0
 rst-indent-comment 2
 rst-indent-field 2
 rst-indent-literal-normal 2
 rst-preferred-adornments '(
   (?= over-and-under 0)
   (?= simple 0)
   (?- simple 0)
   (?~ simple 0)
   (?+ simple 0)
   (?` simple 0)
   (?# simple 0)
   (?@ simple 0))
 rst-preferred-bullets '(?- ?*))
(add-hook 'rst-mode-hook (lambda ()
  (setq evil-shift-width 2)
  (modify-syntax-entry ?_ "w")
))
(evilem-make-motion
 my-easymotion-rst
 (list 'rst-forward-section 'rst-backward-section)
 :pre-hook (setq evil-this-type 'line))
(evil-define-key 'motion rst-mode-map
  (kbd "SPC TAB") 'my-easymotion-rst)

;; Shell
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
