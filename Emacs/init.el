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

(fset 'yes-or-no-p 'y-or-n-p)
(modify-syntax-entry ?_ "w")
(setq tls-checktrust 'ask)

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
(setq solarized-use-less-bold t)
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
  " FlyC"
  " Undo-Tree"
  " hl-p"
  " hl-s"
  " ivy"
  " s-/"
))
(sml/setup)
(setq
 sml/col-number-format "%c"
 sml/line-number-format "%l")

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
;;; Completion
;;;

;; ivy, counsel
(setq
 ivy-wrap t
 ivy-count-format "(%d/%d) "
 magit-completing-read-function 'ivy-completing-read)
(ivy-mode 1)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key [remap find-file] 'counsel-find-file)
(global-set-key [remap switch-to-buffer] 'ivy-switch-buffer)
(global-set-key [remap describe-function] 'counsel-describe-function)
(global-set-key [remap describe-variable] 'counsel-describe-variable)


;;;
;;; Evil
;;;

(setq
 evil-want-C-u-scroll t
 evil-cross-lines t)

(require 'evil)
(require 'evil-magit)

(evil-mode)
(evil-commentary-mode)
(global-evil-surround-mode)
(global-evil-visualstar-mode)

;; Text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
(evil-indent-plus-default-bindings)

;; Movement
(evil-define-key 'motion global-map
  (kbd "C-j") 'evil-next-visual-line
  (kbd "C-k") 'evil-previous-visual-line)

;; Shortcuts using a "leader key" as a prefix
(defvar my-leader-map
  (make-sparse-keymap)
  "Keymap for 'leader key' shortcuts.")
(evil-define-key 'motion global-map "," my-leader-map)
(define-key my-leader-map " " 'whitespace-cleanup)
(define-key my-leader-map "f" 'counsel-find-file)
(define-key my-leader-map "F" (lambda()
  "Show buffer menu with open files"
  (interactive) (buffer-menu t)))
(define-key my-leader-map "b" 'ivy-switch-buffer)
(define-key my-leader-map "B" 'buffer-menu)
(define-key my-leader-map "k" (lambda () (interactive) (kill-buffer nil)))
(define-key my-leader-map "o" 'occur-dwim)
(define-key my-leader-map "q" 'kill-buffer-and-window)
(define-key my-leader-map "r" 'highlight-symbol-query-replace)
(define-key my-leader-map "s" 'swiper)
(define-key my-leader-map "w" 'save-buffer)
(define-key my-leader-map "W" 'save-some-buffers)
(define-key my-leader-map "u" 'universal-argument)
(define-key my-leader-map "x" 'counsel-M-x)
(define-key my-leader-map "+" 'evil-numbers/inc-at-pt)
(define-key my-leader-map "-" 'evil-numbers/dec-at-pt)
(define-key my-leader-map "," 'ivy-resume)

;; Directory navigation (inspired by vim vinagre)
(evil-define-key 'motion global-map "-" 'dired-jump)
(define-key dired-mode-map "-" 'dired-jump)

;; Previous/next thing (inspired by vim unimpaired)
(defun last-error ()
  "Jump to the last error; similar to 'first-error'."
  (interactive) (condition-case err (while t (next-error)) (user-error nil)))
(evil-define-key 'motion global-map
  (kbd "[ SPC") (lambda () (interactive)
    (evil-insert-newline-above)
    (evil-line 2))
  (kbd "] SPC") (lambda () (interactive)
    (evil-insert-newline-below)
    (evil-line 0))
  "[b" 'evil-prev-buffer
  "]b" 'evil-next-buffer
  "[c" 'flycheck-previous-error
  "]c" 'flycheck-next-error
  "[C" (lambda () (interactive)
    (goto-char (point-min))
    (flycheck-next-error))
  "]C" (lambda () (interactive)
    (goto-char (point-max))
    (flycheck-previous-error))
  "[e" 'previous-error
  "]e" 'next-error
  "[E" 'first-error
  "]E" 'last-error
  "[m" 'move-text-up
  "]m" 'move-text-down
  "[s" 'highlight-symbol-prev
  "]s" 'highlight-symbol-next
  "[S" 'highlight-symbol-prev-in-defun
  "]S" 'highlight-symbol-next-in-defun
  "[w" 'evil-window-prev
  "]w" 'evil-window-next
)
(evil-define-key 'normal global-map
  (kbd "C-p") 'highlight-symbol-prev
  (kbd "C-n") 'highlight-symbol-next
)

;; Single key prefix key for god-mode integration
(evil-define-key 'motion global-map
  ";" 'evil-execute-in-god-state)
(evil-define-key 'god global-map
  [escape] 'evil-god-state-bail
  ";" 'evil-repeat-find-char)  ;; makes ';;' act like original ';'


;; Misc
(evil-define-key 'insert global-map
  (kbd "RET") 'evil-ret-and-indent
  (kbd "C-a") 'evil-first-non-blank
  (kbd "C-l") 'end-of-line)
(defun my-evil-fill-paragraph ()
  "Dwim helper to fill the current paragraph"
  (interactive)
  ;; Move point after comment marker; useful for multi-line comments.
  (end-of-line)
  (fill-paragraph)
  (evil-first-non-blank))
(evil-define-key 'motion global-map
  "Q" 'my-evil-fill-paragraph
  (kbd "M-j") 'move-text-down
  (kbd "M-k") 'move-text-up)


;;;
;;; Toggles
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
«_w_»hitespace"
  ("<escape>" nil nil)
  ("b" toggle-dark-light-theme nil)
  ("c" fci-mode nil)
  ("f" auto-fill-mode nil)
  ("l" hl-line-mode nil)
  ("m" toggle-frame-fullscreen nil)
  ("M" toggle-frame-maximized nil)
  ("n" (progn
     (relative-line-numbers-mode -1)
     (linum-mode 'toggle)) nil)
  ("r" (progn
     (linum-mode -1)
     (relative-line-numbers-mode 'toggle)) nil)
  ("t" toggle-truncate-lines nil)
  ("v" visual-line-mode nil)
  ("w" whitespace-mode nil)
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
«_z_» normal"
  ("<escape>" nil nil)
  ("i" default-text-scale-increase nil)
  ("o" default-text-scale-decrease nil)
  ("z" my-default-text-scale-reset nil :exit t)
  ("0" my-default-text-scale-reset nil :exit t)
  ("=" default-text-scale-increase nil)
  ("+" default-text-scale-increase nil)
  ("-" default-text-scale-decrease nil)
  ("." hydra-repeat nil)
)
(define-key my-leader-map "z" 'hydra-zoom/body)


;;
;; Window splitting
;;

(setq
 help-window-select t
 split-height-threshold nil
 split-width-threshold 120
 evil-split-window-below t
 evil-vsplit-window-right t)
(evil-define-key 'motion global-map
  (kbd "C-w n") 'evil-window-vnew
  (kbd "C-w 1") 'evil-window-top-left
  (kbd "C-w 2") (lambda () (interactive) (evil-window-top-left) (evil-window-next 2))
  (kbd "C-w 3") (lambda () (interactive) (evil-window-top-left) (evil-window-next 3))
  (kbd "C-w 4") (lambda () (interactive) (evil-window-top-left) (evil-window-next 4)))
(advice-add 'delete-window :after '(lambda (&rest args) (balance-windows)))
(advice-add 'display-buffer :after '(lambda (&rest args) (balance-windows)))


;;;
;;; Version control
;;;

(setq
 auto-revert-check-vc-info t
 magit-process-popup-time 10)

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
«_g_»rep  \
«_l_»og  \
«_p_»opup  \
«_s_»tatus  \
«_w_»eb"
  ("<escape>" nil nil)
  ("!" magit-git-command nil)
  ("b" magit-blame nil)
  ("c" magit-commit nil)
  ("d" magit-diff nil)
  ("f" counsel-git nil)
  ("g" vc-git-grep nil)
  ("l" magit-log-current nil)
  ("L" magit-log-all nil)
  ("p" magit-dispatch-popup nil)
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

;; Ag, the silver searcher
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

;; Highlighting
(setq
 highlight-symbol-idle-delay 1.0
 highlight-symbol-on-navigation-p t)
(evil-define-key 'motion global-map
  (kbd "SPC") 'highlight-symbol
  (kbd "S-SPC") 'highlight-symbol-remove-all)
(evil-define-key 'visual global-map
  (kbd "SPC") (lambda (start end) (interactive "r")
    (highlight-symbol-add-symbol (buffer-substring start end))))

;; Occur
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


;;;
;;; Programming
;;;

(evil-define-key 'insert prog-mode-map
  (kbd "RET") 'comment-indent-new-line)

;; Flycheck
(setq flycheck-display-errors-delay 1.0)
(global-flycheck-mode)


;;;
;;; Major modes
;;;

;; Programming languages
(defun my-prog-mode-hook ()
  (column-number-mode)
  (highlight-symbol-mode))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)

;; JSON
(setq json-reformat:indent-width 2)
(add-hook 'json-mode-hook (lambda ()
  (setq
   tab-width json-reformat:indent-width
   evil-shift-width tab-width)))

;; Python
(defun my-python-mode-hook ()
  (setq
   fill-column 72
   python-fill-docstring-style 'symmetric))
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; reStructuredText
(setq
 rst-default-indent 0
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
))

;; Shell
(add-to-list 'auto-mode-alist '("bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc-.*\\'" . sh-mode))

;; Yaml
(defun my-yaml-mode-hook ()
  (setq evil-shift-width yaml-indent-offset))
(add-hook 'yaml-mode-hook 'my-yaml-mode-hook)


(provide 'init)
;;; init.el ends here
