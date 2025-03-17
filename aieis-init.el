;;; aieis-init.el --- Main init file
;;; Commentary:

;;; Code:

;;; Setup package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Set Variables
(setq column-number-mode t)
(setq display-buffer-base-action '(display-buffer-reuse-window (reusable-frames . 1)))
(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)
(setq global-auto-revert-mode t)
(setq global-hl-line-mode t)
(setq blink-cursor-mode 0)
(setq truncate-lines t)
(setq indent-tabs-mode nil)
(setq electric-pair-mode 1)
(setq delete-old-versions t)
(setq recentf-mode 1)
(setq native-comp-async-report-warnings-errors 'silent)
(setq org-startup-indented t)
(setq org-hide-leading-stars t)
(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024))

(setq backup-directory-alist '(("" . "~/.emacs.d/backup/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/saves/" t)))
(setq lock-file-name-transforms `((".*" "~/.emacs.d/lockfiles/" t)))

(defun aieis/terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq visible-bell nil ring-bell-function 'aieis/terminal-visible-bell)

(define-key global-map (kbd "M-J") 'delete-other-windows)
(define-key global-map (kbd "C-o") 'recentf-open)
(define-key global-map (kbd "M-j") 'other-window)
(define-key global-map (kbd "M-k") (lambda () (interactive) (other-window -1)))

;; List of packages
;; TODO: make more dynamic (some-sort of require)
(setq-default
 aieis/packages
 '(org-roam
    magit
    marginalia
    consult
    embark
    embark-consult
    orderless
    ace-window
    vertico
    transpose-frame

    company
    haskell-mode
    rust-mode
    pyvenv
    pyvenv-auto

    lsp-mode
    lsp-ui
    lsp-haskell
    lsp-pyright

    flycheck
    string-inflection

    moe-theme
    modus-themes
    all-the-icons
    all-the-icons-dired
    dired-hide-dotfiles))

(defun aieis/install-packages (packages)
  "Install all the packages in the list PACKAGES."

  (interactive)
  (dolist (package packages)
    (package-install package)))

(add-to-list 'load-path (concat user-emacs-directory "manual-package/" ))
(add-to-list 'load-path (concat user-emacs-directory "manual-package/" "lsp-bridge/"))

(require 'odin-mode)
(require 'json-mode)

(add-to-list 'load-path (concat user-emacs-directory "lisp/" ))
(add-to-list 'load-path (concat user-emacs-directory "lisp/lang/" ))
(add-to-list 'load-path (concat user-emacs-directory "lisp/mode/" ))

(setq-default flycheck-emacs-lisp-load-path 'inherit)
(require 'utils)
(require 'read-only-enhanced)
(require 'filemanager)
(require 'sys-utils)
(require 'sys-vars)
(require 'proglang)
(require 'prog-ue)
(require 'local)
(require 'agenda)
(require 'org-setup)
(require 'font-setup)
(require 'async-shell)
(require 'misc-keybinds)
(defvar aieis/local-packages)
(aieis/install-packages aieis/local-packages)

;; (aieis/install-packages aieis/packages)


;; M-x utilities and mini-buffer
(use-package orderless
  :custom (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark
  :config
  (define-key global-map (kbd "C-;") #'embark-act)
  (define-key minibuffer-local-map (kbd "C-'") #'embark-collect)
  (define-key minibuffer-local-map (kbd "C-,") #'embark-become))

(use-package consult
  :config
  (consult-customize consult-theme :preview-key '(:debounce 0.5 any)))

(use-package embark-consult)

(use-package vertico
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package magit)


;; Misc man functions

(require 'thingatpt)
(defun aieis/man-b (&optional target)
  "Show man pages in a separate consistent frame with TARGET."
  (interactive)
  (let* ((frame (aieis/ensure-visible-frame-pattern "\\*Man .*\\*" "*Man Pages*"))
         (window (frame-first-window frame))
         (buf (if target (man target) (call-interactively 'man)))
         (nwindow (get-buffer-window buf)))
    (unless (eq window nwindow)
      (progn
        (delete-window nwindow)
        (set-window-buffer window buf)))))


(defun aieis/man ()
  "Show man pages in a separate consistent frame with interactive query."
  (interactive)
  (aieis/man-b 'nil))

(defun aieis/man-at-point ()
  "Show man page for symbol at point."
  (interactive)
  (let ((target (symbol-at-point)))
    (aieis/man-b (symbol-name target))))


;; Text editing functions
(defun aieis/kill-line-zero-space ()
  "Kill line and indent."
  (interactive)
  (progn
    (kill-line)
    (just-one-space 0)
    (indent-for-tab-command)))

(defun aieis/sudo-find-file (file)
  "Open FILE as root."
  (interactive (list (read-file-name "Open as root: ")))
  (find-file (if (file-writable-p file)
                 file
               (concat "/sudo:root@localhost:" file))))


(define-key global-map (kbd "C-k") #'aieis/kill-line-zero-space)
(define-key global-map (kbd "M-]") #'aieis/man-at-point)


;; Advanced Window Configurations
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil))

(let ((add-display-buffer-alist
       `(
         ("HIDDEN BUFFER"
          (display-buffer-no-window)
          (allow-no-window . t))
         ("\\*.*shell\\*"
          (display-buffer-reuse-mode-window display-buffer-in-direction)
          (direction . right))
         ;; ("\\*.*[C|c]ompilation.*\\*"
         ;;  (display-buffer-reuse-mode-window display-buffer-in-side-window)
         ;;  (side . right))
         ("\\*Embark Actions\\*"
          (display-buffer-reuse-mode-window display-buffer-at-bottom)
          (window-height . fit-window-to-buffer)
          (window-parameters . ((no-other-window . t)
                                (mode-line-format . none))))
         ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
          nil
          (window-parameters (mode-line-format . none)))
         ("\\*Man .*"
          (display-buffer-reuse-mode-window)
          (reusable-frames . visible))
         ("\\(\\*Agenda Commands.*\\|*Org Agenda.*\\)"
          (display-buffer-reuse-mode-window)
          (reusable-frames . visible)))))
  (setq display-buffer-alist (append display-buffer-alist add-display-buffer-alist)))

;; Themes
(use-package moe-theme)
(use-package modus-themes)
(use-package ef-themes)
(use-package ayu-theme)

(with-eval-after-load 'ayu-theme
  (load-theme 'ayu-dark t)
  (set-face-attribute 'region nil ':extend t :background "royal blue"))

(provide 'aieis-init)
;;; aieis-init.el ends here
