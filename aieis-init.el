;;; aieis-init.el --- Main init file
;;; Commentary:

;;; Code:

;; Set Variables
(column-number-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-auto-revert-mode t)
(global-hl-line-mode t)
(blink-cursor-mode 0)
(indent-tabs-mode -1)
(electric-pair-mode 1)
(recentf-mode 1)
(setq-default truncate-lines 1)
(setq-default display-buffer-base-action '(display-buffer-reuse-window (reusable-frames . 1)))
(setq-default delete-old-versions t)
(setq-default native-comp-async-report-warnings-errors 'silent)
(setq-default org-startup-indented t)
(setq-default org-hide-leading-stars t)
(setq-default gc-cons-threshold (* 100 1024 1024))
(setq-default read-process-output-max (* 1024 1024))
(setq-default backup-directory-alist '(("" . "~/.emacs.d/backup/")))
(setq-default auto-save-file-name-transforms `((".*" "~/.emacs.d/saves/" t)))
(setq-default lock-file-name-transforms `((".*" "~/.emacs.d/lockfiles/" t)))

(defun aieis/terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq visible-bell nil ring-bell-function 'aieis/terminal-visible-bell)

(define-key global-map (kbd "M-J") 'delete-other-windows)
(define-key global-map (kbd "C-o") 'recentf-open)
(define-key global-map (kbd "M-j") 'other-window)
(define-key global-map (kbd "M-k") (lambda () (interactive) (other-window -1)))

(add-to-list 'load-path (concat user-emacs-directory "manual-package/" ))
(add-to-list 'load-path (concat user-emacs-directory "manual-package/" "lsp-bridge/"))

(require 'odin-mode)
(require 'json-mode)

(add-to-list 'load-path (concat user-emacs-directory "lisp/" ))
(add-to-list 'load-path (concat user-emacs-directory "lisp/lang/" ))
(add-to-list 'load-path (concat user-emacs-directory "lisp/mode/" ))

(require 'aieis-package)
(aieis/use-package all-the-icons)
(aieis/use-package all-the-icons-dired :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
(aieis/use-package dired-hide-dotfiles :config (define-key dired-mode-map "." #'dired-hide-dotfiles-mode))

(aieis/use-package flycheck)
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

;; M-x utilities and mini-buffer
(aieis/use-package orderless
  :config
  (setq-default completion-styles '(orderless basic))
  (setq-default completion-category-overrides '((file (styles basic partial-completion)))))

(aieis/use-package marginalia
  :config
  (marginalia-mode))

(aieis/use-package embark
  :config
  (define-key global-map (kbd "C-;") #'embark-act)
  (define-key minibuffer-local-map (kbd "C-'") #'embark-collect)
  (define-key minibuffer-local-map (kbd "C-,") #'embark-become))

(aieis/use-package consult
  :config
  (consult-customize consult-theme :preview-key '(:debounce 0.5 any)))

(aieis/use-package embark-consult)

(aieis/use-package vertico
  :config
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(aieis/use-package savehist
  :init
  (savehist-mode))

(aieis/use-package magit :defer t
  :bind (("C-x g" . (lambda () (interactive) (require 'magit) (call-interactively 'magit-status)))))


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
(aieis/use-package ace-window
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
(aieis/use-package moe-theme)
(aieis/use-package modus-themes)
(aieis/use-package ef-themes)
(aieis/use-package ayu-theme)

(with-eval-after-load 'ayu-theme
  (load-theme 'ayu-dark t)
  (set-face-attribute 'region nil ':extend t :background "royal blue"))


(defun aieis/setup-init ()
  "Setup emacs config files as completely as possible."
  (interactive)
  (setq-local aieis/setup-start-time (current-time))
  (package-refresh-contents)
  (aieis/install-all-packages)
  (message "Installation of packages elapsed: %.6f" (float-time (time-since aieis/setup-start-time)))
  (setq-local aieis/load-init-time (current-time))
  (load user-init-file)
  (message "Init reload elapsed: %.6f" (float-time (time-since aieis/load-init-time)))
  (message "Total setup time: %.6f" (float-time (time-since aieis/setup-start-time))))

(provide 'aieis-init)
;;; aieis-init.el ends here
