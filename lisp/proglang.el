;;; proglang.el --- package providing code-specific configuration
;;; Commentary:
;;; Code:
(setq lsp-keymap-prefix "C-c l")
(setq lsp-pyright-multi-root nil)

(setq-default c-basic-offset 4)
(setq-default js-indent-level 4)

(defun aieis-c++-hook ()
  (c-set-offset 'substatement-open 0))

(add-to-list 'c-mode-common-hook 'aieis-c++-hook)

(require 'aieis-lang-common)
(require 'aieis-lang-python)
(require 'aieis-lang-js)
(require 'aieis-lang-odin)
(require 'aieis-lang-json)
(require 'aieis-lang-glsl)

(require 'aieis-package)

(aieis/use-package haskell-mode :defer t)
(aieis/use-package lsp-haskell :defer t)
(aieis/use-package rust-mode :defer t)
(aieis/use-package lua-mode :defer t)
(aieis/use-package pyvenv :defer t)
(aieis/use-package pyvenv-auto :defer t)
(aieis/use-package lsp-ui :defer t)

;; sh-mode
(push '("/[\\.]?[x]?profile\\'" . sh-mode) auto-mode-alist)

(aieis/use-package multiple-cursors
  :config
  (define-key global-map (kbd "C-c m m") 'mc/edit-lines)
  (define-key global-map (kbd "C-c m n") 'mc/mark-next-like-this)
  (define-key global-map (kbd "C-c m p") 'mc/mark-previous-like-this)
  (define-key global-map (kbd "C-c m a") 'mc/mark-all-like-this)
  (define-key mc/keymap  (kbd "C-n")     'mc/mark-next-like-this))

(add-to-list 'aieis/lsp-mode-hooks 'c-mode-common-hook)
(add-to-list 'aieis/lsp-mode-hooks 'rust-mode-hook)
(add-to-list 'aieis/lsp-mode-hooks 'python-mode-hook)
(add-to-list 'aieis/lsp-mode-hooks 'haskell-mode-hook)
(add-to-list 'aieis/lsp-mode-hooks 'odin-mode-hook)

;; FlyCheck
(declare-function aieis/frame-visible? "utils.el")
(defun aieis/flycheck-show-buffer-diagnostics-hook  ()
  "Bring up the fly* diagnostics menu."
  (interactive)
  (if (and (derived-mode-p 'prog-mode) (bound-and-true-p flycheck-mode) (aieis/frame-visible? "\\*Flycheck errors*"))
      (call-interactively 'flycheck-list-errors)))

;; Hooks
;; (add-hook 'flycheck-mode-hook 'aieis/flycheck-show-buffer-diagnostics-hook)
;; (add-hook 'window-configuration-change-hook 'aieis/flycheck-show-buffer-diagnostics-hook)

;; (let ((n-disp-list
;;        '("\\*Flycheck errors.*"
;;          (display-buffer-reuse-mode-window display-buffer-pop-up-frame)
;;          (reusable-frames . visible))))
;;   (add-to-list 'display-buffer-alist n-disp-list))

(aieis/use-package company
  :init
  (setq company-selection-wrap-around t)
  (setq company-idle-delay nil)
  :config (global-company-mode 1))


;; LSP Mode

(aieis/use-package lsp-mode
  :defer t
  :commands
  (lsp lsp-deffered)

  :init
  (message "lsp setup")
  (setq-default lsp-ruff-ruff-args "--preview")
  (setq-default lsp-auto-configure t)
  (setq-default lsp-enable-dap-auto-configure nil)
  (setq-default lsp-auto-guess-root t)
  (setq-default lsp-before-save-edits t)
  (setq-default lsp-eldoc-enable-hover t)
  (setq-default lsp-eldoc-render-all nil)
  (setq-default lsp-completion-enable t)
  (setq-default lsp-completion-show-detail t)
  (setq-default lsp-completion-show-kind t)
  (setq-default lsp-enable-file-watchers t)
  (setq-default lsp-file-watch-threshold 100)
  (setq-default lsp-enable-folding t)
  (setq-default lsp-enable-imenu t)
  (setq-default lsp-enable-indentation t)
  (setq-default lsp-enable-links t)
  (setq-default lsp-enable-on-type-formatting nil)
  (setq-default lsp-enable-snippet nil)
  (setq-default lsp-enable-symbol-highlighting nil)
  (setq-default lsp-enable-text-document-color nil)
  (setq-default lsp-enable-xref t)
  (setq-default lsp-flycheck-live-reporting nil)
  (setq-default lsp-idle-delay 0.5)
  (setq-default lsp-imenu-show-container-name t)
  (setq-default lsp-imenu-sort-methods '(position kind name))
  (setq-default lsp-pyls-plugins-flake8-enabled t)
  (setq-default lsp-signature-auto-activate t)
  (setq-default lsp-signature-render-documentation t)
  (setq-default lsp-signature-doc-lines 10)
  (setq-default lsp-ui-sideline-enable nil)
  (setq-default lsp-ui-sideline-show-hover nil)
  (setq-default lsp-ui-sideline-delay 0.5)
  (setq-default lsp-ui-doc-delay 5)
  (setq-default lsp-ui-sideline-ignore-duplicates t)
  (setq-default lsp-ui-doc-position 'top)
  (setq-default lsp-ui-doc-alignment 'frame)
  (setq-default lsp-ui-doc-header nil)
  (setq-default lsp-ui-doc-include-signature t)
  (setq-default lsp-ui-doc-use-childframe t)

  :config
  (dolist (hook aieis/lsp-mode-hooks)
    (add-hook 'hook #'lsp-deferred))

  (dolist (var aieis/lsp-servers)
    (let* ((mode (car var))
           (executable-path (cadr var))
           (server-id (caddr var))
           (lang-id (cadddr var))
           (client (make-lsp-client
                    :new-connection (lsp-stdio-connection executable-path)
		    :major-modes (list mode)
		    :server-id server-id
		    :multi-root t)))
      (message "Registering client: major-mode: %s server-id: %s executable: %s" mode server-id executable-path)
      (lsp-register-client client)
      (add-to-list 'lsp-language-id-configuration (cons mode lang-id))))

  (lsp-enable-imenu))

(aieis/use-package lsp-ui :defer t)

;; Keymap Keys
(define-key prog-mode-map (kbd "C-c f d") 'flycheck-list-errors)
(define-key prog-mode-map (kbd "C-c f n") 'flycheck-next-error)
(define-key prog-mode-map (kbd "C-c f f") 'flycheck-mode)
(define-key prog-mode-map (kbd "C-.") 'company-complete)
(define-key prog-mode-map (kbd "M-/") 'company-files)

(define-key global-map (kbd "C-,") 'compile)
(define-key global-map (kbd "C->") 'project-compile)

(provide 'proglang)
;;; proglang.el ends here
