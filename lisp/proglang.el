;;; proglang.el --- package providing code-specific configuration
;;; Commentary:
;;; Code:

(custom-set-variables
 '(js-indent-level 4)
 '(c-basic-offset 4))
(c-set-offset 'substatement-open 0)

(custom-set-variables
 '(lsp-keymap-prefix "C-c l")
 '(lsp-pyright-multi-root nil))


(require 'aieis-lang-common)
(require 'aieis-lang-python)
(require 'aieis-lang-js)
(require 'aieis-lang-odin)
(require 'aieis-lang-json)

(require 'aieis-package)

(aieis/use-package haskell-mode :defer t)
(aieis/use-package lsp-haskell :defer t)
(aieis/use-package rust-mode :defer t)
(aieis/use-package pyvenv :defer t)
(aieis/use-package pyvenv-auto :defer t)
(aieis/use-package lsp-ui :defer t)

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
  :hook (prog-mode . company-mode))

(aieis/use-package lsp :defer t)

;; LSP Mode
(with-eval-after-load 'lsp
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

  ;; Python specific
  (setq lsp-ruff-ruff-args "--preview")

  (lsp-enable-imenu)
  (setq lsp-auto-configure t)
  (setq lsp-enable-dap-auto-configure nil)
  (setq lsp-auto-guess-root t)
  (setq lsp-before-save-edits t)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-eldoc-render-all nil)
  (setq lsp-completion-enable t)
  (setq lsp-completion-show-detail t)
  (setq lsp-completion-show-kind t)
  (setq lsp-enable-file-watchers t)
  (setq lsp-file-watch-threshold 100)
  (setq lsp-enable-folding t)
  (setq lsp-enable-imenu t)
  (setq lsp-enable-indentation t)
  (setq lsp-enable-links t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-text-document-color nil)
  (setq lsp-enable-xref t)
  (setq lsp-flycheck-live-reporting nil)
  (setq lsp-idle-delay 0.5)
  (setq lsp-imenu-show-container-name t)
  (setq lsp-imenu-sort-methods '(position kind name))
  (setq lsp-pyls-plugins-flake8-enabled t)
  (setq lsp-signature-auto-activate t)
  (setq lsp-signature-render-documentation t)
  (setq lsp-signature-doc-lines 10)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-delay 0.5)
  (setq lsp-ui-doc-delay 5)
  (setq lsp-ui-sideline-ignore-duplicates t)
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-alignment 'frame)
  (setq lsp-ui-doc-header nil)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-use-childframe t))

;; Keymap Keys
(define-key prog-mode-map (kbd "C-c f d") 'flycheck-list-errors)
(define-key prog-mode-map (kbd "C-c f n") 'flycheck-next-error)
(define-key prog-mode-map (kbd "C-c f f") 'flycheck-mode)
(define-key prog-mode-map (kbd "C-.") 'company-complete)
(define-key prog-mode-map (kbd "M-/") 'company-files)

(provide 'proglang)
;;; proglang.el ends here
