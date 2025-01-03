;;; proglang.el --- package providing code-specific configuration
;;; Commentary:
;;; Code:

(custom-set-variables
 '(js-indent-level 4)
 '(c-basic-offset 4))
(c-set-offset 'substatement-open 0)

(require 'aieis-lang-common)
(require 'aieis-lang-python)
(require 'aieis-lang-js)
(require 'aieis-lang-odin)
(require 'aieis-lang-json)

(add-to-list 'aieis/lsp-mode-hooks 'c-mode-common-hook)
(add-to-list 'aieis/lsp-mode-hooks 'rust-mode-hook)
(add-to-list 'aieis/lsp-mode-hooks 'python-mode-hook)
(add-to-list 'aieis/lsp-mode-hooks 'haskell-mode-hook)
(add-to-list 'aieis/lsp-mode-hooks 'odin-mode-hook)

;; FlyCheck
(declare-function aieis/frame-visible? "utils.el")
(defun aieis/flycheck-show-buffer-diagnostics-hook ()
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

(require 'company)
(global-company-mode)
(setq company-selection-wrap-around t)
(setq company-idle-delay nil)


;; LSP Mode

(custom-set-variables
 '(lsp-keymap-prefix "C-c l")
 '(lsp-pyright-multi-root nil))

(require 'lsp)
(require 'lsp-pyright)
(require 'lsp-haskell)

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

(lsp-enable-imenu)
(custom-set-variables
 '(lsp-auto-configure t)
 '(lsp-enable-dap-auto-configure nil)
 '(lsp-auto-guess-root t)
 '(lsp-before-save-edits t)
 '(lsp-eldoc-enable-hover t)
 '(lsp-eldoc-render-all nil)
 '(lsp-completion-enable t)
 '(lsp-completion-show-detail t)
 '(lsp-completion-show-kind t)
 '(lsp-enable-file-watchers t)
 '(lsp-file-watch-threshold 100)
 '(lsp-enable-folding t)
 '(lsp-enable-imenu t)
 '(lsp-enable-indentation t)
 '(lsp-enable-links t)
 '(lsp-enable-on-type-formatting nil)
 '(lsp-enable-snippet nil)
 '(lsp-enable-symbol-highlighting nil)
 '(lsp-enable-text-document-color nil)
 '(lsp-enable-xref t)
 '(lsp-flycheck-live-reporting nil)
 '(lsp-idle-delay 0.5)
 '(lsp-imenu-show-container-name t)
 '(lsp-imenu-sort-methods '(position kind name))
 '(lsp-pyls-plugins-flake8-enabled t)
 '(lsp-signature-auto-activate t)
 '(lsp-signature-render-documentation t)
 '(lsp-signature-doc-lines 10)
 '(lsp-ui-sideline-enable nil)
 '(lsp-ui-sideline-show-hover nil)
 '(lsp-ui-sideline-delay 0.5)
 '(lsp-ui-doc-delay 5)
 '(lsp-ui-sideline-ignore-duplicates t)
 '(lsp-ui-doc-position 'top)
 '(lsp-ui-doc-alignment 'frame)
 '(lsp-ui-doc-header nil)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-use-childframe t))


;; Keymap Keys
(define-key prog-mode-map (kbd "C-c f d") 'flycheck-list-errors)
(define-key prog-mode-map (kbd "C-c f n") 'flycheck-next-error)
(define-key prog-mode-map (kbd "C-c f f") 'flycheck-mode)
(define-key prog-mode-map (kbd "C-.") 'company-complete)

(define-key global-map (kbd "C-,") 'compile)
(define-key global-map (kbd "C->") 'project-compile)
(define-key global-map (kbd "C-@") 'delete-trailing-whitespace)

(provide 'proglang)
;;; proglang.el ends here
