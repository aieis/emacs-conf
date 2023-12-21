;;; proglang.el --- package providing code-specific configuration
;;; Commentary:
;;; Code:

(custom-set-variables
 '(js-indent-level 2)
 '(c-basic-offset 4)
 '(global-flycheck-mode 1))

;; Python
(declare-function vc-git-root "vc-git.el")
(defun aieis/pyrightconfig-write ()
  "Setup the python virtual environment."
  (interactive)
  (let* ((input (read-string "venv:"))
         (venv-dir (file-truename input))
         (venv-file-name (directory-file-name venv-dir))
         (venvPath (file-name-directory venv-file-name))
         (venv (file-name-base venv-file-name))
         (base-dir (vc-git-root default-directory))
         (out-file (expand-file-name "pyrightconfig.json" base-dir))
         (out-contents
          (json-encode
           (list
            :venvPath venvPath
            :venv venv ))))
    (with-temp-file out-file (insert out-contents))))

(with-eval-after-load 'pyvenv
  (require 'pyvenv-auto)
  (add-hook 'python-mode-hook 'pyvenv-auto-run))

;; FlyCheck
(declare-function aieis/frame-visible? "utils.el")
(defun aieis/flycheck-show-buffer-diagnostics-hook ()
  "Bring up the fly* diagnostics menu."
  (interactive)
  (if (and (derived-mode-p 'prog-mode) (bound-and-true-p flycheck-mode) (aieis/frame-visible? "\\*Flycheck errors*"))
      (call-interactively 'flycheck-list-errors)))

;; File Association
(push '("\\.js[x]?\\'" . javascript-mode) auto-mode-alist)
(push '("\\.ts[x]?\\'" . javascript-mode) auto-mode-alist)


;; Hooks
(add-hook 'flycheck-mode-hook 'aieis/flycheck-show-buffer-diagnostics-hook)
(add-hook 'window-configuration-change-hook 'aieis/flycheck-show-buffer-diagnostics-hook)

;; Display
(let ((n-disp-list
       '("\\*Flycheck errors.*"
         (display-buffer-reuse-mode-window display-buffer-pop-up-frame)
         (reusable-frames . visible))))
  (add-to-list 'display-buffer-alist n-disp-list))

;; Flycheck
(with-eval-after-load 'flycheck
  (defvar flycheck-disabled-checkers)
  (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc))

;; LSP Mode

(custom-set-variables
 '(lsp-keymap-prefix "C-l")
 '(lsp-pyright-multi-root nil))

(with-eval-after-load 'lsp
  (declare-function lsp-deferred "../myinit.org")
  (declare-function lsp-enable-imenu "lsp-mode.el")
  (declare-function pyvenv-auto-run "pyvenv-auto.el")
  
  (require 'lsp-pyright)
  (require 'lsp-haskell)

  (add-hook 'c-mode-common-hook #'lsp-deferred)

  (add-hook 'rust-mode-hook #'lsp-deferred)

  (add-hook 'python-mode-hook (lambda () (pyvenv-auto-run) (lsp-deferred)))

  (add-hook 'haskell-mode-hook #'lsp-deferred)

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
   
   '(lsp-ui-sideline-show-hover t)
   '(lsp-ui-sideline-delay 0.5)
   '(lsp-ui-doc-delay 5)
   '(lsp-ui-sideline-ignore-duplicates t)
   '(lsp-ui-doc-position 'top)
   '(lsp-ui-doc-alignment 'frame)
   '(lsp-ui-doc-header nil)
   '(lsp-ui-doc-include-signature t)
   '(lsp-ui-doc-use-childframe t)))


;; Keymap Keys
(define-key prog-mode-map (kbd "C-c f d") 'flycheck-list-errors)
(define-key prog-mode-map (kbd "C-c f n") 'flycheck-next-error)

(provide 'porglang)
;;; proglang.el ends here
