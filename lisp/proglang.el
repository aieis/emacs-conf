(defun aieis/js-mode-hook ()
  (setq js-indent-level 2))

(defun aieis/c-common-hook ()
  (setq c-basic-offset 4))

;; Python
(defun aieis/pyrightconfig-write (virtualenv)
  (interactive "venv: ")
  (let* ((venv-dir (tramp-file-local-name (file-truename virtualenv)))
         (venv-file-name (directory-file-name venv-dir))
         (venvPath (file-name-directory venv-file-name))
         (venv (file-name-base venv-file-name))
         (base-dir (vc-git-root default-directory))
         (out-file (expand-file-name "pyrightconfig.json" base-dir))
         (out-contents (json-encode (list :venvPath venvPath :venv venv))))
    (with-temp-file out-file (insert out-contents))))

(defun aieis/flymake-show-buffer-diagnostics-hook (&optional arg)
  (if (and (derived-mode-p 'prog-mode) (bound-and-true-p flymake-mode) (aieis/frame-visible? "\\*Flymake diagnostics*"))
      (call-interactively 'flymake-show-buffer-diagnostics)))

;; File Association
(push '("\\.js[x]?\\'" . javascript-mode) auto-mode-alist)
(push '("\\.ts[x]?\\'" . javascript-mode) auto-mode-alist)


;; Hooks
(add-hook 'c-mode-common-hook 'aieis/c-common-hook)
;; (add-hook 'c-mode-common-hook 'eglot-ensure)
;; (add-hook 'haskell-mode-hook 'eglot-ensure)
;; (add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'js-mode-hook 'aieis/js-mode-hook)

(add-hook 'flymake-mode-hook 'aieis/flymake-show-buffer-diagnostics-hook)
(add-hook 'window-buffer-change-functions 'aieis/flymake-show-buffer-diagnostics-hook)
(add-hook 'window-selection-change-functions 'aieis/flymake-show-buffer-diagnostics-hook)
;; Keymap Keys
(define-key prog-mode-map (kbd "C-c f d") 'flymake-show-buffer-diagnostics)
(define-key prog-mode-map (kbd "C-c f n") 'flymake-goto-next-error)

;; Display
(let ((n-disp-list
       '("\\*Flymake diagnostics.*"
         (display-buffer-reuse-mode-window display-buffer-pop-up-frame)
         (reusable-frames . visible))))
  (add-to-list 'display-buffer-alist n-disp-list))


;; LSP Mode
(setq lsp-keymap-prefix "C-l")

(require 'lsp)
(require 'lsp-haskell)
(require 'lsp-pyright)

(add-hook 'c-mode-common-hook #'lsp-deferred)
(add-hook 'rust-mode-hook #'lsp-deferred)
(add-hook 'python-mode-hook #'lsp-deferred)
(add-hook 'haskell-mode-hook #'lsp-deferred)
