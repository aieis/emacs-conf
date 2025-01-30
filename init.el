;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(put 'upcase-region 'disabled nil)

(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(async-shell-command-buffer 'rename-buffer)
 '(blink-cursor-mode 0)
 '(c-basic-offset 4)
 '(column-number-mode t)
 '(custom-enabled-themes '(vscode-dark-plus))
 '(custom-safe-themes
   '("39dd7106e6387e0c45dfce8ed44351078f6acd29a345d8b22e7b8e54ac25bac4" "d35afe834d1f808c2d5dc7137427832ccf99ad2d3d65d65f35cc5688404fdf30" "694dbeb8f98dddfb603a2fe0c04101f3fe457ee49bf90a6a581271e7f9c580c8" "841b6a0350ae5029d6410d27cc036b9f35d3bf657de1c08af0b7cbe3974d19ac" default))
 '(delete-old-versions t)
 '(display-buffer-base-action '(display-buffer-reuse-window (reusable-frames . 1)))
 '(electric-pair-mode 1)
 '(gc-cons-threshold (* 100 1024 1024))
 '(global-auto-revert-mode t)
 '(global-flycheck-mode 1)
 '(global-hl-line-mode t)
 '(indent-tabs-mode nil)
 '(js-indent-level 4)
 '(lsp-auto-configure t)
 '(lsp-auto-guess-root t)
 '(lsp-before-save-edits t)
 '(lsp-completion-enable t)
 '(lsp-completion-show-detail t)
 '(lsp-completion-show-kind t)
 '(lsp-eldoc-enable-hover t)
 '(lsp-eldoc-render-all nil)
 '(lsp-enable-dap-auto-configure nil)
 '(lsp-enable-file-watchers t)
 '(lsp-enable-folding t)
 '(lsp-enable-imenu t)
 '(lsp-enable-indentation t)
 '(lsp-enable-links t)
 '(lsp-enable-on-type-formatting nil)
 '(lsp-enable-snippet nil)
 '(lsp-enable-symbol-highlighting nil)
 '(lsp-enable-text-document-color nil)
 '(lsp-enable-xref t)
 '(lsp-file-watch-threshold 100)
 '(lsp-flycheck-live-reporting nil)
 '(lsp-idle-delay 0.5)
 '(lsp-imenu-show-container-name t)
 '(lsp-imenu-sort-methods '(position kind name))
 '(lsp-keymap-prefix "C-c l")
 '(lsp-pyls-plugins-flake8-enabled t)
 '(lsp-pyright-langserver-command "basedpyright")
 '(lsp-pyright-multi-root nil)
 '(lsp-signature-auto-activate t)
 '(lsp-signature-doc-lines 10)
 '(lsp-signature-render-documentation t)
 '(lsp-ui-doc-alignment 'frame)
 '(lsp-ui-doc-delay 5)
 '(lsp-ui-doc-header nil)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-position 'top)
 '(lsp-ui-doc-use-childframe t)
 '(lsp-ui-sideline-delay 0.5)
 '(lsp-ui-sideline-enable nil)
 '(lsp-ui-sideline-ignore-duplicates t)
 '(lsp-ui-sideline-show-hover t)
 '(menu-bar-mode nil)
 '(native-comp-async-report-warnings-errors 'silent)
 '(org-hide-leading-stars t)
 '(org-return-follows-link t)
 '(org-startup-indented t)
 '(package-selected-packages
   '(wgsl-mode monochrome-theme vscode-dark-plus-theme company-glsl tree-sitter powershell yasnippet string-inflection glsl-mode ef-themes all-the-icons lsp-mode pyvenv haskell-mode embark consult vertico transpose-frame rust-mode pyvenv-auto org-roam orderless moe-theme modus-themes marginalia magit lsp-ui lsp-pyright lsp-haskell flycheck embark-consult dired-hide-dotfiles company all-the-icons-dired ahk-mode ace-window))
 '(read-process-output-max (* 1024 1024) t)
 '(recentf-mode 1)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(warning-suppress-log-types
   '((use-package)
     (defvaralias losing-value woman-topic-history))))
(put 'downcase-region 'disabled nil)

(require 'org)
(org-babel-load-file (expand-file-name "myinit.org" user-emacs-directory))

