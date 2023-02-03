;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-
;;; Code:
(cd "~/.emacs.d/")
(require 'org)
(org-babel-load-file
 (expand-file-name "myinit.org"
                   user-emacs-directory))
(cd "~")
;; (add-to-list 'load-path "~/emacs_win/xelb")
;; (add-to-list 'load-path "~/emacs_win/exwm")

;; (require 'exwm)
;; (require 'exwm-config)
;; (exwm-config-example)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(moe-light))
 '(custom-safe-themes
   '("df1cbfd16a8af6e821c3299d92c84a0601e961f1be6efd761d6dd40621fde9eb" "3c451787cee570710bff441154a7db8b644cdbb7d270189b2724c6041a262381" "d537a9d42c6f5349d1716ae9be9a0645cc168f7aff2a8353819d570e5d02c0b3" default))
 '(warning-suppress-types '((emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
