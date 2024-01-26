;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org)
(org-babel-load-file
 (expand-file-name "myinit.org"
                   user-emacs-directory))

(put 'upcase-region 'disabled nil)

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
