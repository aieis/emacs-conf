;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org)
(org-babel-load-file
 (expand-file-name "myinit.org"
                   user-emacs-directory))

(put 'upcase-region 'disabled nil)

(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
