;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-
;;; Code:
(cd "~/.emacs.d/")
(require 'org)
(org-babel-load-file
 (expand-file-name "myinit.org"
                   user-emacs-directory))
(cd "~")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tango))
 '(package-selected-packages
   '(ivy posix-manual helm rainbow-mode magit use-package orderless marginalia embark consult ace-window))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
