;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; (require 'org)
;;(org-babel-load-file (expand-file-name "myinit.org" user-emacs-directory))
(setq custom-file (concat user-emacs-directory "custom.el"))
(load-file (concat user-emacs-directory "aieis-init.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
