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
 '(custom-enabled-themes '(modus-operandi-tinted))
 '(custom-safe-themes
   '("d395c1793e0d64797d711c870571a0033174ca321ed48444efbe640bf692bf4f" "250007c5ae19bcbaa80e1bf8184720efb6262adafa9746868e6b9ecd9d5fbf84" "896e4305e7c10f3217c5c0a0ef9d99240c3342414ec5bfca4ec4bff27abe2d2d" "bfc0b9c3de0382e452a878a1fb4726e1302bf9da20e69d6ec1cd1d5d82f61e3d" "35335d6369e911dac9d3ec12501fd64bc4458376b64f0047169d33f9d2988201" "df1cbfd16a8af6e821c3299d92c84a0601e961f1be6efd761d6dd40621fde9eb" "3c451787cee570710bff441154a7db8b644cdbb7d270189b2724c6041a262381" "d537a9d42c6f5349d1716ae9be9a0645cc168f7aff2a8353819d570e5d02c0b3" default))
 '(org-agenda-files
   '("~/notes/snippets.org" "/home/ai/notes/art.org" "/home/ai/org/c:/Users/Ahmed/Home/notes/tasks.org"))
 '(warning-suppress-types '((emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
