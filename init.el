;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-
;;; Code:
(require 'org)
(org-babel-load-file
 (expand-file-name "myinit.org"
                   user-emacs-directory))
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
 '(custom-enabled-themes '(modus-vivendi-tinted))
 '(custom-safe-themes
   '("f82e68d489e6c21c9552c4e8e35a03d126d9eba632a8e7b4f9329d1374b4a19c" "eb7cd622a0916358a6ef6305e661c6abfad4decb4a7c12e73d6df871b8a195f8" "4d844092dbec5c6fa347ca02c988ed99378b9e05de86a4d437de6370c78633df" "a9c9f34794fa0b2da9e111a85402fb3f9b82e281398e0249bbc073210592e198" "e2337309361eef29e91656c5e511a6cb8c54ce26231e11424a8873ea88d9093e" "bc6a96def5282e9d8a07edc03e02697eae9ab2e21a90ed2f07038bbf6ed4145c" "75e027e3ab2892c5c1f152e3d9fae03718f75bee50d259040e56e7e7672a4872" "2da5f695ab1373fd25e06302a3055eac39c37e6962063eb87c2e0a6e2d6ef5c0" "1e2f562077d76160ad2a4b648bf754698ab8cdf3914db933badc356881dad147" "d395c1793e0d64797d711c870571a0033174ca321ed48444efbe640bf692bf4f" "250007c5ae19bcbaa80e1bf8184720efb6262adafa9746868e6b9ecd9d5fbf84" "896e4305e7c10f3217c5c0a0ef9d99240c3342414ec5bfca4ec4bff27abe2d2d" "bfc0b9c3de0382e452a878a1fb4726e1302bf9da20e69d6ec1cd1d5d82f61e3d" "35335d6369e911dac9d3ec12501fd64bc4458376b64f0047169d33f9d2988201" "df1cbfd16a8af6e821c3299d92c84a0601e961f1be6efd761d6dd40621fde9eb" "3c451787cee570710bff441154a7db8b644cdbb7d270189b2724c6041a262381" "d537a9d42c6f5349d1716ae9be9a0645cc168f7aff2a8353819d570e5d02c0b3" default))
 '(org-agenda-files
   '("~/notes/tags.org" "~/notes/art.org" "~/notes/tasks.org" "~/notes/snippets.org"))
 '(warning-suppress-types '((emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
