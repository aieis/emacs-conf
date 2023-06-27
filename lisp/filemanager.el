(require 'dired )
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
(setq dired-dwim-target t)

(require 'ls-lisp)
(setq ls-lisp-dirs-first t)
(setq ls-lisp-use-insert-directory-program nil)

(define-key dired-mode-map "." #'dired-hide-dotfiles-mode)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
