(require 'dired )
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
(setq dired-dwim-target t)

(require 'ls-lisp)
(setq ls-lisp-dirs-first t)
(setq ls-lisp-use-insert-directory-program nil)

(define-key dired-mode-map "." #'dired-hide-dotfiles-mode)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)


(defun aieis--/add-jump-directory (kb target-dir)
  (define-key dired-mode-map (kbd (concat "SPC g " kb)) `(lambda () (interactive) (find-file ,target-dir))))

(unbind-key "SPC" dired-mode-map)


(setq-local
 aieis--dirs
 (cond
  ((string-equal system-type "windows-nt")
         '(("r" "C:/")
           ("p r" "d:/projects")
           ("p m" "d:/projects/misc")
           ("w r" "d:/WorkSpace/")
           ("w n" "d:/WorkSpace/PY_NCLLauncher/")))
        ((string-equal system-type "gnu/linux")
         '(("r" "/")
           ("p r" "~/projects")
           ("p m" "~/projects/misc")
           ("w r" "~/WorkSpace/")
           ("w n" "~/WorkSpace/PY_NCLLauncher/")))))



(mapcar #'(lambda (vls) (aieis--/add-jump-directory (car vls) (car (cdr vls)))) aieis--dirs)
