(require 'dired )
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
(setq dired-dwim-target t)

(require 'ls-lisp)
(setq ls-lisp-dirs-first t)
(setq ls-lisp-use-insert-directory-program nil)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(define-key dired-mode-map "." #'dired-hide-dotfiles-mode)

(defun aieis/dired-open-new ()
  (interactive)
  (let ((target-dir dired-directory))
    (select-frame-set-input-focus (make-frame))
    (find-file target-dir)))


(defun aieis/dired-open-new-file()
  (interactive)
  (let ((target-file (dired-get-file-for-visit)))
    (select-frame-set-input-focus (make-frame))
    (find-file target-file)))


(defun aieis--/add-jump-directory (kb target-dir)
  (define-key dired-mode-map (kbd (concat "SPC g " kb)) `(lambda () (interactive) (find-file ,target-dir))))


(unbind-key "SPC" dired-mode-map)


(setq-local
 aieis--dirs
 (cond
  ((string-equal system-type "windows-nt")
         '(("r" "C:/")
           ("p r" "c:/proj")
           ("p m" "c:/proj/misc")
           ("w r" "c:/wkspaces/")
           ("w n" "c:/wkspaces/PY_NCLLauncher/")))
        ((string-equal system-type "gnu/linux")
         '(("r" "/")
           ("p r" "~/projects")
           ("p m" "~/projects/misc")
           ("w r" "~/WorkSpace/")
           ("w n" "~/WorkSpace/PY_NCLLauncher/")))))


(mapcar #'(lambda (vls) (aieis--/add-jump-directory (car vls) (car (cdr vls)))) aieis--dirs)

(define-key dired-mode-map (kbd "SPC n r") #'aieis/dired-open-new)
(define-key dired-mode-map (kbd "SPC n f") #'aieis/dired-open-new-file)
(define-key dired-mode-map (kbd "SPC q") #'aieis/force-delete-window)
