;;; filemanager.el -- setup the dired filemanager
;;; Commentary:
;;; Code:
(require 'dired )
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
(setq dired-dwim-target t)

(require 'ls-lisp)
(setq ls-lisp-dirs-first t)
(setq ls-lisp-use-insert-directory-program nil)

(defun aieis/dired-open-new ()
  "Open a new frame with a Dired buffer."
  (interactive)
  (let ((target-dir dired-directory))
    (select-frame-set-input-focus (make-frame))
    (find-file target-dir)))


(defun aieis/dired-open-new-file()
  "Open a new frame with targeted file."
  (interactive)
  (let ((target-file (dired-get-file-for-visit)))
    (select-frame-set-input-focus (make-frame))
    (find-file target-file)))


(defun aieis--/add-jump-directory (kb target-dir)
  "Add a shortcut, KB, to jump to a target directory TARGET-DIR in a Dired buffer."
  (define-key dired-mode-map (kbd (concat "SPC g " kb)) `(lambda () (interactive) (find-file ,target-dir))))

(define-key dired-mode-map (kbd "SPC") 'nil)

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


(mapc #'(lambda (vls) (aieis--/add-jump-directory (car vls) (car (cdr vls)))) aieis--dirs)

(define-key dired-mode-map (kbd "SPC n r") #'aieis/dired-open-new)
(define-key dired-mode-map (kbd "SPC n f") #'aieis/dired-open-new-file)

(require 'utils)
(define-key dired-mode-map (kbd "SPC q") #'aieis/force-delete-window)

(require 'aieis-image-dired)
(define-key dired-mode-map (kbd "SPC p") `(lambda () (interactive) (image-dired (dired-get-file-for-visit))))

(provide 'filemanager)
;;; filemanager.el ends here
