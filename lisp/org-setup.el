;;; org-setup.el --- Setup org environment
;;; Commentary:
;;; Code:


(setq aieis-org-notes-dir
      (let ((notes-dir (file-truename "~/notes"))
            (backup-notes-dir (concat user-emacs-directory "/notes")))
        (cond ((file-exists-p notes-dir) notes-dir)
              ((file-exists-p backup-notes-dir) backup-notes-dir)
              (t (progn (make-directory backup-notes-dir) backup-notes-dir)))))

(defun aieis--notes-file (filename)
  "Returns the fullpath of a file in the notes directory given a `FILENAME'."
  (concat aieis-org-notes-dir "/" filename))

(with-eval-after-load 'org
  (setq org-return-follows-link t)
  (let ((cmd-pairs
         '(("M-N" org-move-subtree-down)
           ("M-P" org-move-subtree-up))))

    (mapc #'(lambda (pair) (define-key org-mode-map (kbd (car pair)) (cadr pair))) cmd-pairs))

  (let ((tasks-file (aieis--notes-file "tasks.org"))
        (art-file (aieis--notes-file "art.org"))
        (snippets-file (aieis--notes-file "snippets.org")))

    (setq org-capture-templates
          (list
           (list "b" "Template for adding a task" 'entry
                 (list 'file+headline tasks-file "Refile")
                 "* TODO %^t %? %^G")
           (list "a" "Template for adding an artwork task" 'entry
                 (list 'file+headline art-file "Art")
                 "* TODO %^t %?  %^G")
           (list "c" "Add a code snippet" 'entry
                 (list 'file+headline snippets-file "Snippet")
                 "* %t %?")))

    (setq org-agenda-files (list tasks-file art-file snippets-file))))

(if (file-exists-p aieis-org-notes-dir)
    (setq-default org-roam-directory aieis-org-notes-dir)
  (message "Notes directory '%s' does not exist" aieis-org-notes-dir))

(aieis/use-package org-roam :defer t)
(aieis/use-package org-capture)

(with-eval-after-load 'org-roam
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(define-key global-map (kbd "C-c a") 'aieis/org-agenda)
(define-key global-map (kbd "C-c c") (lambda () (interactive) (require 'org-capture) (call-interactively 'org-capture)))
(define-key global-map (kbd "C-c n l") (lambda () (interactive) (require 'org-roam) (call-interactively 'org-roam-buffer-toggle)))
(define-key global-map (kbd "C-c n f") (lambda () (interactive) (require 'org-roam) (call-interactively 'org-roam-node-find)))
(define-key global-map (kbd "C-c n g") (lambda () (interactive) (require 'org-roam) (call-interactively 'org-roam-graph)))
(define-key global-map (kbd "C-c n i") (lambda () (interactive) (require 'org-roam) (call-interactively 'org-roam-node-insert)))
(define-key global-map (kbd "C-c n c") (lambda () (interactive) (require 'org-roam) (call-interactively 'org-roam-capture)))
(define-key global-map (kbd "C-c n j") (lambda () (interactive) (require 'org-roam) (call-interactively 'org-roam-dailies-capture-today)))

(provide 'org-setup)
;;; org-setup.el ends here
