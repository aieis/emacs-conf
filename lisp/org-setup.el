;;; org-setup.el --- Setup org environment
;;; Commentary:
;;; Code:

(with-eval-after-load 'org
  (setq org-return-follows-link t)
  (let ((cmd-pairs
         '(("M-N" org-move-subtree-down)
           ("M-P" org-move-subtree-up))))

    (mapc #'(lambda (pair) (define-key org-mode-map (kbd (car pair)) (cadr pair))) cmd-pairs))

  (setq org-capture-templates
        `(("b"
           "Template for adding a task"
           entry
           (file+headline "~/notes/tasks.org" "Refile")
           "* TODO %^t %? %^G")
          ("a"
           "Template for adding a task"
           entry
           (file+headline "~/notes/art.org" "Art")
           "* TODO %^t %?  %^G")
          ("c" "Add a code snippet" entry
           (file+headline "~/notes/snippets.org" "Snippet")
           "* %t %?"))))

(let ((notes-dir (file-truename "~/notes")))
  (if (file-exists-p notes-dir)
      (setq-default org-roam-directory notes-dir)
    (message "Notes directory '%s' does not exist" notes-dir)))

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
