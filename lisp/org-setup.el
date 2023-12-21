(with-eval-after-load 'org
  (define-key global-map (kbd "C-c a") 'aieis/org-agenda)
  (define-key global-map (kbd "C-c c") 'org-capture)
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

(setq-default org-roam-directory (file-truename "~/notes"))
(require 'org-roam)
(with-eval-after-load 'org-roam
  (define-key global-map (kbd "C-c n l") 'org-roam-buffer-toggle)
  (define-key global-map (kbd "C-c n f") 'org-roam-node-find)
  (define-key global-map (kbd "C-c n g") 'org-roam-graph)
  (define-key global-map (kbd "C-c n i") 'org-roam-node-insert)
  (define-key global-map (kbd "C-c n c") 'org-roam-capture)
  (define-key global-map (kbd "C-c n j") 'org-roam-dailies-capture-today)
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))
