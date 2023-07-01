(defun aieis/ensure-org-buffer ()
  (aieis/ensure-visible-frame-pattern "\\*Org Agenda\\*" "*Org Agenda*"))

(defun aieis/org-agenda ()
  (interactive)
  (progn 
    (aieis/ensure-org-buffer)
    (org-agenda)))

(defun aieis/org-agenda-list ()
  (interactive)
  (progn
    (aieis/ensure-org-buffer)
    (org-agenda-list)))
