;;; agenda.el --- Provide agenda functions
;;; Commentary:
;;; Code:

(require 'utils)

(defun aieis/ensure-org-buffer ()
  "Ensure there is a frame for the agenda buffer."
  (aieis/ensure-visible-frame-pattern "\\*Org Agenda\\*" "*Org Agenda*"))

(defun aieis/org-agenda ()
  "Show org agenda in a dedicated frame."
  (interactive)
  (progn
    (aieis/ensure-org-buffer)
    (org-agenda)))

(defun aieis/org-agenda-list ()
  "Show org agneda list in a dedicated frame."
  (interactive)
  (progn
    (aieis/ensure-org-buffer)
    (org-agenda-list)))

(provide 'agenda)
;;; agenda.el ends here
