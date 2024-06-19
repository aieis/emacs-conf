;;; aieis-image-dired.el -- config for image-dired-mode
;;; Commentary:
;;; Code:

(require 'image-dired)

(defun aieis/image-dired-forward-image ()
  "Move to the next image and display."
  (interactive)
  (image-dired-forward-image)
  (image-dired-display-this))

(defun aieis/image-dired-backward-image ()
  "Move to the previous image and display."
  (interactive)
  (image-dired-backward-image)
  (image-dired-display-this))

(define-key image-dired-thumbnail-mode-map (kbd "<right>") #'aieis/image-dired-forward-image)
(define-key image-dired-thumbnail-mode-map (kbd "<left>") #'aieis/image-dired-backward-image)

(provide 'aieis-image-dired)
;;; aieis-image-dired.el ends here
