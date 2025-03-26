;;; misc-keybinds.el ---  misc keybindings. could be global
;;; Commentary:
;;; Keybinds found below

;;; Code:

(define-key global-map (kbd "C-,") 'compile)
(define-key global-map (kbd "C-<") 'project-compile)
(define-key global-map (kbd "C-@") 'delete-trailing-whitespace)
(define-key global-map (kbd "M-`") 'other-frame)

(provide 'misc-keybinds)
;;; misc-keybinds.el ends here
