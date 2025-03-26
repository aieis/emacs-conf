;;; aieis-lang-json.el -- json setup
;;; Commentary:
;;; Code:


(require 'json-mode)
(define-key json-mode-map (kbd "C-c C-f") 'json-pretty-print-buffer)

(defun aieis--json-win-mode-hook ()
  "NUL files get created on windows without this."
  (require 'dos-w32)
  (setq-local null-device (concat user-emacs-directory "null-device.txt")))

(if (string-equal system-type "windows-nt")
    (add-hook 'json-mode-hook 'aieis--json-win-mode-hook))

(provide 'aieis-lang-json)
;;; aieis-lang-json.el ends here
