;;; async-shell.el --- async shell
;;; Commentary:

;;; Code:

(require 'comint)

(custom-set-variables
 '(async-shell-command-buffer 'rename-buffer))

(defun aieis/revert-shell-command ()
  "Terminate current process and restart it."
  (interactive)
  (while (get-buffer-process (current-buffer))
    (comint-interrupt-subjob)
    (sleep-for 0.01))
  (revert-buffer))

(define-key comint-mode-map (kbd "<f5>") 'aieis/revert-shell-command)


(provide 'async-shell)
;;; async-shell.el ends here

