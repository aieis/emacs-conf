;;; sys-utils.el --- System utilities based on OS
;;; Commentary:
;;; Code:

(defun aieis/open-terminal (&optional file)
  "Open a terminal window in the current FILE's directory."
  (interactive)
  (let ((target-dir
         (cond
          ((string-equal major-mode "dired-mode") dired-directory)
          (file (file-name-directory file))
          (buffer-file-name (file-name-directory buffer-file-name))
          (t "~"))))

    (cond
     ((string-equal system-type "windows-nt")
      (start-process "" nil "wezterm.exe" "start" "--cwd"  (expand-file-name target-dir) "pwsh.exe"))
     ((string-equal system-type "gnu/linux")
      (start-process "" nil "wezterm" "start" "--cwd" (expand-file-name target-dir))))))



(defun aieis/open-default (&optional file)
  "Open a FILE with the default application."
  (interactive)
  (let ((target-file
         (cond
          ((string-equal major-mode "dired-mode") dired-directory)
          (file file)
          (buffer-file-name buffer-file-name)
          (t nil))))

    (when target-file
      (cond
       ((string-equal system-type "windows-nt")
        (w32-shell-execute "open" target-file))
       ((string-equal system-type "gnu/linux")
        (start-process "" nil "xdg-open" target-file))))))

; Keybindings
(define-key global-map (kbd "M-S-<return>") 'aieis/open-terminal)
(provide 'sys-utils)
;;; sys-utils.el ends here
