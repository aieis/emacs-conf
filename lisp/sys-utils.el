(defun aieis/open-terminal (&optional file)
  "Open a terminal window in the current file's directory. "
  (interactive)
  (let ((target-dir
         (cond
          ((string-equal major-mode "dired-mode") dired-directory)
          (file (file-name-directory file))
          (buffer-file-name (file-name-directory buffer-file-name))
          (t "~"))))

    (cond
     ((string-equal system-type "windows-nt")
      (w32-shell-execute "powershell" target-dir))
     ((string-equal system-type "gnu/linux")
      (start-process "" nil "sakura" target-dir)))))

                                        ; Keybindings ;
(define-key global-map (kbd "M-RET") 'aieis/open-terminal)
