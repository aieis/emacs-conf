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


(defun aieis/background-async-shell-command (command &optional output-buffer error-buffer)
    "Execute string COMMAND asynchronously in background.

Like `shell-command', but adds `&' at the end of COMMAND
to execute it asynchronously.

The output appears in OUTPUT-BUFFER, which could be a buffer or
the name of a buffer, and defaults to `shell-command-buffer-name-async'
if nil or omitted.  That buffer is in shell mode.  Note that, unlike
with `shell-command', OUTPUT-BUFFER can only be a buffer, a buffer's
name (a string), or nil.

You can customize `async-shell-command-buffer' to specify what to do
when the buffer specified by `shell-command-buffer-name-async' is
already taken by another running shell command.

To run COMMAND without displaying the output in a window you can
configure `display-buffer-alist' to use the action
`display-buffer-no-window' for the buffer given by
`shell-command-buffer-name-async'.

Optional argument ERROR-BUFFER is for backward compatibility; it
is ignored, and error output of the async command is always
mingled with its regular output.

In Elisp, you will often be better served by calling `start-process'
directly, since it offers more control and does not impose the use of
a shell (with its need to quote arguments)."
  (interactive
   (list
    (read-shell-command (if shell-command-prompt-show-cwd
                            (format-message "Async shell command in `%s': "
                                            (abbreviate-file-name
                                             default-directory))
                          "Async shell command: ")
                        nil nil
			(let ((filename
			       (cond
				(buffer-file-name)
				((eq major-mode 'dired-mode)
				 (dired-get-filename nil t)))))
			  (and filename (file-relative-name filename))))
    nil
    ;; FIXME: the following argument is always ignored by 'shell-command',
    ;; when the command is invoked asynchronously, except, perhaps, when
    ;; 'default-directory' is remote.
    shell-command-default-error-buffer))
  (unless (string-match "&[ \t]*\\'" command)
    (setq command (concat command " &")))
  (shell-command command "HIDDEN BUFFER" error-buffer))

(define-key global-map (kbd "C-#") 'aieis/background-async-shell-command)


(provide 'async-shell)
;;; async-shell.el ends here

