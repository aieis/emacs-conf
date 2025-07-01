(setq mu4e-local-dir "C:/msys64/usr/local/share/emacs/site-lisp/mu4e")
(when (file-directory-p mu4e-local-dir)
  (add-to-list 'load-path mu4e-local-dir)
  (load "mailvars")
  (require 'mu4e)

  ;; use mu4e for e-mail in emacs
  (setq mail-user-agent 'mu4e-user-agent)

  (setq mu4e-drafts-folder "/[Gmail].Drafts")
  (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
  (setq mu4e-trash-folder  "/[Gmail].Trash")

  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
  ;; additional non-Gmail addresses and want assign them different
  ;; behavior.)

  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.

  (setq mu4e-maildir-shortcuts
        '( (:maildir (concat aieis/maildir "/INBOX")              :key ?i)
           (:maildir (concat aieis/maildir "/[Gmail].Sent Mail")  :key ?s)
           (:maildir (concat aieis/maildir "/[Gmail].Trash")      :key ?t)
           (:maildir (concat aieis/maildir "/[Gmail].All Mail")   :key ?a)))

  (add-to-list 'mu4e-bookmarks
               ;; ':favorite t' i.e, use this one for the modeline
               '(:query (concat "maildir:" aieis/maildir "/INBOX") :name "Inbox" :key ?i :favorite t))

  ;; allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "wsl offlineimap")

  ;; something about ourselves
  (setq
   user-mail-address aieis/email
   user-full-name  aieis/name
   mu4e-compose-signature
   (concat
    aieis/name
    "Developer\n"))

  (require 'smtpmail)
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials
        '(("smtp.gmail.com" 587 aieis/email nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)

  (setq message-kill-buffer-on-exit t))
