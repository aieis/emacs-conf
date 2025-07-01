(defun aieis/org+-copy-code ()
  "Export code silently to kill-ring."
  (interactive)
  (require 'ox-ascii)
  (cl-letf (((symbol-function 'org-element-normalize-string) #'identity))
    (save-excursion
      (kill-new
       (org-export-as 'ascii
                      nil nil t)))))

(defun aieis/org-export-helper (cid cls desc buf)
  (if (member cid cls) cls
    (progn
      (setq ncls (cons cid cls))
      (org-id-goto cid)
      (message "Exporting file %s" cid)
      (aieis/org+-copy-code)
      (switch-to-buffer buf)
      (insert (concat "\n* " desc "\n"))
      (yank)
      (org-id-goto cid)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (ln)
          (when (nth 2 ln)
            (let ((type (org-element-property :type ln))
                  (path (org-element-property :path ln)))

              (message "\n\n%s\n" (nth 2 ln))
              (setq desc (substring-no-properties (nth 2 ln)))

              (when (string= type "id")
                (setq ncls (aieis/org-export-helper path ncls desc buf)))))))
      ncls)))



(defun aieis/org-export ()
  (interactive)
  (aieis/org-export-helper (org-id-get) '() "Org Export" (generate-new-buffer "*Custom Org Export*")))
