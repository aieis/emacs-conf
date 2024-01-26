(defun aieis/new-frame-with-name (fn)
  "Open a new frame with scratch buffer selected"
  (interactive)
  (let ((frame (make-frame))
        (scratch-name fn))
    (select-frame-set-input-focus frame)
    (unless (get-buffer scratch-name)
      (with-current-buffer (get-buffer-create scratch-name)
        (text-mode)))
    (switch-to-buffer scratch-name 'norecord)
    frame))


(defun aieis/list-match (fun ls)
  (cond ((null ls) 'nil)
        (t (or (funcall fun (car ls)) (aieis/list-match fun (cdr ls))))))


(defun aieis/frame-visible? (pattern)
  (let ((frames (visible-frame-list)))
    (aieis/list-match
     (lambda (frame)
       (let* ((res (string-match pattern (frame-parameter frame 'name)))
              (nres (and res (= res 0))))
         (if nres frame 'nil))) frames)))


(defun aieis/ensure-visible-frame-pattern (pattern fn)
  (let* ((frame (aieis/frame-visible? pattern)))
    (if frame frame
      (aieis/new-frame-with-name fn))))

(defun aieis/force-delete-window ()
  (interactive)
  (cond ((= 1 (length (window-list))) (delete-frame))
        (t (delete-window))))


(defun align-non-space (BEG END)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t))

(provide 'utils)
;;; utils.el ends here
