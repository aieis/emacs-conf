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

(defun aieis/ensure-visible-frame-pattern (pattern fn)
  (let* ((frames (visible-frame-list))
        (frame (aieis/list-match (lambda (frame)
                                (let* ((res (string-match pattern (frame-parameter frame 'name)))
                                       (nres (and res (= res 0))))
                                  (if nres frame 'nil)))
                                 frames)))
    (if frame frame
      (aieis/new-frame-with-name fn))))
