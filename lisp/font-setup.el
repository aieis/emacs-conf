;;; font-setup.el --- font setup
;;; Commentary:
;;; Code:
(defun frame-font-setup
    (&rest ...)
  (unless (assoc 'font default-frame-alist)
    (let* ((font-a (catch 'break
                     (dolist (font-a aieis/font-list)
                       (when (member (car font-a) (font-family-list))
                         (throw 'break font-a)))))
           (font (when (car font-a) (format "%s-%d" (car font-a) (cdr font-a)))))
      (when font
        (add-to-list 'default-frame-alist (cons 'font font))
        (set-frame-font font t t)))))
(add-hook 'focus-in-hook #'frame-font-setup)

(provide 'font-setup)
;;; font-setup.el ends here
