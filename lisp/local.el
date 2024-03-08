;;; local.el ---  Local variables and configuration (os / work / home )
;;; Commentary:

;;; Code:
;; Temporary Packages



(let ((lr-file (concat (file-name-directory (or load-file-name buffer-file-name)) "local-restrict.el"))
      (lr-default-file (concat (file-name-directory (or load-file-name buffer-file-name)) "local-restrict-default.el")))
  (unless (file-exists-p lr-file)
    (copy-file
     lr-default-file
     lr-file)))


(load "local-restrict")
(provide 'local)
;;; local.el ends here
