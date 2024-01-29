<<<<<<< HEAD
;;; local.el ---  Local variables and configuration (os / work / home )
;;; Commentary:

;;; Code:
;; Temporary Packages

(unless (load "local-restrict" :noerror)
  (write-region
   ";;; local-restrict.el ---  Local variables and configuration (os / work / home )
;;; Commentary:
;;; Keep local / temporary data in here

;;; Code:

;; Temporary Packages
(setq-default aieis/local-packages
              (list))

(provide 'local-restrict)
;;; local-restrict.el ends here"
   nil
   (concat (file-name-directory (or load-file-name buffer-file-name)) "tmp.el"))
   (load "local-restrict" :noerror))


(setq-default aieis/local-packages
              (list 'ahk-mode))

(provide 'local)
;;; local.el ends here


=======
;;; local.el ---  Local variables and configuration (os / work / home )
;;; Commentary:

;;; Code:
;; Temporary Packages

(unless (load "local-restrict" :noerror)
  (write-region
   ";;; local-restrict.el ---  Local variables and configuration (os / work / home )
;;; Commentary:
;;; Keep local / temporary data in here

;;; Code:

;; Temporary Packages
(setq-default aieis/local-packages
              (list))

(provide 'local-restrict)
;;; local-restrict.el ends here"
   nil
   (concat (file-name-directory (or load-file-name buffer-file-name)) "tmp.el"))
  (load "local-restrict" :noerror))

(setq-default aieis/local-packages
              (list 'ahk-mode))

(provide 'local)
;;; local.el ends here


>>>>>>> 62477724a4e48bc5bf15f072b36185caaec9d591
