<<<<<<< HEAD
;;; sys-vars.el --- system specific variables
;;; Commentary:
;;; Code:

(defun aieis/windows--set-vars ()
  "Set the default variables for the Windows XX OS."
  (setq-default aieis/ue-root-path "c:/local/ue")
  (setq-default aieis/llvm-root-path "c:/local/llvm"))

(defun aieis/linux--set-vars ()
  "Set the default variables for the linux OS."
  (setq-default aieis/ue-root-path "/opt/unreal-engine"))



(cond ((string-equal system-type "windows-nt")
      (aieis/windows--set-vars))
     ((string-equal system-type "gnu/linux")
      (aieis/linux--set-vars)))


(provide 'sys-vars.el)
;;; sys-vars.el ends here
=======
;;; sys-vars.el --- system specific variables
;;; Commentary:
;;; Code:

(defun aieis/windows--set-vars ()
  "Set the default variables for the Windows XX OS."
  (setq-default aieis/ue-setup t)
  (setq-default aieis/ue-root-path "c:/local/ue")
  (setq-default aieis/llvm-root-path "c:/local/llvm"))

(defun aieis/linux--set-vars ()
  "Set the default variables for the linux OS."
  (setq-default aieis/ue-setup 'nil)
  (setq-default aieis/ue-root-path "/opt/unreal-engine"))



(cond ((string-equal system-type "windows-nt")
      (aieis/windows--set-vars))
     ((string-equal system-type "gnu/linux")
      (aieis/linux--set-vars)))


(provide 'sys-vars.el)
;;; sys-vars.el ends here
>>>>>>> 62477724a4e48bc5bf15f072b36185caaec9d591
