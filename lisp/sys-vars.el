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

(provide 'sys-vars)
;;; sys-vars.el ends here
