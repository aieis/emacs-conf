<<<<<<< HEAD
;;; prog-ue.el --- handling unreal engine
;;; Commentary:
;;; Code:

(require 'sys-vars.el)
(require 'dash)

(setq-default aieis/llvm-versions
              (--filter (eq (or (string-equal "." it) (string-equal ".." it)) nil)
                        (directory-files aieis/llvm-root-path)))


(defun aieis/llvm-activate (tc)
  "Activate a toolchain TC from discovered toolchains."
  (interactive (list (completing-read "Select toolchain: " aieis/llvm-versions)))
  (let ((tcp (concat aieis/llvm-root-path "/" tc "/bin"))
        (nexec-path (--filter (eq (string-match aieis/llvm-root-path it) nil) exec-path)))
    (add-to-list 'nexec-path tcp)
    (setq exec-path nexec-path)))


(provide 'prog-ue.el)
;;; prog-ue.el ends here
=======
;;; prog-ue.el --- handling unreal engine
;;; Commentary:
;;; Code:



(require 'sys-vars.el)
(require 'dash)


(unless (eq aieis/ue-setup nil)
  (setq-default aieis/llvm-versions
                (--filter (eq (or (string-equal "." it) (string-equal ".." it)) nil)
                          (directory-files aieis/llvm-root-path)))

  (defun aieis/llvm-activate (tc)
    "Activate a toolchain TC from discovered toolchains."
    (interactive (list (completing-read "Select toolchain: " aieis/llvm-versions)))
    (let ((tcp (concat aieis/llvm-root-path "/" tc "/bin"))
          (nexec-path (--filter (eq (string-match aieis/llvm-root-path it) nil) exec-path)))
      (add-to-list 'nexec-path tcp)
      (setq exec-path nexec-path))))


(provide 'prog-ue.el)
;;; prog-ue.el ends here
>>>>>>> 62477724a4e48bc5bf15f072b36185caaec9d591
