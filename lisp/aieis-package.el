;;; aieis-package.el  --- Handle packages
;;; Commentary:
;;; This packages will handle the packages in a way that does not hinder a new system

;;; Code:
(defvar aieis-packages '())
(defvar aieis-local-packages '())

(defmacro aieis/use-package (package &rest body)
  "A macro to redirect data to `use-package'.
PACKAGE is the package name and BODY is the rest."

  (declare (indent defun))
  `(progn
     (add-to-list 'aieis-packages ',package)
     (when (require 'use-package nil 'noerror)
       (use-package ,package ,@body))))

(defun aieis/install-packages (packages)
  "Install all the packages in the list PACKAGES."
  (dolist (package packages)
    (package-install package)))


(defun aieis/install-all-packages ()
  "Install all the packages in the list aieis-packages and aieis-local-packages."
  (interactive)
  (dolist (package (concat aieis-packages aieis-local-packages))
    (package-install package)))

(provide 'aieis-package)
;;; aieis-package.el ends here
