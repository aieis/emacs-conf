;;; aieis-package.el  --- Handle packages
;;; Commentary:
;;; This packages will handle the packages in a way that does not hinder a new system

;;; Code:
(defvar aieis-packages '())
(defvar aieis-local-packages '())


;;; Setup package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(require 'use-package nil 'noerror)

(defmacro aieis/use-package (package &rest body)
  "A macro to redirect data to `use-package'.
PACKAGE is the package name and BODY is the rest."

  (declare (indent defun))
  `(progn
     (add-to-list 'aieis-packages ',package)
     (when (require 'use-package nil 'noerror)
       (use-package ,package ,@body))))

(defun aieis/package-exists-in-repository-p (package)
  "Check if PACKAGE exists in the package repositories.
  Returns package if it exists, nil otherwise."
  (unless package-archive-contents
    (package-refresh-contents))
  (assoc package package-archive-contents))


(defun aieis/install-packages (packages)
  "Install all the packages in the list PACKAGES."
  (dolist (package packages)
    (if (aieis/package-exists-in-repository-p package)
	(package-install package)
      (message "Package '%s' Does not exists in the repositories. Consider calling `package-refresh-contents'" package))))


(defun aieis/install-all-packages ()
  "Install all the packages in the list aieis-packages and aieis-local-packages."
  (interactive)
  (require 'recentf)
  (unless (package-installed-p 'use-package)
    (aieis/install-packages '('use-package)))
  (aieis/install-packages aieis-packages)
  (aieis/install-packages aieis-local-packages))

(provide 'aieis-package)
;;; aieis-package.el ends here
