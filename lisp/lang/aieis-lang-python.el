;;; aieis-lang-python.el -- python setup
;;; Commentary:
;;; Code:
(require 'vc-git)

(defun aieis/pyrightconfig-write ()
  "Setup the python virtual environment."
  (interactive)
  (let* ((input (read-string "venv:"))
         (venv-dir (file-truename input))
         (venv-file-name (directory-file-name venv-dir))
         (venvPath (file-name-directory venv-file-name))
         (venv (file-name-base venv-file-name))
         (base-dir (vc-git-root default-directory))
         (out-file (expand-file-name "pyrightconfig.json" base-dir))
         (out-contents
          (json-encode
           (list
            :venvPath venvPath
            :venv venv ))))
    (with-temp-file out-file (insert out-contents))))

(provide 'aieis-lang-python)
;;; aieis-lang-python.el ends here
