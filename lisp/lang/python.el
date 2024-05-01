;; Python
(declare-function vc-git-root "vc-git.el")
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

(with-eval-after-load 'pyvenv
  (require 'pyvenv-auto)
  (add-hook 'python-mode-hook 'pyvenv-auto-run))
