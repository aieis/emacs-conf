;;; aieis-lang-glsl.el -- js setup
;;; Commentary:
;;; Code:

(require 'aieis-package)

(defun aieis/vk-compile ()
  (interactive)
  (let* ((file (buffer-file-name))
	 (cmd (concat "glslc " file " -o " file ".spv")))
    (compile cmd)))

(defun aieis/filter-list (condition ls f-acc a)
  (if (eq ls '()) a
    (let ((e (car ls))
	  (nls (cdr ls)))
      (aieis/filter-list condition nls f-acc (if (funcall condition e) (funcall f-acc e a) a)))))

(defun aieis/vk-find-glsl (target-dir)
  (aieis/filter-list
   (lambda (e)
     (or (string-equal (file-name-extension e) "vert")
	 (string-equal (file-name-extension e) "frag")))
   (directory-files target-dir 1)
   (lambda (e a) (concat a " " e))
   '()))

(defun aieis/vk-compile-dir ()
  (interactive)
  (let*(
	(file (buffer-file-name))
	(dir-name (file-name-directory file))
	(target-dir (if (string-equal major-mode "dired-mode") dired-directory
		      (substring dir-name 0 (- (length dir-name) 1))))
	(files (aieis/vk-find-glsl target-dir))
	(cmd (concat "glslc -c " files)))
    (compile cmd)))

(aieis/use-package glsl-mode :defer t
  :config
  (define-key glsl-mode-map (kbd "C-c d") 'aieis/vk-compile-dir)
  (define-key glsl-mode-map (kbd "C-c s") 'aieis/vk-compile))

(provide 'aieis-lang-glsl)
;;; aieis-lang-glsl.el ends here
