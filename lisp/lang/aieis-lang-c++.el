;;; aieis-lang-c++ -- provide utils for handling c++
;;; Commentary:
;;; Code:

(defvar aieis-last-c++-name "")

(defun aieis-find-c++-class-name ()
  "Find the current C++ struct or class name."
  (interactive)
  (save-excursion
    (call-interactively 'move-end-of-line)
    (search-backward-regexp "^\\s-*\\(?:\\(?:abstract\\|final\\)\\s-+\\)?\\(class\\|struct\\)\\s-+\\(\\(?:\\sw\\|\\\\\\|\\s_\\)+\\)")
    (let ((ret (match-string 2)))
      (message ret)
      ret)))

(defun aieis-add-c++-class-name ()
  "Add Last saved C++ class used by other functions."
  (interactive)
  (insert aieis-last-c++-name))

(defalias 'aieis-move-c++-member-function
  (kmacro "C-SPC C-s { <return> C-b M-x f o r w a r d - s e x p <return> M-w C-x o M-> <return> <return> C-y M-x b a c k w a r d - s e x p <return> C-a C-s SPC M-x a i e is - a d d - c + + - c l a s s - n a m e <return> : : C-x h <tab> C-x o M-x C-g C-x SPC M-x b <return> <return> C-p C-e ; C-n C-a <tab> C-SPC M-x f o <return> C-w"))

(defun aieis-move-c++-def-interactive ()
  "Move member function definition to the other buffer.
It will move it to the buffer reacheable by \`C-o`."
  (interactive)
  (setq aieis-last-c++-name (aieis-find-c++-class-name))
  (aieis-move-c++-member-function))

(provide 'aieis-lang-c++)
;;; aieis-lang-c++.el ends here


