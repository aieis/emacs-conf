;;; prog-ue.el --- handling unreal engine
;;; Commentary:
;;; Code:
(require 'sys-vars)

(push '("\\.uproject\\'" . json-mode) auto-mode-alist)

(defvar aieis/llvm-versions
  (if (file-exists-p aieis/llvm-root-path)
      (seq-filter (lambda (it)(eq (or (string-equal "." it) (string-equal ".." it)) nil))
                  (directory-files aieis/llvm-root-path))
    '()))

(defun aieis/llvm-activate (tc)
  "Activate a toolchain TC from discovered toolchains."
  (interactive (list (completing-read "Select toolchain: " aieis/llvm-versions)))
  (let* ((tcr (concat aieis/llvm-root-path "/" tc))
         (tcp (concat tcr "/bin"))
         (nexec-path (--filter (eq (string-match aieis/llvm-root-path it) nil) exec-path)))
    (add-to-list 'nexec-path tcp)
    (setq exec-path nexec-path)
    (setenv "LLVM_PATH" tcr)
    (setenv "LIBCLANG_PATH" tcp)
    (setenv "PATH" (s-join path-separator exec-path))))


(defun aieis-ue-hook ()
  (dolist (macro '("UCLASS" "GENERATED_BODY" "UFUNCTION" "UPROPERTY" "UE_LOG"))
    (add-to-list 'c-macro-names-with-semicolon macro))
  (c-make-macro-with-semi-re))
(add-to-list 'c++-mode-hook 'aieis-ue-hook)

(provide 'prog-ue)
;;; prog-ue.el ends here
