;;; aieis-lang-json.el -- json setup
;;; Commentary:
;;; Code:


(require 'json-mode)
(define-key json-mode-map (kbd "C-c C-f") 'json-pretty-print-buffer)

(provide 'aieis-lang-json)
;;; aieis-lang-json.el ends here
