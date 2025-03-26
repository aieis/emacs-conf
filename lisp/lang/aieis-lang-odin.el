;;; aieis-lang-odin.el --- odin setup
;;; Commentary:
;;; Code:

;; With odin-mode (https://github.com/mattt-b/odin-mode) and lsp-mode already added to your init.el of course!.

(require 'aieis-lang-common)
(add-to-list 'aieis/lsp-servers '(odin-mode "ols" ols "odin"))

(provide 'aieis-lang-odin)
;;; aieis-lang-odin.el ends here
