;; https://github.com/emacs-typescript/typescript.el

(use-package typescript-mode :ensure t)
(require 'typescript-mode)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(provide 'my-ts)
