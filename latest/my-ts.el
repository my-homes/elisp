;; EmacsでTypeScript/React開発する2023 - たごもりすメモ https://tagomoris.hatenablog.com/entry/2023/01/24/102616
(use-package web-mode :ensure t)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.ts[x]?\\'" . web-mode))
(add-hook 'web-mode-hook 'lsp)

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
