;; emacs29でビルドインされたtree-sitterとtsx-ts-modeでtypescriptの開発環境を構築する | joppot https://joppot.info/posts/c05e989a-e642-4c84-a5b8-a0e0c3178941
(use-package lsp-mode :ensure t)

(use-package typescript-ts-mode
  :mode (("\\\\.tsx\\\\'" . tsx-ts-mode)
         ("\\\\.ts\\\\'" . tsx-ts-mode))
  :config
  (setq typescript-ts-mode-indent-offset 2))

(use-package treesit
  :config
  (setq treesit-font-lock-level 4))

(use-package treesit-auto
  :ensure t
  :init
  (require 'treesit-auto)
  (global-treesit-auto-mode)
  :config
  (setq treesit-auto-install t))

(use-package tree-sitter
  :ensure t
  :hook ((typescript-ts-mode . tree-sitter-hl-mode)
         (tsx-ts-mode . tree-sitter-hl-mode))
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-ts-mode . tsx)))

(use-package tide
  :ensure t
  :hook (tsx-ts-mode . setup-tide-mode)
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1))
  (setq company-tooltip-align-annotations t))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mts\\'" . tsx-ts-mode))

(add-hook 'tsx-ts-mode-hook #'(lambda () (lsp)))

(require 'treesit)
(setq treesit-font-lock-level 4)

;; (require 'treesit-auto)
;; (global-treesit-auto-mode)
;; (setq treesit-auto-install t)

(require 'tree-sitter)
(add-hook 'typescript-ts-mode-hook 'tree-sitter-hl-mode)
(add-hook 'tsx-ts-mode-hook 'tree-sitter-hl-mode)
(global-tree-sitter-mode)
(tree-sitter-require 'tsx)
(add-to-list 'tree-sitter-major-mode-language-alist '(tsx-ts-mode . tsx))

(require 'tide)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
(add-hook 'tsx-ts-mode-hook 'setup-tide-mode)
(setq company-tooltip-align-annotations t)

;; (setq treesit-language-source-alist
;;       '((tsx . ("https://github.com" "master" "tsx/src"))
;;         (typescript . ("https://github.com" "master" "typescript/src"))))

;; (treesit-install-language-grammar 'tsx)

;; Treesitの設定
(setq treesit-language-source-alist
      '((json "https://github.com/tree-sitter/tree-sitter-json")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        ))
;; Treesitがインストールされてない場合は自動でインストールする
(dolist (element treesit-language-source-alist)
  (let* ((lang (car element)))
    (if (treesit-language-available-p lang)
        (message "treesit: %s is already installed" lang)
      (message "treesit: %s is not installed" lang)
      (treesit-install-language-grammar lang))))

(require 'treesit-auto)
(global-treesit-auto-mode)
(setq treesit-auto-install t)

(use-package treesit-auto
  :demand t
  :config
  (global-treesit-auto-mode))

(treesit-install-language-grammar 'tsx)

;; https://sourceforge.net/projects/elisp-tree-sitter.mirror/files/
;; https://github.com/iquiw/emacs-tree-sitter-module

(provide 'my-tsx)
