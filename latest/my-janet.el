;;;# -*- mode:emacs-lisp-mode; -*-
;;(straight-use-package 'janet-mode)
(use-package janet-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.janet\\'" . janet-mode))
(add-to-list 'auto-mode-alist '("\\.j\\'" . janet-mode))
;(setq janet-special-forms (sort (cons "forv" janet-special-forms) #'string<))
(put 'forv 'janet-indent-function 3)

(provide 'my-janet)
