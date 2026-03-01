(load "~/.emacs.d/xprint.el")
;; (load "~/.emacs.d/straight.el")

(setq my-env::*use-evil* t)

;; https://github.com/skeeto/at-el
;; 【@, another object system for Emacs Lisp】
;; (straight-use-package '(@ :type git :host github :repo "skeeto/at-el"))
;; (require '@)

(require 'my-env)

(use-package names :ensure t)
(use-package s :ensure t)
(use-package js2-mode :ensure t)
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.mts\\'" . js2-mode))
(add-hook 'js-mode-hook (function (lambda () (setq indent-tabs-mode nil tab-width 2 js-indent-level 2))))
(setq js2-basic-offset 2) ;; For js2-mode

(add-to-list 'load-path (expand-file-name "~/emacs-ffi"))
;;  (require 'ffi)
(require 'json)
;;(use-package s)
;;(require 's)

  (defun api::json::encode ($x)
    (json-encode $x))
  (defun api::json::parse ($x)
    (json-parse-string
     $x
     :array-type 'list
     :false-object nil
     :object-type 'plist))

(if noninteractive
    nil
  ;;(get-feature 'my-env)
  ;;(require 'my-env)
  (require 'my-env)

  (xdump (my-env::*list-non-special-buffers*))

  (setq warning-minimum-level :emergency)
  (setq-default make-backup-files nil)
  (setq-default indent-tabs-mode nil)
  (put 'erase-buffer 'disabled nil)

  (setq locale-coding-system 'utf-8-unix)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  ;; Set default buffer file coding system to utf-8-unix
  (setq default-buffer-file-coding-system 'utf-8-unix)
  ;; Also set the default for undecided files
  (prefer-coding-system 'utf-8-unix)
  ;;改行コード表示
  (setq eol-mnemonic-dos "(CRLF)")
  (setq eol-mnemonic-mac "(CR)")
  (setq eol-mnemonic-unix "(LF)")

  ;; [get-feature]
  ''(unless (featurep 'get-feature)
    (defun get-feature (feature-name &optional url file-name)
      (if (featurep feature-name) t
        (unless url (setq url (format "https://github.com/emacs-pkg/%s/raw/main/%s.el"
                                      feature-name feature-name)))
        (unless file-name (setq file-name (format "%s.el" feature-name)))
        (let ((make-backup-files nil)
              (file-path (expand-file-name file-name user-emacs-directory)))
          (ignore-errors
            (url-copy-file url file-path 'ok-if-already-exists))
          (ignore-errors
            (load file-path nil 'nomessage))
          (featurep feature-name))))
    (get-feature 'get-feature))

  ;;(get-feature 'oop)
  ;;(get-feature 'getprop)

  ;;(get-feature 'newlisp)
  ;;(add-to-list 'auto-mode-alist '("\\.lsp$" . newlisp-mode))

  ;;(get-feature 'janet-mode)
  ;;(add-to-list 'auto-mode-alist '("\\.janet$" . janet-mode))
  ;;(add-to-list 'auto-mode-alist '("\\.j$"     . janet-mode))

  ;;(require 'package)

  ;; [racket]
  ;; (straight-use-package 'racket-mode)
  ;;(use-package racket-mode)
  ;;(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))

  (defun eshell-load-bash-aliases ()
    "Read Bash aliases and add them to the list of eshell aliases."
    ;; Bash needs to be run - temporarily - interactively
    ;; in order to get the list of aliases.
    (interactive)
    (with-temp-buffer
      (call-process "bash" nil '(t nil) nil "-ci" "alias")
      (goto-char (point-min))
      (while (re-search-forward "alias \\(.+\\)='\\(.+\\)'$" nil t)
        (eshell/alias (match-string 1) (match-string 2)))))
  ;; (add-hook 'eshell-mode-hook 'eshell-load-bash-aliases)

  (defun list-all-buffers (&optional files-only)
    "Display a list of names of existing buffers.
The list is displayed in a buffer named `*Buffer List*'.
Non-null optional arg FILES-ONLY means mention only file buffers.

For more information, see the function `buffer-menu'."
    (interactive "P")
    (display-buffer (list-buffers-noselect files-only (buffer-list))))

  (defun mu-open-in-external-app ()
    "Open the file where point is or the marked files in Dired in external
app. The app is chosen from your OS's preference."
    (interactive)
    (let* ((file-list
            (dired-get-marked-files)))
      (mapc
       #'(lambda (file-path)
           (my-env::*run-command-in-eshell*
            default-directory
            (format "bash.exe -c \"start 'C:/Program Files/KMPlayer 64X/KMPlayer64.exe' '%s'\"" (file-name-nondirectory file-path))
            )
           )
       file-list)))

  ;; (require 'example)
  )

;;(add-to-list 'load-path (expand-file-name "~/ejacs"))
;;(autoload 'js-console "js-console" nil t)

;;(use-package xwwp :ensure t)

(add-to-list 'load-path "~/emacs-websocket/")
(require 'websocket)

(add-to-list 'load-path "~/deno-bridge/")
(require 'deno-bridge)

(provide 'my-init)
