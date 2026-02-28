(require 'package)
(require 'xprint)

(require 'package)
(setq package-archives
      '(
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ;;("org" . "http://orgmode.org/elpa/")
        ("melpa" . "https://melpa.org/packages/")
        ))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package)
    (package-install 'use-package))

(require 'files)
(let* ((elc-list (directory-files-recursively "~/.emacs.d/elpa/" ".+[.]elc")))
  (unless elc-list
    (package-recompile-all)
    )
  )

(provide 'my-pkg)
