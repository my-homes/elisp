;; -*- coding: utf-8 -*-
;;; -*- lexical-binding: t -*-

(setq visible-bell nil)

;; (setq-default display-line-numbers-position 'right)
;; (setq-default display-line-numbers 'visual)
;; (setq-default display-line-numbers-width 4)
;; (setq-default display-line-numbers-width-start t)
;; (set-face-attribute 'line-number nil :foreground "DarkOliveGreen" :background "#131521")
;; (set-face-attribute 'line-number-current-line nil :foreground "gold")
;; (global-display-line-numbers-mode t)

;; https://kei10in.hatenablog.jp/entry/20091104/1257341380
(when nil
  (require 'linum)
  (setq linum-format
        '(lambda (line)
           (let ((fmt
                  (let ((min-w (if (display-graphic-p) 10 5))
                        (w (length (number-to-string
                                    (count-lines (point-min) (point-max))))))
                    (concat "%"
                            (number-to-string (if (< min-w w) w min-w))
                            "d: "))))
             (propertize (format fmt line) 'face 'linum))))
  (global-linum-mode)
  )

(global-auto-revert-mode 1)
(require 'dired)
(add-hook 'dired-mode-hook 'auto-revert-mode)

(require 'my-pkg)
(require 'cl-lib)
(require 'xprint)

(use-package company :ensure t)
(use-package general :ensure t)
(use-package racket-mode :ensure t)
(use-package dash :ensure t)
(use-package dired-hacks-utils :ensure t)
(use-package dired-filter :ensure t)
(use-package dired-subtree :ensure t)
(use-package magit :ensure t)

(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
(modify-syntax-entry ?# "<" racket-mode-syntax-table)
(modify-syntax-entry ?\n ">" racket-mode-syntax-table)

(require 'dash)
(require 'dired-hacks-utils)
(require 'dired-filter)
(require 'dired-subtree)

;; https://qiita.com/keita44_f4/items/2adae69f05dd4a7c5f25
;; [company]
;; (straight-use-package 'company)
;; (require 'company)
;; ;;(global-company-mode) ; 全バッファで有効にする
;; (setq company-idle-delay 0.5) ; デフォルトは0.5
;; ;;(setq company-idle-delay 0) ; デフォルトは0.5
;; (setq company-minimum-prefix-length 1) ; デフォルトは4
;; (setq company-selection-wrap-around nil) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
;; (add-hook 'emacs-lisp-mode-hook #'(lambda () (company-mode 1)))
;; (add-hook 'lisp-interaction-mode-hook #'(lambda () (company-mode 1)))
;; (add-hook 'racket-mode-hook #'(lambda () (company-mode 1)))
;; (add-hook 'lisp-mode-hook #'(lambda () (company-mode 1)))

(setq scroll-step 1)
(setq scroll-conservatively 101)

(setq-default file-name-coding-system 'cp932)
(setq-default truncate-lines t)

(require 'arc-mode)
(add-to-list 'auto-mode-alist '("\\.nupkg\\'" . archive-mode))
;; (add-to-list 'auto-mode-alist '("\\.tar\\'" . archive-mode))

(defvar *ctrl-c-binding* (key-binding ( kbd "C-c")))
(defvar *ctrl-x-binding* (key-binding ( kbd "C-x")))

(require 'cl-lib)
(require 'eshell)
;;(define-key eshell-proc-mode-map ( kbd "C-c") *ctrl-c-binding*)
(require 'find-func)
(require 'view)
(require 'json)

;;(require 'names)

;;(eval-when-compile (require 'names))
;;;###autoload
;;(define-namespace my-env::

(setq my-env::version "v3.6.1")

(defun my-env::*query-replace* ()
  (interactive)
  (view-mode-exit t)
  (call-interactively #'query-replace)
  )

(defun my-env::*copy-region-or-yank* ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'my-env::*copy-region*)
    (call-interactively #'my-env::*view-mode-yank*)
    )
  )

(defun my-env::*view-mode-tab-key* ()
  (interactive)
  (view-mode-exit t)
  (call-interactively 'indent-for-tab-command)
  )

(defun my-env::*view-mode-undo* ()
  (interactive)
  (view-mode-exit t)
  (call-interactively 'undo)
  ;;(view-mode-enter t)
  )

(defun my-env::*view-mode-redo* ()
  (interactive)
  (view-mode-exit t)
  (call-interactively 'undo-redo)
  ;;(view-mode-enter t)
  )

(defun my-env::*view-mode-yank* ()
  (interactive)
  (let* (($is-view-mode view-mode))
    (when $is-view-mode
      (view-mode-exit t)
      )
    (call-interactively 'yank)
    (when $is-view-mode
      ;;(view-mode-enter t)
      )
    )
  )

(defun my-env::*mode* ()
  (and
   (not (window-minibuffer-p (selected-window)))
   my-env::*mode-is-on*))

(defun my-env::*ding* ()
  (if my-env::ding-dings (ding)))

(defun my-env::*redisplay* ()
  (interactive)
  (my-env::*recenter*)
  ;;(my-env::*show-info*)
  (force-mode-line-update)
  (when (input-pending-p) (discard-input)))

(defun my-env::*down-key* ()
  (interactive)
  (if (eq major-mode 'dired-mode)
      (dired-next-line 1)
    (if (my-env::*mode*)
        (my-env::*slide-down*)
      (my-env::*next-line*)
      )
    (my-env::*redisplay*)))

(defun my-env::*up-key* ()
  (interactive)
  (if (eq major-mode 'dired-mode)
      (dired-*previous-line* 1)
    (if (my-env::*mode*)
        (my-env::*slide-up*)
      (my-env::*previous-line*)
      )
    (my-env::*redisplay*)))

(defun my-env::*right-key* ()
  (interactive)
  (if (my-env::*mode*)
      (my-env::*forward-sexp*)
    (my-env::forward-char))
  (my-env::*redisplay*))

(defun my-env::*left-key* ()
  (interactive)
  (if (my-env::*mode*)
      (my-env::*backward-sexp*)
    (my-env::backward-char))
  (my-env::*redisplay*))

(defun my-env::*right-quick* ()
  (interactive)
  (my-env::*forward-sexp*)
  (my-env::*recenter*))

(defun my-env::*left-quick* ()
  (interactive)
  (my-env::*backward-sexp*)
  (my-env::*recenter*))

(defun my-env::*up-quick* ()
  (interactive)
  (re-search-backward "^\\s(" nil t)
  (my-env::*recenter*))

(defun my-env::*down-quick* ()
  (interactive)
  (and
   (ignore-errors
     (save-excursion (forward-char) (re-search-forward "^\\s(" nil t)))
   (progn (forward-char) (re-search-forward "^\\s(" nil t))
   (backward-char))
  (my-env::*recenter*))

(defun my-env::*slide-down* ()
  (if (not (bolp))
      (forward-char)
    (if (eobp) (my-env::*ding*) (forward-line 1))))

(defun my-env::*slide-up* ()
  (if (not (bolp))
      (backward-char)
    (if (bobp) (my-env::*ding*) (forward-line -1))))

(defun my-env::forward-char()
  (if (eobp)
      (my-env::*ding*)
    (forward-char)))

(defun my-env::backward-char ()
  (if (bobp)
      (my-env::*ding*)
    (backward-char)))

(defun my-env::*next-line* ()
  (if (save-excursion (end-of-line) (eobp))
      (my-env::*ding*)
    (next-line)))

(defun my-env::*previous-line* ()
  (if (save-excursion (beginning-of-line) (bobp))
      (my-env::*ding*)
    (previous-line)))

(defun my-env::*forward-sexp* ()
  (interactive)
  (cond
   ((eobp) (my-env::*ding*))
   ((my-env::*within-string* (point)) (my-env::*forward-within-string*))
   ;;((looking-at "#'" (forward-sexp)))
   ((looking-at "#|") (search-forward "|#" nil t))
   ((looking-at "(") (forward-sexp))
   ((looking-at "\n")
    (let ((bol? (bolp)))
      (forward-char)
      (when bol?
        (while (and (bolp) (looking-at "\n"))
          (forward-char)))))
   ((looking-at "\\s-*\\s<")
    (let ((opoint (point)))
      (forward-line)
      (while (looking-at "\\s-*\\s<")
        (setq opoint (point))
        (forward-line))
      (goto-char (max opoint (save-excursion (beginning-of-line) (point))))))
   ((looking-at "\\s)") (my-env::*ding*))
   ((looking-at "\\s.")
    (forward-char))
   ((looking-at "#")
    (let ((opoint (point)))
      (forward-line)
      (while (looking-at "#")
        (setq opoint (point))
        (forward-line))
      (goto-char (max opoint (save-excursion (beginning-of-line) (point))))))
   ((looking-at "\\s-") (while (looking-at "\\s-") (forward-char)))
   (t (ignore-errors (forward-sexp)))))

(defun my-env::*backward-sexp* ()
  (interactive)
  (let (comment-begin)
    (cond
     ((bobp) (my-env::*ding*))
     ((my-env::*within-string* (point)) (my-env::*backward-within-string*))
     ((looking-back "|#") (search-backward "#|" nil t))
     ;; ((and (looking-back "|") (looking-at "#")) (search-backward "#|" nil t))
     ((looking-back "\\s(") (my-env::*ding*))
     ((looking-back "\\s.")
      (backward-char))
     ((and (looking-back "\\s>")
           (save-excursion (backward-char)
                           (setq comment-begin (my-env::*find-comment* (point)))))
      (goto-char comment-begin)
      (while (and (looking-back "\\s>")
                  (save-excursion
                    (backward-char)
                    (setq comment-begin (my-env::*find-comment* (point)))))
        (goto-char comment-begin))
      (when (and (looking-back "|") (looking-at "#"))
        (forward-char)
        )
      )
     ((looking-back "\\s-") (while (looking-back "\\s-") (backward-char)))
     ((looking-back "\\s<") (while (looking-back "\\s<") (backward-char)))
     ((looking-back "\n")
      (backward-char)
      (while (and (bolp) (looking-back "\n")
                  (save-excursion (backward-char) (bolp)))
        (backward-char)))
     (t (ignore-errors (backward-sexp))))))

(defun my-env::*within-string* (pos)
  (save-excursion
    (goto-char pos)
    (let ((parsed (syntax-ppss)))
      (if (nth 3 parsed) (nth 8 parsed) nil))))

(defun my-env::*forward-within-string* ()
  (let ((opoint (point))
        (parsed (syntax-ppss))
        beg end)
    (save-excursion
      (setq beg (nth 8 parsed))
      (goto-char beg)
      (forward-sexp)
      (setq end (point)))
    (if (>= (point) (1- end))
        (my-env::*ding*)
      (forward-char))))

(defun my-env::*backward-within-string* ()
  (let ((opoint (point))
        (parsed (syntax-ppss))
        beg end)
    (save-excursion
      (setq beg (nth 8 parsed))
      (goto-char beg)
      (forward-sexp)
      (setq end (point)))
    (if (<= (point) (1+ beg))
        (my-env::*ding*)
      (backward-char))))

(defun my-env::*find-comment* (eol)
  (save-excursion
    (goto-char eol)
    (let ((parsed (syntax-ppss)))
      (if (not (nth 4 parsed))
          nil
        (goto-char (nth 8 parsed))
        (while (looking-back "\\s-") (backward-char))
        (point)))))

(defun my-env::*show-info* ()
  (when (my-env::*mode*)
    (save-excursion
      (cond
       ((my-env::*within-string* (point)) nil)
       ((looking-back "\\s)\\|\\s\"\\|\\sw\\|\\s_")
        (let ((opoint (point)))
          (my-env::*backward-sexp*)
          (my-env::*count-lines* opoint (point))))
       ((looking-at
         "\\(\\s-*\\)\\(\\sw\\|\\s_\\|\\s(\\|\\s<\\|\\s\"\\|\\s'\\)")
        (goto-char (match-end 1))
        (let ((opoint (point)))
          (my-env::*forward-sexp*)
          (my-env::*count-lines* opoint (point))))))))

(defun my-env::*count-lines* (start end)
  (let ((lines (count-lines start end)))
    (if (= lines 1) (message "1 line.") (message "%s lines." lines))))

(defun my-env::*recenter* ()
  (interactive)
  (cond
   ((pos-visible-in-window-p (point)) nil)
   ((< (point) (window-start)) (recenter 0))
   (t (recenter -1)))
  )

(defun my-env::*operate-on-region-or-sexp* (op)
  (interactive)
  (let* (($is-view-mode view-mode))
    (when $is-view-mode
      (view-mode-exit t)
      )
    (if (region-active-p)
        (funcall op (mark) (point))
      (funcall op
               (point)
               (progn (my-env::*forward-sexp*) (point))
               )
      )
    (when $is-view-mode
      ;;(view-mode-enter t)
      )
    )
  )

(defun my-env::*delete-forward-char* ()
  (interactive)
  (view-mode-exit t)
  (if (region-active-p)
      (call-interactively #'my-env::*delete-region*)
    (call-interactively #'delete-forward-char)
    )
  )

(defun my-env::*delete-backward-char* ()
  (interactive)
  (view-mode-exit t)
  (if (region-active-p)
      (call-interactively #'my-env::*delete-region*)
    (call-interactively #'delete-backward-char)
    )
  )

(defun my-env::*copy-region* ()
  (interactive)
  (my-env::*operate-on-region-or-sexp*
   #'(lambda (beg end)
       (kill-ring-save beg end)
       (setq this-command '*kill-region*)))
  ;;(message "*copy-region*")
  )

(defun my-env::*delete-region* ()
  (interactive)
  (my-env::*operate-on-region-or-sexp* #'delete-region))

(defun my-env::*indent-region* ()
  (interactive)
  (my-env::*operate-on-region-or-sexp* #'indent-region))

(defun my-env::*kill-region* ()
  (interactive)
  (my-env::*operate-on-region-or-sexp* #'kill-region))

(defun my-env::*comment-region* ()
  (interactive)
  (my-env::*operate-on-region-or-sexp* #'comment-region))

(defun my-env::*uncomment-region* ()
  (interactive)
  (my-env::*operate-on-region-or-sexp* #'uncomment-region))

(defun my-env::*buffer-visible* (buffer)
  (let ((wins (window-list))
        (found nil))
    (dolist (win wins)
      (when (eq buffer (window-buffer win))
        (setq found t)
        )
      )
    found))

(defun my-env::*rotate-buffer* ()
  (interactive)
  (let ((bufflist (buffer-list))
        (bufforig (current-buffer))
        (found nil)
        currbuff buffname)
    (while (and bufflist (not found))
      (setq currbuff (pop bufflist))
      (setq buffname (buffer-name currbuff))
      (cond
       ((eq bufforig currbuff) nil)
       ((minibufferp currbuff) nil)         ; minibuffer
       ((my-env::*buffer-visible* currbuff) nil)
       ((string= buffname "*GNU Emacs*") nil)
       ((string= buffname "*Bookmark List*") nil)
       ((string= buffname "*Buffer List*") nil)
       ((string= buffname "*Messages*") nil)
       ((string= buffname "*Backtrace*") nil)
       ((string= buffname "*Help*") nil)
       ((string= buffname "*Completions*") nil)
       ((string= buffname "*Compile-Log*") nil)
       ((string= buffname "*Quail Completions*") nil)
       ((string= buffname "*Warnings*") nil)
       ((string= buffname "*Async-native-compile-log*") nil)
       ((string= buffname "*straight-byte-compilation*") nil)
       ((string= buffname "*straight-process*") nil)
       ;;((string= buffname "*xprint*") nil) ; *xprint*
       ((string-match "^[*]" buffname) nil) ; *scratch*, *Help* etc
       ((string-match "^[ ]" buffname) nil) ; work buffer
       (t (setq found currbuff))))
    (if (not found)
        (my-env::*ding*)
      (switch-to-buffer found)
      (bury-buffer found))))

(defun my-env::*jump-to-function* (&optional func-name)
  (interactive)
  (unless func-name (setq func-name (find-tag-default)))
  (let* (;;(func-name (find-tag-default))
         (interned (intern func-name))
         ;;(cb (current-buffer))
         (cw (selected-window))
         )
    ;; (unless in-current-window (delete-other-windows))
    (when (and (symbol-function interned)
               (symbolp (symbol-function interned)))
      (setq interned (symbol-function interned))
      )
    (if (and (fboundp interned) (subrp (symbol-function interned)))
        (describe-function interned)
      (cond
       ((condition-case nil
            (progn (find-function-do-it interned nil 'switch-to-buffer) 'ok)
          (error nil)
          ) nil)
       ((condition-case nil
            (progn (find-function-do-it interned 'defvar 'switch-to-buffer 'ok))
          (error nil)
          ) nil)
       (t (error "%s not found" interned))
       )
      (select-window cw))))

(defun my-env::*other-window* (count &optional all-frames)
  (interactive "p")
  (if (>= (length (window-list)) 2)
      (other-window count all-frames)
    ;; (save-window-excursion
    ;;   (list-buffers nil)
    ;;   )
    ;; (switch-to-buffer-other-window "*Buffer List*")
    (my-env::*list-buffers*)
    (delete-other-windows)
    )
  (message "%S" (current-buffer))
  )

(defun my-env::*exchange-point-and-mark* (arg)
  (interactive "P")
  (let ((active (region-active-p)))
    (exchange-point-and-mark arg)
    (if (not active)
        (deactivate-mark))))

(defun my-env::*kill-current-buffer* ()
  (interactive)
  (cond
   ((and (eq major-mode 'Buffer-menu-mode) Buffer-menu-files-only)
    (my-env::*list-buffers*)
    )
   ((get-buffer-process (current-buffer))
    (my-env::*rotate-buffer*)
    (ignore-errors
      (delete-window)))
   ((and
     (buffer-file-name (current-buffer))
     (buffer-modified-p (current-buffer)))
    (my-env::*rotate-buffer*)
    (ignore-errors
      (delete-window)))
   (t
    (let* (
           (bname (buffer-name))
           (fname buffer-file-name)
           (dname default-directory)
           )
      (kill-buffer (current-buffer))
      (unless (equal bname "*Buffer List*")
        (message (format "Killed buffer: %s" bname))
        )
      (ignore-errors
        (delete-window)
        )
      (cond
       ((and fname (not (string-search ".zip:" fname)) (not (string-search ".nupkg:" fname)))
        (unless
            (and
             (eq major-mode 'dired-mode)
             (equal default-directory dname))
          (my-env::*list-files*)
          )
        )
       ((eq major-mode 'Buffer-menu-mode)
        (call-interactively #'Buffer-menu-execute)
        )
       ((equal (buffer-name) " *xprint*")
        (call-interactively #'bury-buffer)
        )
       ((not view-mode)
        (my-env::*list-buffers*)
        )
       )
      ))))

(defun my-env::*kill-other-buffers* ()
  (interactive)
  (let ((cb (current-buffer))
        (cw (selected-window))
        (wins (window-list)))
    (dolist (win wins)
      (if (equal win cw) nil
        (select-window win)
        (cond
         ((equal (current-buffer) cb) nil)
         ((get-buffer-process (current-buffer)) nil)
         ((and
           (buffer-file-name (current-buffer))
           (buffer-modified-p (current-buffer)))
          nil)
         (t (kill-buffer (current-buffer))))
        ;;(delete-window win)
        )
      )
    (select-window cw)
    (ignore-errors (delete-other-windows))
    )
  )

(defun my-env::*bookmark-set* ()
  (interactive)
  (if (null (buffer-file-name)) (error "Not a file buffer.")
    (let ((name
           (format
            "%s:%s"
            (file-name-nondirectory (buffer-file-name))
            (format-time-string "%Y/%m/%d-%H:%M:%S" (current-time)))))
      (bookmark-set name)
      (message "Bookmark %s created." name)
      )
    ))

(defun my-env::*list-bookmarks* ()
  (interactive)
  (list-bookmarks)
  (switch-to-buffer "*Bookmark List*")
  )

(defun my-env::*list-files* ()
  (interactive)
  (ignore-errors (kill-buffer "*Buffer List*"))
  (let (win)
    (save-window-excursion
      (setq win (list-buffers t))))
  (switch-to-buffer "*Buffer List*")
  )

(defun my-env::*list-buffers* ()
  (interactive)
  (ignore-errors (kill-buffer "*Buffer List*"))
  (let (win)
    (save-window-excursion
      (setq win (list-buffers nil))))
  (switch-to-buffer "*Buffer List*")
  )

(defun my-env::*rerun-eshell* (&optional other-window)
  (interactive)
  (condition-case nil
      (kill-buffer "*eshell*")
    (error nil))
  (let* ((cwd default-directory))
    (save-window-excursion
      (switch-to-buffer "*eshell*")
      (cd cwd)
      (eshell)
      )
    (if other-window
        (switch-to-buffer-other-window "*eshell*")
      (switch-to-buffer "*eshell*")
      )
    )
  )

(defun my-env::*run-command-in-eshell* (dir cmd)
  (ignore-errors (kill-buffer "*eshell*"))
  (save-window-excursion
    (cd dir)
    (eshell)
    )
  (switch-to-buffer-other-window "*eshell*")
  (goto-char (point-max))
  (insert cmd)
  (eshell-send-input)
  )

(defun my-env::*cmd-line-to-shell-text* (cmd-list)
  (let*
      ((json (my-env::json::encode cmd-list))
       (file-name (make-temp-file "tmp." nil ".json"))
       ;; text
       )
    (xdump file-name)
    ;; (setq text (concat "#! /usr/bin/env xrun" "\n" json))
    (xdump json)
    (f-write-text json 'utf-8 file-name)
    file-name
    )
  )

(defun my-env::*media-list-m3u-text* (media-list)
  (let*
      (
       (file-name (make-temp-file "tmp." nil ".m3u"))
       (m3u-text "")
       )
    (dolist (media media-list)
      (xdump media)
      (setq m3u-text (concat m3u-text media "\n"))
      )
    (xdump m3u-text)
    (xdump file-name)
    ;;(f-write-text m3u-text 'utf-8 file-name)
    (let* ((coding-system-for-write 'utf-8))
      (with-temp-file file-name
        (insert m3u-text)))
    file-name
    )
  )

(defun my-env::*run-file-in-eshell* (prefix-arg)
  (interactive "P")
  (if (null buffer-file-name) (*ding*)
    (let* (
           (win (selected-window))
           (dir (file-name-directory buffer-file-name))
           (fname (file-name-nondirectory buffer-file-name))
           (fext (file-name-extension fname))
           ;; (cmd-args (read-string (format "Arguments for '%s': " fname)))
           (end-of-args nil)
           cmd-arg
           (cmd-args (list fname))
           cmd-file
           ;; (cmd-file
           ;;  (my-env::*cmd-line-to-shell-text*
           ;;   ;; (list "bash" "-c" fname))
           ;;   (list fname))
           ;;  )
           ;; (cmd (format "xrun-json '%s' %s" cmd-file cmd-args))
           cmd
           )
      ;; (setq cmd (format "cd \"%s\" && xrun './%s %s'" dir fname cmd-args))
      ;;(setq cmd (format "xrun './%s %s'" fname cmd-args))
      (when prefix-arg
        (while (not end-of-args)
          (setq cmd-arg (read-string (format "Argument for '%s': " fname)))
          (if (string-empty-p cmd-arg)
              (setq end-of-args t)
            ;; (add-to-list 'cmd-args (format "\"%s\"" cmd-arg) t)
            (add-to-list 'cmd-args cmd-arg t)
            )
          )
        )
      (setq cmd-file
            (my-env::*cmd-line-to-shell-text*
             ;; (list "bash" "-c" fname))
             ;; (list fname)
             cmd-args
             )
            )
      (setq cmd (format "xrun-json '%s'" cmd-file))
      (ignore-errors
        (set-file-modes (buffer-file-name) (string-to-number "775" 8))
        )
      (delete-other-windows)
      (my-env::*run-command-in-eshell* dir cmd)
      (select-window win)
      )
    )
  )

(defun create-and-select-new-frame ()
  "Create a new frame and select it."
  (interactive)
  (when (display-graphic-p)
    (let* ((new-frame (make-frame)))
      (select-frame-set-input-focus new-frame)
      new-frame
      )
    )
  )

(defun my-env::*lookup-file-or-function* ()
  (interactive)
  ;;(xclear)
  ;;(xdump load-path)
  (let* (;;(path (cons user-emacs-directory load-path))
         (feature-list (if (boundp '*get-feature-list*) *get-feature-list* nil))
         names
         name)
    ;;(xdump path)
    ;;(xdump (expand-file-name "init.el" user-emacs-directory))
    ;;(xdump (file-exists-p (expand-file-name "init.el" user-emacs-directory)))
    (when (file-exists-p (expand-file-name "init.el" user-emacs-directory))
      (push "init.el" feature-list))
    ;;(xdump feature-list)
    (setq names (copy-sequence feature-list))
    (dolist (p load-path)
      ;;(xdump p)
      ;;(xdump (directory-files p nil "[.]el$"))
      (dolist (el (ignore-errors (directory-files p nil "[.]el$")))
        (unless (member el names)
          (push el names)
          )
        )
      )
    (mapatoms
     #'(lambda (x)
         (when (fboundp x)
           (push (symbol-name x) names)
           )
         )
     )
    (setq names (sort names #'string-lessp))
    ;;(xdump names)
    (setq name
          (completing-read
           "File Name or Function Name (Completion): "
           names nil t))
    ;;(xdump name)
    (cond
     ((string-suffix-p ".el" name)
      ;;(*ding*)
      (catch :break
        ;;(xdump feature-list)
        ;;(xdump name)
        ;;(xdump (member name feature-list))
        (when (member name feature-list)
          ;;(xprint :raw "(1)")
          (find-file (expand-file-name name user-emacs-directory))
          (message "%s" (expand-file-name name user-emacs-directory))
          (throw :break t)
          )
        ;;(xprint :raw "(2)")
        (dolist (p load-path)
          (let ((els (ignore-errors (directory-files p nil "[.]el$"))))
            (when (member name els)
              ;;(find-file (expand-file-name name p))
              (let (($new-frame (create-and-select-new-frame)))
                (find-file (expand-file-name name p))
                (message "%s" (expand-file-name name p))
                (throw :break t)
                )
              )
            )
          )
        )
      )
     (t
      ;;(xprint :raw "(3)")
      ;;(xdump name)
      ;;(my-env::*jump-to-function* name t)
      (describe-function (intern-soft name))
      )
     )
    )
  )

(defvar eshell-last-command-status nil)

(defun eshell-exec (command)
  (eshell-print (format "(EXEC)> %s\n" command))
  (let* ((output (eshell-command-result command)))
    (setq output
          (replace-regexp-in-string "\\`[ \t\n\r]*\\|[ \t\n\r]*\\'" "" output))
    (setq output
          (replace-regexp-in-string "^" "  " output))
    (eshell-print (format "%s\n" output))
    (eshell-print (format "    ====> %d\n" eshell-last-command-status))
    (setq my-env::eshell-last-command-status eshell-last-command-status)
    eshell-last-command-status
    )
  )

(defun eshell-log (&rest rest)
  (let ((fmt (pop rest)))
    (eshell-print (apply #'format (concat "(LOG)> " fmt "\n") rest))
    nil))

(defun eshell-status ()
  my-env::eshell-last-command-status)

(defun my-env::*tab-next* ()
  (interactive)
  (tab-next)
  (let ((lst nil))
    (dolist (win (window-list))
      (push (window-buffer win) lst)
      )
    (message "%S" (reverse lst))))

(defun my-env::*list-non-special-buffers* ()
  (let* ((list (buffer-list))
         result)
    (dolist (b list result)
      (when (not (string-match-p "*" (buffer-name b)))
        (push (buffer-name b) result)))))

(add-hook 'post-command-hook
          #'(lambda ()
              (my-env::*set-mode* my-env::*mode-is-on*)
              (when (eq major-mode 'archive-mode)
                (view-mode-exit t)
                )
              ))

(defadvice save-buffer (before save-buffer-always activate)
  "always save buffer"
  (view-mode-exit 1)
  (set-buffer-modified-p t)
  (set-buffer-file-coding-system 'utf-8-unix)
  (delete-trailing-whitespace)
  ;; (when (eq (key-binding ( kbd "SPC")) 'self-insert-command)
  ;;   (view-mode-enter t)
  ;;   )
  (view-mode-enter t)
  )

''(defun my-env::*yank* ()
  (interactive)
  (view-mode-exit t)
  (call-interactively #'yank)
  )

(defun my-env::*view-mode-space-key* ()
  (interactive)
  (view-mode-exit t)
  ;;(self-insert-command 1 ?\ )
  (call-interactively #'self-insert-command)
  (setq this-command #'self-insert-command)
  )

(defun my-env::*view-mode-return-key* ()
  (interactive)
  (view-mode-exit t)
  (call-interactively 'newline)
  )

''(defun tab-key ()
  (interactive)
  (call-interactively 'indent-for-tab-command)
  )

(defun my-env::*quit* ()
  (interactive)
  ;; (quit-window)
  (delete-frame)
  )

(defun my-env::json::encode ($x)
  (json-encode $x))
(defun my-env::json::parse ($x)
  (json-parse-string
   $x
   :array-type 'list
   :false-object nil
   :object-type 'plist))

(defun version ()
  (interactive)
  (message "my-env %s" version)
  version)

(defun my-env::*quit-buffer* ()
  (interactive)
  (delete-other-windows)
  (my-env::*kill-current-buffer*)
  (unless (eq major-mode 'dired-mode)
    ;;(view-mode-enter t)
    )
  )

;;; Customization

(defvar my-env::ding-dings nil)

(setq bookmark-bmenu-file-column 45)

;; (unless (boundp 'whitespace-line-column)
;;   (setq whitespace-line-column 1000000))

;; (setq whitespace-style
;;       '(face
;;         tabs spaces trailing lines space-before-tab newline
;;         indentation empty space-after-tab
;;         space-mark tab-mark newline-mark
;;         missing-newline-at-eof))

(defadvice eshell-script-interpreter (around esi activate)
  (setq ad-return-value
        (let ((file (ad-get-arg 0))
              (maxlen eshell-command-interpreter-max-length))
          (if (and (file-readable-p file)
                   (file-regular-p file))
              (with-temp-buffer
                (insert-file-contents-literally file nil 0 maxlen)
                (when (re-search-forward "^#![ \t]*\\(.+\\)$" nil t)
                  (let ((lst (split-string (match-string 1))))
                    (when (string= "/usr/bin/env" (car lst))
                      (setq lst (cdr lst))
                      (when (string= "-S" (car lst))
                        (setq lst (cdr lst))
                        )
                      )
                    (append lst (list file)))))))))

;;; Internal Variables

;;(defvar my-env::*mode-is-on* nil)
(defvar my-env::*mode-is-on* t)
(make-variable-buffer-local 'my-env::*mode-is-on*)

;;; Functions

(defun my-env::*toggle-mode* ()
  (interactive)
  ;; (setq my-env::*mode-is-on* (not my-env::*mode-is-on*))
  (my-env::*set-mode* (not my-env::*mode-is-on*))
  (cond
   (my-env::*mode-is-on* (message "my-env::*mode* is ON"))
   (t (message "my-env::*mode* is OFF")))
  )

(defun my-env::*set-mode* (arg)
  (setq my-env::*mode-is-on* arg)
  (show-paren-mode 1)
  (cond
   ((display-graphic-p)
    (setq show-paren-delay 0)
    ;;(setq show-paren-style 'expression)
    (setq show-paren-style 'parenthesis)
    )
   (t
    (setq show-paren-delay 0.125)
    (setq show-paren-style 'parenthesis)
    )
   )
  ;; (show-paren-mode 1)
  ;; (cond
  ;;  ((and arg (display-graphic-p))
  ;;   (setq show-paren-delay 0)
  ;;   (setq show-paren-style 'expression)
  ;;   (setq show-trailing-whitespace t)
  ;;   (whitespace-mode 1)
  ;;   )
  ;;  (t
  ;;   (setq show-paren-delay 0.125)
  ;;   (setq show-paren-style 'parenthesis)
  ;;   ;; (setq display-fill-column-indicator nil)
  ;;   (setq show-trailing-whitespace nil)
  ;;   (whitespace-mode 0)
  ;;   )
  ;;  )
  )

(defun my-env::*dired-open-marked* ()
  "Open marked files in `dired'."
  (interactive)
  (let (
        (marked (dired-get-marked-files))
        (file (ignore-errors (dired-get-filename)))
        )
    (cond
     ((not marked)
      (if (not file)
          (ding)
        (find-file-noselect file)
        (my-env::*list-files*)
        )
      )
     (t
      (message "Opening selcted fies...please wait")
      (mapc 'find-file-noselect (dired-get-marked-files))
      (my-env::*list-files*)
      )
     )
    )
  )

;;(require 'dired-filter)
;;(require 'dired-subtree)

;;(defalias 'my-dired-filter-map dired-filter-map)

(setq dired-listing-switches "-lgGhF")

;; (defun dired-directory-changed-p (dirname) t)
;; C-.でドットファイルの表示と非表示を切り替える
(defun reload-current-dired-buffer ()
  "Reload current `dired-mode' buffer."
  (let* ((dir (dired-current-directory)))
    (progn
      ;; (kill-buffer (current-buffer))
      (progn (setq dired-directory "?") (rename-buffer " ?" 'UNIQUE))
      (dired dir)
      )))
(defun toggle-dired-listing-switches ()
  "Toggle `dired-mode' switch between with and without 'A' option to show or hide dot files."
  (interactive)
  (progn
    (if (string-match "[Aa]" dired-listing-switches)
        (setq dired-listing-switches "-lgGhF")
      (setq dired-listing-switches "-lgGhFA"))
    (reload-current-dired-buffer)))

(add-hook 'Buffer-menu-mode-hook
          #'(lambda ()
              (view-mode-exit t)
              (define-key Buffer-menu-mode-map ( kbd "q") #'my-env::*kill-current-buffer*)
              (define-key Buffer-menu-mode-map ( kbd "j") #'my-env::*down-key*)
              (define-key Buffer-menu-mode-map ( kbd "k") #'my-env::*up-key*)
              ))

(defun my-env::global-bind-key (key fun)
  (define-key global-map       key fun)
  (define-key view-mode-map       key fun)
  )

(defun my-env::visual-bind-key (key fun)
  (define-key view-mode-map       key fun)
  )

(defun my-env::*setup-key-bindings* ()
  (interactive)

  )

(defadvice archive-mode (after xxx2 activate)
  "xxx2"
  (view-mode-exit t)
  (my-env::*setup-key-bindings*)
  (delete-other-windows)
  )

(defadvice archive-extract (after xxx activate)
  "xxxr"
  ;;(view-mode-enter t)
  )

;; https://stackoverflow.com/questions/5154309/how-to-make-a-opened-buffer-read-only-without-reloading-again-with-find-file-re
(add-hook 'find-file-hook
          #'(lambda ()
              (when (string-suffix-p ".rkt" (buffer-file-name))
                (racket-mode))
              (when (and (buffer-file-name)
                         (file-exists-p (buffer-file-name))
                         (file-writable-p (buffer-file-name)))
                (message "View mode enabled in current buffer")
                (unless (or (string-suffix-p ".md" (buffer-file-name)) (string-suffix-p ".markdown" (buffer-file-name)))
                  (view-mode-enter t)
                  )
                )
              (delete-other-windows)
              ))

(add-hook 'archive-extract-hook
          #'(lambda ()
              (when (string-suffix-p ".rkt" (buffer-file-name))
                (racket-mode))
              (when (and (buffer-file-name)
                         (file-exists-p (buffer-file-name))
                         (file-writable-p (buffer-file-name)))
                (message "View mode enabled in current buffer")
                ;;(view-mode-enter t)
                )
              (delete-other-windows)
              ))

(add-hook 'dired-mode-hook
          #'(lambda ()
              (view-mode-exit t)
              (delete-other-windows)
              ))

(add-hook 'tar-mode-hook
          #'(lambda ()
              (view-mode-exit t)
              (delete-other-windows)
              ))

(defun my-env::*escape-key* ()
  (interactive)
  (view-mode-enter t)
  )

(defadvice archive-mode (after xxx2 activate)
  "xxx2"
  (view-mode-exit t)
  (my-env::*setup-key-bindings*)
  (delete-other-windows)
  )

(defun mu-open-in-external-app ()
  "Open the file where point is or the marked files in Dired in external
app. The app is chosen from your OS's preference."
  (interactive)
  (let*
      ((file-list
        (dired-get-marked-files))
       (cmd-file
        (my-env::*media-list-m3u-text* file-list))
       ;; (cmd (format "open-with-default-app.exe '%s' &" cmd-file))
       (cmd (format "start '%s'" cmd-file))
       )
    (my-env::*run-command-in-eshell*
     default-directory
     cmd          )
    )
  )

(add-hook 'minibuffer-setup-hook #'(lambda () (view-mode-exit 1)))

(require 'my-keys)

;;(use-package markdown-mode :ensure t)
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

;;(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; Optional: Use gfm-mode for GitHub Flavored Markdown (GFM)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; yasnippetでコードのスニペットを登録して爆速でプログラミングする | joppot
;; https://joppot.info/posts/fd05c10f-e966-4b96-81e0-f3eeb2caaa3a
(unless (package-installed-p 'yasnippet-snippets)
  (package-install 'yasnippet-snippets))
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        ;;"/path/to/some/collection/"           ;; foo-mode and bar-mode snippet collection
        ;;"/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
        ))
(yas-global-mode 1)

(add-hook 'markdown-mode
          #'(lambda ()
              (view-mode-exit 1)
              )
          )

;; "C-x y n" yas-new-snippetでスニペット用のバッファを用意します。
;; "C-x y v" yas-visit-snippet-file
;; "C-x y l" yas-describe-tablesは現在のメジャーモードで使えるスニペットのテーブルを表示します。
;; "C-x y i" yas-insert-snippetはエコーエリアで対話的にスニペット挿入をサポートしてくれます。
(global-set-key (kbd "C-x y n") #'yas-new-snippet)
(global-set-key (kbd "C-x y v") #'yas-visit-snippet-file)
(global-set-key (kbd "C-x y l") #'yas-describe-tables)
(global-set-key (kbd "C-x y i") #'yas-insert-snippet)

;; (define-key *ctrl-x-binding* (kbd "y") yas-minor-mode-map)

(provide 'my-env)
