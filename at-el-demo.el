#! /usr/bin/env -S emacs -batch -l
(load "~/.emacs.d/init.el")
(unless (featurep 'get-feature)
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
(get-feature 'straight)

(require 'straight)
(use-package @)
(require '@)

(log "hello")

;; Create a rectangle prototype, extending the root object @.
;; Convention: prefix "class" variable names with @.
(defvar @rectangle (@extend :width nil :height nil))

;; (log @rectangle "@rectangle")

;; The @ function is used to access properties of an object, following
;; the prototype chain breadth-first as necessary. An error is thrown
;; if the property has not been defined.
(@ @rectangle :width) ; => nil

(log (@ @rectangle :width) "(1)")

;; The @ function is setf-able. Assignment *always* happens on the
;; immediate object, never on a parent prototype.
(setf (@ @rectangle :width) 0)
(setf (@ @rectangle :height) 0)

;; Define the method :area on @rectangle.
;; The first argument is this/self. Convention: call it @@.
(setf (@ @rectangle :area) (lambda (@@) (* (@ @@ :width) (@ @@ :height))))

;; Convenience macro def@ for writing methods. Symbols like @: will be
;; replaced by lookups on @@. The following is equivalent to the above
;; definition.
(def@ @rectangle :area ()
  (* @:width @:height))

;; Create a color mix-in prototype
(defvar @colored (@extend :color (list)))

;; The @: variables are setf-able, too.
(def@ @colored :mix (color)
  (push color @:color))

;; Create a colored rectangle from the prototypes.
(defvar foo (@extend @colored @rectangle :width 10 :height 4))

;; @! is used to call methods. The object itself is passed as the
;; first argument to the function stored on that prototype's property.
(log (@! foo :area))  ; => 40
(log (@! foo :mix :red))
(log (@! foo :mix :blue))
(log (@ foo :color))  ; => (:blue :red)

;; @: variables are turned into method calls when in function position.
(def@ foo :describe ()
  (format "{color: %s, area: %d}" @:color (@:area)))

(log (@! foo :describe))  ; => "{color: (:blue :red), area: 40}"
