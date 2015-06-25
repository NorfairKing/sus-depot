;;; extensions.el --- private-org Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq private-org-pre-extensions
      '(
        todo-keywords
        agenda-commands
        ))

(setq private-org-post-extensions
      '(
        ;; post extension private-orgs go here
        ))

;; For each extension, define a function private-org/init-<extension-private-org>
;;
(defun private-org/init-todo-keywords ()
  ; The @ means that there's going to need to be a note attached to the change.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-todo-keyword-faces
        '(("TODO" . "red")
          ("NEXT" . "yellow")
          ("WAITING" . "blue")
          ("DONE" . "green")
          ("CANCELLED" . "green")))
  )

(defun private-org/init-agenda-commands ()
  ; Next action list
  (setq org-agenda-custom-commands '(("n" todo "NEXT")))
  )

;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
