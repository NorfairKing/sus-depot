;; extensions.el --- private-org Layer extensions File for Spacemacs
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
        appearance
        todo-keywords
        capture
        refile
        agenda-commands
        custom-keys
        ))

(setq private-org-post-extensions
      '(
        ))

(defun private-org/init-appearance ()
  (setq org-bullets-bullet-list '("▶" "►" "▸" "·"))
  )

;; For each extension, define a function private-org/init-<extension-private-org>
;;
(defun private-org/init-todo-keywords ()
  ; The @ means that there's going to need to be a note attached to the change.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "STARTED(s)" "WAITING(w@)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-todo-keyword-faces
        '(("TODO" . "red")
          ("NEXT" . "yellow")
          ("STARTED" . "yellow")
          ("WAITING" . "blue")
          ("DONE" . "green")
          ("CANCELLED" . "green")))
  )

(defun private-org/init-capture ()
  (setq org-directory "~/workflow/")
  (setq org-default-notes-file (concat org-directory "/inbox.txt"))

  (setq org-capture-templates
        '(
          ("i" "Inbox" entry (file "~/workflow/inbox.org" )
           "* In: %^{Stuff}")
          ("p" "Project" entry (file "~/workflow/inbox.org")
           "* %^{Project name}\n** NEXT %^{Next action}\n" :prepend t)
          ("t" "Tickler" entry (file+headline "~/workflow/tickler.org" "Near future")
           "** %^{Tickle item}\n SCHEDULED: %^t")))
  )

(defun private-org/init-refile ()
  ; Targets include this file and any file contributing to the agenda - up to 9 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))

  ; Use full outline paths for refile targets - we file directly with IDO
  (setq org-refile-use-outline-path t)

  ; Targets complete directly with IDO
  (setq org-outline-path-complete-in-steps nil)

  ; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm))

  ; Use IDO for both buffer and file completion and ido-everywhere to t
  (setq org-completion-use-ido t)
  (setq ido-everywhere t)
  (setq ido-max-directory-size 100000)
  (ido-mode (quote both))
  ; Use the current window when visiting files and buffers with ido
  (setq ido-default-file-method 'selected-window)
  (setq ido-default-buffer-method 'selected-window)
  ; Use the current window for indirect buffer display
  (setq org-indirect-buffer-display 'current-window)

  ;;;; Refile settings
  ; Exclude DONE state tasks from refile targets
  (defun bh/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  (setq org-refile-target-verify-function 'bh/verify-refile-target)
  )

(defun private-org/init-agenda-commands ()
  ; Next action list
  (setq org-agenda-custom-commands
         '(
           ("w" "Next actions and work agenda"
            (
             (org-agenda-files '("~/workflow/work.org"))
             (todo "STARTED|NEXT" ((org-agenda-files '("~/workflow/work.org")) (org-agenda-text-search-extra-files nil)))
             (agenda "")
             )
           )
           ("n" "All next actions and agenda"
            ((todo "STARTED|NEXT")
             (agenda "")))
           )
     )
  )

(defun new-people-entry ()
  "Insert new entry"
  (interactive)
  (insert "\n")
  (insert "** ")
  (org-insert-time-stamp (current-time))
  (insert "\n")
  (insert "- "))

(defun private-org/init-custom-keys ()

  (evil-leader/set-key
    "oe" 'new-people-entry
    )
  (print "Test")
  )

;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
