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
        clockers
        appearance
        archive
        todo-keywords
        capture
        refile
        agenda-commands
        custom-keys
        files
        ))

(setq private-org-post-extensions
      '(
        ))


(defun private-org/init-clockers ()
  (global-set-key (kbd "<f5>") 'org-clock-in)
  (global-set-key (kbd "<f6>") 'org-clock-out)
  )

(defun private-org/init-appearance ()
  (setq org-bullets-bullet-list '("*" "**" "***" "****"))
  ; Turn off org-indent-mode
  (with-eval-after-load 'org (setq org-startup-indented nil))
  )

(defun private-org/init-archive ()
  (setq org-archive-location "archive/%s::")
  )

;; For each extension, define a function private-org/init-<extension-private-org>
;;
(defun private-org/init-todo-keywords ()
  ; The @ means that there's going to need to be a note attached to the change.
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "NEXT(n!)" "STARTED(s!)" "WAITING(w!)" "READY(r!)" "|" "DONE(d!)" "CANCELLED(c!)")))
  (setq org-todo-keyword-faces
        '(("TODO" . "red")
          ("NEXT" . "orange")
          ("STARTED" . "orange")
          ("WAITING" . "blue")
          ("READY" . "brown")
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
           "** %^{Tickle item}\n SCHEDULED: %^t")
          ("e" "Euler problem" entry (file+headline "~/workflow/current-project.org" "Projects")
           "** Euler Problem %^{problem}\n*** TODO Solve problem %\\1 in a naive way.\n*** TODO Write up on the naive solution of problem %\\1\n*** TODO Solve problem %\\1 in a better way.\n*** TODO Write the tests for problem %\\1\n*** TODO write up on the better solution of problem %\\1")
          ))
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
           ("w" "WAITING actions"
            (
             (todo "WAITING")
             (agenda "")
             )
           )
           ("n" "All next actions and agenda and waiting"
            (
             (todo "STARTED|NEXT")
             (agenda "")
            )
           )
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

(defun new-header ()
  (interactive)
  (org-insert-heading-after-current)
  (evil-insert-state)
  )

(defun private-org/init-custom-keys ()

  (evil-leader/set-key
    "oe" 'new-people-entry)
  (evil-leader/set-key
    "hh" 'new-header)
  (print "Test")
  )

(defun private-org/init-files ()
  (setq org-agenda-files '("~/workflow" "~/workflow/projects"))
  )


;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
