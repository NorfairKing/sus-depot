(setq org-bullets-bullet-list '("*" "**" "***" "****"))
; Turn off org-indent-mode
(with-eval-after-load 'org (setq org-startup-indented nil))

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

(setq org-directory "~/workflow/")
(setq org-default-notes-file (concat org-directory "/inbox.txt"))
(setq org-agenda-custom-commands
       '(
         ("f" "Work actions"
          (
           (tags-todo "work/STARTED|NEXT|WAITING")
           (agenda "")
           )
         )
         ("w" "WAITING actions"
          (
           (todo "WAITING")
           (agenda "")
           )
         )
         ("n" "All next actions and agenda"
          (
           (todo "STARTED|NEXT")
           (agenda "")
          )
         )
        )
   )

(setq org-agenda-files 
    '("~/workflow-old/batch"
      "~/workflow-old/projects"
      "~/workflow-old/projects/bounties"
      "~/workflow-old/projects/conf"
      "~/workflow-old/projects/fpco"
      "~/workflow-old/projects/interview"
      "~/workflow-old/projects/reports"
      "~/workflow-old/someday"
      "~/workflow-old/reference"))

(setq org-duration-format 'h:mm)
