;;; packages.el --- my-defaults layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: danilo nascimentomento <danilo@cachyos-x8664>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or ;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-defaults-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-defaults/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-defaults/pre-init-PACKAGE' and/or
;;   `my-defaults/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-defaults-packages
  '(
    org-roam
    org
    ts-fold
    zoom
    )
  "The list of Lisp packages required by the my-defaults layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun size-callback ()
  (cond ((> (frame-pixel-width) 1280) '(0.65 . 0.75))
        (t                            '(0.5 . 0.5))))

(defun my-defaults/init-xclip ()
  (use-package! xclip
                :config
                (setq xclip-program "wl-copy")
                (setq xclip-select-enable-clipboard t)
                (setq xclip-mode t)
                (setq xclip-method (quote wl-copy)))

  (setq xclip-select-enable-clipboard t))


(defun my-defaults/init-zoom ()
  (use-package zoom
    :defer t
    :config
    ;; zoom-exclude-modes -> same which golden ration
    (dolist (modes '("bs-mode"
                     "calc-mode"
                     "ediff-mode"
                     "dired-mode"
                     "gud-mode"
                     "gdb-locals-mode"
                     "gdb-registers-mode"
                     "gdb-breakpoints-mode"
                     "gdb-threads-mode"
                     "gdb-frames-mode"
                     "gdb-inferior-io-mode"
                     "gdb-disassembly-mode"
                     "gdb-memory-mode"
                     "ranger-mode"
                     "speedbar-mode"))

      (add-to-list 'zoom-ignored-major-modes modes))
    (add-to-list 'zoom-ignored-buffer-name-regexps "^*[hH]elm.*")
    (custom-set-variables
     '(zoom-size 'size-callback))
    ))

(defun my-defaults/pre-init-ts-fold ()
  (spacemacs|use-package-add-hook ts-fold
    :post-init
    (add-hook 'prog-mode-hook (lambda ()
                                ;; TODO: Before enable ts-fold for a lang we need to check if treesitter is ready to analyze it
                                ;; (if (treesit-ready-p 'current-prog-mode))
                                (ts-fold-mode)))
    ))

(defun my-defaults/post-init-org-roam ()
  (setq org-roam-capture-templates '(("d" "default" plain
                                      "%?"
                                      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                      :unnarrowed t)
                                     ("l" "programming language" plain
                                      "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
                                      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                      :unnarrowed t))))

(defun my-defaults/pre-init-org ()
  ;; Configure TODO behavior
  ;; When using a hierarchical TODO, that is, a task that is linked to several subtasks (children),
  ;; it prevents this task from being moved to completed until all of its children are completed.
  ;; https://orgmode.org/org.html#TODO-dependencies-1
  ;; Do you need of a more complex dependencies structure? check out the module ‘org-depend.el’ in the ‘org-contrib’ repository.
  (setq org-enforce-todo-dependencies t)
  ;; Same idea as `org-enforce-todo-dependencies`, but for checkboxes
  (setq org-enforce-todo-checkbox-dependencies t)
  ;; Every time a task is changed to a "completed" state a "CLOSED" timestamp will be applied.
  ;; https://orgmode.org/org.html#Closing-items
  (setq org-log-done 'time)
  ;; Places all stamps and status change tracking notes for tasks in a drawer called "LOGBOOK"
  (setq org-log-into-drawer t)
  ;; Setup TODO keyswords
  (setq org-todo-keyword-faces
        '(
          ("TODO" . (:foreground "GoldenRod" :weight bold))
          ("PLANNING" . (:foreground "DeepPink" :weight bold))
          ("IN-PROGRESS" . (:foreground "Cyan" :weight bold))
          ("VERIFYING" . (:foreground "DarkOrange" :weight bold))
          ("BLOCKED" . (:foreground "Red" :weight bold))
          ("DONE" . (:foreground "LimeGreen" :weight bold))
          ("OBE" . (:foreground "LimeGreen" :weight bold))
          ("WONT-DO" . (:foreground "LimeGreen" :weight bold))
          ))
  ;; How to configure this variable -> https://orgmode.org/org.html#Tracking-TODO-state-changes
  ;;                                -> https://orgmode.org/org.html#Setting-up-keywords-for-individual-files
  (setq org-todo-keywords
        '((sequence "TODO(t)" "PLANNING(p)" "IN-PROGRESS(i@/!)" "VERIFYING(v!)" "BLOCKED(b@)"  "|" "DONE(d!)" "OBE(o@!)" "WONT-DO(w@/!)" ))
        )
  ;; Setup captures
  ;; TODO This loading model can be improved, to define org-capture-templates it is necessary that org-directory
  ;; is configured
  (spacemacs|use-package-add-hook org
    :post-config
    (setq org-capture-templates
          '(
            ("c" "Code To-Do"
             entry (file+headline "~/org/todos.org" "Code Related Tasks")
             "* TODO [#B] %?\n:Created: %T\n%i\n%a\nProposed Solution: "
             :empty-lines 0)
            ("j" "Work Log Entry"
             entry (file+datetree "~/org/work-todos.org" "Work entries")
             "* %? "
             :empty-lines 0)
            ("g" "General To-Do"
             entry (file+headline "~/org/todos.org" "General Tasks")
             "* TODO [#B] %?\n:Created: %T\n"
             :empty-lines 0)
            ("m" "Meeting"
             entry (file+datetree "~/org/meetings.org")
             "* %? :meeting:%^g \n:Created: %T\n** Attendees\n*** \n** Notes\n** Action Items\n*** TODO [#A] "
             :tree-type week
             :clock-in t
             :clock-resume t
             :empty-lines 0)
            )
          )
    ;; Sets the maximum width that each line must have in a paragraph when adjusted by org-fill-paragraph
    (add-hook 'org-mode-hook
              (lambda ()
                (setq-local fill-column 160))))

  (setq org-tag-alist '(;; Ticket types
                        (:startgroup . nil)
                        ("@bug" . ?b)
                        ("@feature" . ?u)
                        ("@spike" . ?j)
                        (:endgroup . nil)

                        ;; Ticket flags
                        ("@write_future_ticket" . ?w)
                        ("@emergency" . ?e)
                        ("@research" . ?r)

                        ;; Meeting types
                        (:startgroup . nil)
                        ("big_sprint_review" . ?i)
                        ("cents_sprint_retro" . ?n)
                        ("dsu" . ?d)
                        ("grooming" . ?g)
                        ("sprint_retro" . ?s)
                        (:endgroup . nil)

                        ;; Code TODOs tags
                        ("QA" . ?q)
                        ("backend" . ?k)
                        ("broken_code" . ?c)
                        ("frontend" . ?f)

                        ;; Special tags
                        ("CRITICAL" . ?x)
                        ("obstacle" . ?o)

                        ;; Meeting tags
                        ("HR" . ?h)
                        ("general" . ?l)
                        ("meeting" . ?m)
                        ("misc" . ?z)
                        ("planning" . ?p)

                        ;; Work Log Tags
                        ("accomplishment" . ?a)
                        ))
  ;; Must do this so the agenda knows where to look for my files
  (setq org-agenda-files '("~/org" "~/Documentos/org-roam" "~/.org-jira"))

  (setq org-jira-custom-jqls '(
                               (:jql "project = 'SFI' and assignee = currentUser() and status NOT IN ('CONCLUÍDO', 'Cancelado') ORDER BY created DESC"
                                     :limit 10
                                     :filename "squad-tasks")
                               (:jql "project = 'Chapter Backend' AND assignee = currentUser() AND status NOT IN (CONCLUÍDO, Cancelado, '✅ DONE', '❌ CANCELED') ORDER BY created DESC"
                                     :limit 10
                                     :filename "chapter-backend-tasks")
                               ))
  )
