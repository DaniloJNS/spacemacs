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
    dirvish)
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
  (cond ((> (frame-pixel-width) 1280) '(0.5 . 0.75))
        (t                            '(0.5 . 0.5))))

(defun my-defaults/init-dirvish ()
  (use-package dirvish
    :ensure t
    :init
    (dirvish-override-dired-mode)
    :custom
    (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
     '(("h" "~/"                          "Home")
       ("d" "~/Downloads/"                "Downloads")
       ("m" "/mnt/"                       "Drives")
       ("s" "/ssh:my-remote-server")      "SSH server"
       ("e" "/sudo:root@localhost:/etc")  "Modify program settings"
       ("t" "~/.local/share/Trash/files/" "TrashCan")))
    :config
    (require 'nerd-icons)
    ;; (dirvish-peek-mode)             ; Preview files in minibuffer
    ;; (dirvish-side-follow-mode)      ; similar to `treemacs-follow-mode'
    (setq dirvish-mode-line-format
          '(:left (sort symlink) :right (omit yank index)))
    (setq dirvish-attributes           ; The order *MATTERS* for some attributes
          '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
          dirvish-side-attributes
          '(vc-state nerd-icons collapse file-size))
    (setq dirvish-subtree-state-style 'nerd)
    (setq delete-by-moving-to-trash t)
    (setq dired-listing-switches
          "-l --almost-all --human-readable --no-group")
    (setq dirvish-path-separators (list
                                   (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                                   (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                                   (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
    ;; this command is useful when you want to close the window of `dirvish-side'
    ;; automatically when opening a file
    (put 'dired-find-alternate-file 'disabled nil)

    (evil-define-key 'normal dirvish-mode-map (kbd ";") #'dired-up-directory)
    (evil-define-key 'normal dirvish-mode-map (kbd "?") #'dirvish-dispatch)
    (evil-define-key 'normal dirvish-mode-map (kbd "a") #'dirvish-setup-menu)
    (evil-define-key 'normal dirvish-mode-map (kbd "f") #'dirvish-file-info-menu)
    (evil-define-key 'normal dirvish-mode-map (kbd "o") #'dirvish-quick-access)
    (evil-define-key 'normal dirvish-mode-map (kbd "s") #'dirvish-quicksort)
    (evil-define-key 'normal dirvish-mode-map (kbd "r") #'dirvish-history-jump)
    (evil-define-key 'normal dirvish-mode-map (kbd "l") #'dirvish-ls-switches-menu)
    (evil-define-key 'normal dirvish-mode-map (kbd "*") #'dirvish-mark-menu)
    (evil-define-key 'normal dirvish-mode-map (kbd "y") #'dirvish-yank-menu)
    (evil-define-key 'normal dirvish-mode-map (kbd "N") #'dirvish-narrow)
    (evil-define-key 'normal dirvish-mode-map (kbd "^") #'dirvish-history-last)
    (evil-define-key 'normal dirvish-mode-map (kbd "TAB") #'dirvish-subtree-toggle)
    (evil-define-key 'normal dirvish-mode-map (kbd "M-f") #'dirvish-history-go-forward)
    (evil-define-key 'normal dirvish-mode-map (kbd "M-f") #'dirvish-history-go-backward)
    (evil-define-key 'normal dirvish-mode-map (kbd "M-t") #'dirvish-layout-toggle)
    (evil-define-key 'normal dirvish-mode-map (kbd "M-e") #'dirvish-emerge-menu)
    (evil-define-key 'normal dirvish-mode-map (kbd "J") #'dirvish-fd)
    (evil-define-key 'normal dirvish-mode-map (kbd "C-f") #'dirvish-fd)
    :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
    (("C-c f" . dirvish)
     :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
     (";"   . dired-up-directory)        ; So you can adjust `dired' bindings here
     ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
     ("a"   . dirvish-setup-menu)        ; [a]ttributes settings: press `a' + `t' toggles mtime, etc.
     ("f"   . dirvish-file-info-menu)    ; [f]ile info
     ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
     ("s"   . dirvish-quicksort)         ; [s]ort flie list
     ("r"   . dirvish-history-jump)      ; [r]ecent visited
     ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
     ("*"   . dirvish-mark-menu)
     ("y"   . dirvish-yank-menu)
     ("N"   . dirvish-narrow)
     ("^"   . dirvish-history-last)
     ("TAB" . dirvish-subtree-toggle)
     ("M-f" . dirvish-history-go-forward)
     ("M-b" . dirvish-history-go-backward)
     ("M-t" . dirvish-layout-toggle)
     ("M-e" . dirvish-emerge-menu))))

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
    (add-to-list 'zoom-ignored-buffer-name-regexps "^*[tT]reemacs.*")
    (custom-set-variables
     '(zoom-size 'size-callback))
    ))

(defun show-buffer-name()
  (interactive)
  (message "Buffer: %s" (current-buffer)))

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
  ;; Sets the maximum width that each line must have in a paragraph when adjusted by org-fill-paragraph
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local fill-column 160)))
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
    (setq org-agenda-files '("~/org" "~/Documentos/org-roam" "~/.org-jira" "~/org/rebase/infleet/video_streaming.org" "~/org/personal"))

    (setq org-jira-custom-jqls '(
                                 (:jql "project = 'SFI' and assignee = currentUser() and status NOT IN ('CONCLUÍDO', 'Cancelado') ORDER BY created DESC"
                                       :limit 10
                                       :filename "squad-tasks")
                                 (:jql "project = 'Chapter Backend' AND assignee = currentUser() AND status NOT IN (CONCLUÍDO, Cancelado, '✅ DONE', '❌ CANCELED') ORDER BY created DESC"
                                       :limit 10
                                       :filename "chapter-backend-tasks")
                                 ))

    ;; Allow refile to create parent tasks with confirmation
    (setq org-refile-allow-creating-parent-nodes (quote confirm))
                                        ; Targets include this file and any file contributing to the agenda - up to 9 levels deep
    (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                     (org-agenda-files :maxlevel . 9))))
                                        ; Use full outline paths for refile targets - we file directly with IDO
    (setq org-refile-use-outline-path t)
                                        ; Targets complete directly with IDO
    (setq org-outline-path-complete-in-steps nil)
                                        ; Use the current window for indirect buffer display
    (setq org-indirect-buffer-display 'current-window)

    ;; Refile settings
    ;; Exclude DONE state tasks from refile targets
    (defun my-defaults/verify-refile-target ()
      "Exclude todo keywords with a done state from refile targets"
      (not (member (nth 2 (org-heading-components)) org-done-keywords)))
    (setq org-refile-target-verify-function 'my-defaults/verify-refile-target)

    ;; TODO Define exclusive tags from agenda view (they should be tags that indicate tasks that are blocked) and add them to the function
    (defun my-defaults/org-auto-exclude-function (tag)
      "Automatic task exclusion in the agenda views with org-agenda-filter-by-tag"
      (message "Will be rececived tag %s" tag)
      (and (cond
            ((string= tag "@bug")
             t)
            ((string= tag "farm")
             t))
           (concat "-" tag)))

    (setq org-agenda-auto-exclude-function 'my-defaults/org-auto-exclude-function)

    ;; Resume clocking task when emacs is restarted
    (org-clock-persistence-insinuate)
    ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
    (setq org-clock-history-length 23)
    ;; Resume clocking task on clock-in if the clock is open
    (setq org-clock-in-resume t)
    ;; Change tasks to NEXT when clocking in
    ;; (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
    ;; Separate drawers for clocking and logs
    (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
    ;; Save clock data and state changes and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t)
    ;; Clock out when moving task to a done state
    (setq org-clock-out-when-done t)
    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (setq org-clock-persist t)
    ;; Do not prompt to resume an active clock
    (setq org-clock-persist-query-resume nil)
    ;; Enable auto clock resolution for finding open clocks
    (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
    ;; Include current clocking task in clock reports
    (setq org-clock-report-include-clocking-task t)
    )
  )
