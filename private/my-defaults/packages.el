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

(defconst my-defaults-packages
  '(
    org-roam
    org-mode-custom
    ts-fold
    ;; Mode used for eww config files
    yuck-mode
    zoom
    (combobulate :location local)
    xclip
    ;; (emacs-color-theme-solarized :location (recipe :fetcher github
    ;;                                                :repo "bbatsov/zenburn-emacs"))
    dirvish))

(defun my-defaults/init-yuck-mode ()
  (use-package yuck-mode
    :ensure t))

(defun my-defaults/init-combobulate ()
  (use-package combobulate
    :custom
    ;; You can customize Combobulate's key prefix here.
    ;; Note that you may have to restart Emacs for this to take effect!
    (combobulate-key-prefix "C-c o")
    :hook ((prog-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
    :load-path ("~/workspace/open-source-projects/combobulate")))

;; (defun my-defaults/init-emacs-color-theme-solarized ()
;;   (use-package emacs-color-theme-solarized
;;     :ensure t))

(defun my-defaults/init-dirvish ()
  (use-package dirvish
    :ensure t
    :init
    (dirvish-override-dired-mode)
    :custom
    (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
     '(("h" "~/"                          "Home")
       ("d" "~/Downloads/"                "Downloads")
       ("o" "/org" "Org files")
       ("w" "~/workspace/" "Workspace")))
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
  (use-package xclip
    :ensure t
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
     '(zoom-size (size-callback)))))

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

(defun my-defaults/post-init-org-mode-custom ()
  ;; ------------------------------------LATEX SETUP ---------------------------------------------------
  ;; Increase preview width
  (plist-put org-latex-preview-appearance-options
             :page-width 1.0)
  ;; :page-width 0.8)
  ;; Use native highlighting for =LaTeX=  related syntax in =org= buffers.
  ;; By using native highlighting the =org-face= gets added which we want to avoid.
  (setq org-highlight-latex-and-related '(native script))

  ;; ;; Use dvisvgm to generate previews
  ;; ;; You don't need this, it's the default:
  ;; (setq org-latex-preview-process-default 'dvisvgm)

  ;; Turn on auto-mode, it's built into Org and much faster/more featured than
  ;; org-fragtog. (Remember to turn off/uninstall org-fragtog.)
  (add-hook 'org-mode-hook 'org-latex-preview-auto-mode)
  ;; Enable `evil-tex-mode` in LaTeX source blocks. Requires latex and evil enabled
  (add-hook 'org-src-mode-hook
            (when (string= major-mode "latex-mode")
              (evil-tex-mode 1)))

  (dolist (pkg '("amsmath" "amssymb" "mathtools" "mathrsfs"))
    (add-to-list 'org-latex-packages-alist `("" ,pkg t)))

  ;; ;; Block C-n, C-p etc from opening up previews when using auto-mode
  ;; (setq org-latex-preview-auto-ignored-commands
  ;;       '(next-line previous-line mwheel-scroll
  ;;         scroll-up-command scroll-down-command))

  ;; Enable consistent equation numbering
  (setq org-latex-preview-numbered t)

  ;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
  ;; fragment and updates the preview in real-time as you edit it.
  ;; To preview only environments, set it to '(block edit-special) instead
  (setq org-latex-preview-live t)
  (setq org-latex-preview-numbered t)

  ;; More immediate live-previews -- the default delay is 1 second
  (setq org-latex-preview-live-debounce 0.25)

  (setq org-latex-preview-auto-ignored-commands
        '(next-line previous-line mwheel-scroll ultra-scroll
                    scroll-up-command scroll-down-command
                    evil-scroll-up evil-scroll-down evil-scroll-line-up evil-scroll-line-down))

  (defun my/org-latex-preview-uncenter (ov)
    (overlay-put ov 'before-string nil))
  (defun my/org-latex-preview-recenter (ov)
    (overlay-put ov 'before-string (overlay-get ov 'justify)))
  (defun my/org-latex-preview-center (ov)
    (save-excursion
      (goto-char (overlay-start ov))
      (when-let* ((elem (org-element-context))
                  ((or (eq (org-element-type elem) 'latex-environment)
                       (string-match-p "^\\\\\\[" (org-element-property :value elem))))
                  (img (overlay-get ov 'display))
                  (prop `(space :align-to (- center (0.55 . ,img))))
                  (justify (propertize " " 'display prop 'face 'default)))
        (overlay-put ov 'justify justify)
        (overlay-put ov 'before-string (overlay-get ov 'justify)))))
  (define-minor-mode org-latex-preview-center-mode
    "Center equations previewed with `org-latex-preview'."
    :global nil
    (if org-latex-preview-center-mode
        (progn
          (add-hook 'org-latex-preview-overlay-open-functions
                    #'my/org-latex-preview-uncenter nil :local)
          (add-hook 'org-latex-preview-overlay-close-functions
                    #'my/org-latex-preview-recenter nil :local)
          (add-hook 'org-latex-preview-overlay-update-functions
                    #'my/org-latex-preview-center nil :local))
      (remove-hook 'org-latex-preview-overlay-close-functions
                   #'my/org-latex-preview-recenter)
      (remove-hook 'org-latex-preview-overlay-update-functions
                   #'my/org-latex-preview-center)
      (remove-hook 'org-latex-preview-overlay-open-functions
                   #'my/org-latex-preview-uncenter)))
  ;; ------------------------------------LATEX SETUP ---------------------------------------------------

  ;; Sets the maximum width that each line must have in a paragraph when adjusted by org-fill-paragraph
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local fill-column 120)))
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
        '((sequence "TODO(t)" "PLANNING(p)" "IN-PROGRESS(i@/!)" "VERIFYING(v!)" "BLOCKED(b@)"  "|" "DONE(d!)" "OBE(o@!)" "WONT-DO(w@/!)" )))
  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t))
                ("WAITING" ("WAITING" . t))
                ("HOLD" ("WAITING") ("HOLD" . t))
                (done ("WAITING") ("HOLD"))
                ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
  ;; (setq org-capture-templates
  ;;       (quote (("t" "todo" entry (file "~/git/org/refile.org")
  ;;                "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
  ;;               ("r" "respond" entry (file "~/git/org/refile.org")
  ;;                "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
  ;;               ("n" "note" entry (file "~/git/org/refile.org")
  ;;                "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
  ;;               ("j" "Journal" entry (file+datetree "~/git/org/diary.org")
  ;;                "* %?\n%U\n" :clock-in t :clock-resume t)
  ;;               ("w" "org-protocol" entry (file "~/git/org/refile.org")
  ;;                "* TODO Review %c\n%U\n" :immediate-finish t)
  ;;               ("m" "Meeting" entry (file "~/git/org/refile.org")
  ;;                "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
  ;;               ("p" "Phone call" entry (file "~/git/org/refile.org")
  ;;                "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
  ;;               )))
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
            ("h" "Habit"
             entry (file "~/git/org/refile.org")
             "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
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
    ;; Do not dim blocked tasks
    (setq org-agenda-dim-blocked-tasks nil)

    ;; Compact the block agenda view
    (setq org-agenda-compact-blocks t)

    ;; Custom agenda command definitions
    (setq org-agenda-custom-commands
          (quote (("N" "Notes" tags "NOTE"
                   ((org-agenda-overriding-header "Notes")
                    (org-tags-match-list-sublevels t)))
                  ("h" "Habits" tags-todo "STYLE=\"habit\""
                   ((org-agenda-overriding-header "Habits")
                    (org-agenda-sorting-strategy
                     '(todo-state-down effort-up category-keep))))
                  ("b" "Agenda"
                   ((agenda "" nil)
                    (tags "REFILE"
                          ((org-agenda-overriding-header "Tasks to Refile")
                           (org-tags-match-list-sublevels nil)))
                    (tags-todo "-CANCELLED/!"
                               ((org-agenda-overriding-header "Stuck Projects")
                                (org-agenda-skip-function 'my-defaults//skip-non-stuck-projects)
                                (org-agenda-sorting-strategy
                                 '(category-keep))))
                    (tags-todo "-HOLD-CANCELLED/!"
                               ((org-agenda-overriding-header "Projects")
                                (org-agenda-skip-function 'my-defaults//skip-non-projects)
                                (org-tags-match-list-sublevels 'indented)
                                (org-agenda-sorting-strategy
                                 '(category-keep))))
                    (tags-todo "-CANCELLED/!NEXT"
                               ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                      (if my-defaults//hide-scheduled-and-waiting-next-tasks
                                                                          ""
                                                                        " (including WAITING and SCHEDULED tasks)")))
                                (org-agenda-skip-function 'my-defaults//skip-projects-and-habits-and-single-tasks)
                                (org-tags-match-list-sublevels t)
                                (org-agenda-todo-ignore-scheduled my-defaults//hide-scheduled-and-waiting-next-tasks)
                                (org-agenda-todo-ignore-deadlines my-defaults//hide-scheduled-and-waiting-next-tasks)
                                (org-agenda-todo-ignore-with-date my-defaults//hide-scheduled-and-waiting-next-tasks)
                                (org-agenda-sorting-strategy
                                 '(todo-state-down effort-up category-keep))))
                    (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                               ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                      (if my-defaults//hide-scheduled-and-waiting-next-tasks
                                                                          ""
                                                                        " (including WAITING and SCHEDULED tasks)")))
                                (org-agenda-skip-function 'my-defaults//skip-non-project-tasks)
                                (org-agenda-todo-ignore-scheduled my-defaults//hide-scheduled-and-waiting-next-tasks)
                                (org-agenda-todo-ignore-deadlines my-defaults//hide-scheduled-and-waiting-next-tasks)
                                (org-agenda-todo-ignore-with-date my-defaults//hide-scheduled-and-waiting-next-tasks)
                                (org-agenda-sorting-strategy
                                 '(category-keep))))
                    (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                               ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                      (if my-defaults//hide-scheduled-and-waiting-next-tasks
                                                                          ""
                                                                        " (including WAITING and SCHEDULED tasks)")))
                                (org-agenda-skip-function 'my-defaults//skip-project-tasks)
                                (org-agenda-todo-ignore-scheduled my-defaults//hide-scheduled-and-waiting-next-tasks)
                                (org-agenda-todo-ignore-deadlines my-defaults//hide-scheduled-and-waiting-next-tasks)
                                (org-agenda-todo-ignore-with-date my-defaults//hide-scheduled-and-waiting-next-tasks)
                                (org-agenda-sorting-strategy
                                 '(category-keep))))
                    (tags-todo "-CANCELLED+WAITING|HOLD/!"
                               ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                      (if my-defaults//hide-scheduled-and-waiting-next-tasks
                                                                          ""
                                                                        " (including WAITING and SCHEDULED tasks)")))
                                (org-agenda-skip-function 'my-defaults//skip-non-tasks)
                                (org-tags-match-list-sublevels nil)
                                (org-agenda-todo-ignore-scheduled my-defaults//hide-scheduled-and-waiting-next-tasks)
                                (org-agenda-todo-ignore-deadlines my-defaults//hide-scheduled-and-waiting-next-tasks)))
                    (tags "-REFILE/"
                          ((org-agenda-overriding-header "Tasks to Archive")
                           (org-agenda-skip-function 'my-defaults//skip-non-archivable-tasks)
                           (org-tags-match-list-sublevels nil))))
                   nil))))
    ;; Must do this so the agenda knows where to look for my files
    (setq org-agenda-files '("~/org" "~/Documentos/org-roam" "~/.org-jira" "~/org/rebase/infleet" "~/org/personal"))

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
