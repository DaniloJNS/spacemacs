;;; funcs.el --- my-defaults layer functions file for Spacemacs.
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
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; (defun my-defaults//my-evil-treemacs-binds()
;;   ((evil-define-key 'treemacs treemacs-mode-map (kbd "C-l") #'treemacs-RET-action)))

(defun toggle-fold ()
  (interactive)
  (save-excursion
    (end-of-line)
    (ts-fold-toggle)))

(defun size-callback ()
  (cond ((> (frame-pixel-width) 1280) '(0.5 . 0.75))
        (t                            '(0.5 . 0.5))))

;;------------------------------
;; Org mode
(defun my-defaults/text-scale-adjust-latex-previews ()
  "Adjust the size of latex preview fragments when changing the
buffer's text scale."
  (pcase major-mode
    ('latex-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (if (eq (overlay-get ov 'category)
               'preview-overlay)
           (my-defaults/text-scale--resize-fragment ov))))
    ('org-mode
     (dolist (ov (overlays-in (point-min) (point-max)))
       (if (eq (overlay-get ov 'org-overlay-type)
               'org-latex-overlay)
           (my-defaults/text-scale--resize-fragment ov))))))

(defun my-defaults/text-scale--resize-fragment (ov)
  (overlay-put
   ov 'display
   (cons 'image
         (plist-put
          (cdr (overlay-get ov 'display))
          :scale (+ 1.0 (* 0.25 text-scale-mode-amount))))))

(add-hook 'text-scale-mode-hook #'my-defaults/text-scale-adjust-latex-previews)


;; --------------------------------
;; Window purpose layout to Coding

(defun my-defaults//toggle-vterm-terminal ()
  (interactive)
  (if (projectile-project-p) (call-interactively 'spacemacs/projectile-shell-pop) (spacemacs/shell-pop-vterm)))

(with-eval-after-load 'window-purpose
  (defvar my-defaults-x-code1--window-layout
    '(nil
      (0 0 255 59)
      (t
       (0 0 40 59)
       (:purpose dired :purpose-dedicated t :width 0.15810276679841898 :height 0.5166666666666667
                 :edges (0.0 0.0 0.15810276679841898 0.5166666666666667))
       (:purpose buffers :purpose-dedicated t :width 0.15810276679841898 :height 0.4666666666666667
                 :edges (0.0 0.5166666666666667 0.15810276679841898 0.9833333333333333)))
      (:purpose edit :purpose-dedicated t :width 0.6996047430830039 :height 0.9833333333333333
                :edges (0.15810276679841898 0.0 0.857707509881423 0.9833333333333333))
      (t
       (217 0 255 59)
       (:purpose ilist :purpose-dedicated t :width 0.15019762845849802 :height 0.5166666666666667
                 :edges (0.857707509881423 0.0 1.007905138339921 0.5166666666666667))
       (:purpose lsp-error-list :purpose-dedicated nil :width 0.15019762845849802 :height 0.4666666666666667
                 :edges (0.857707509881423 0.5166666666666667 1.007905138339921 0.9833333333333333))))
    "Window layout for my-defaults-x-code1-dired-ibuffer.
                  Has a main `edit' window, and four side windows - `dired' and `buffers' at left.
                  `imenu-list' and `Lsp Error List' at right
                  All windows are purpose-dedicated.")

  ;; the name arg ("my-defaults-x-code1") is necessary for Emacs 24.5 and older
  ;; (omitting it produces an "Invalid slot name" error)
  (defvar my-defaults-x-code1-purpose-config
    (purpose-conf :mode-purposes
                  '((ibuffer-mode . buffers)
                    (dired-mode . dired)
                    (imenu-list-major-mode . ilist)
                    )
                  :regexp-purposes
                  '((".*Error List.*" . lsp-error-list))))

  (defvar my-defaults-x-code1-buffers-changed nil
    "Internal variable for use with `frame-or-buffer-changed-p'."))

(defun my-defaults-x-code1--setup-lsp-diagnostics-issues ()
  (save-selected-window
    (-if-let (buffer (get-buffer lsp-treemacs-errors-buffer-name))
        (progn
          (lsp-treemacs-errors-list--refresh))
      (let* ((buffer (lsp-treemacs-errors-list--refresh)))
        (setq lsp-treemacs--current-workspaces (lsp-workspaces))
        (add-hook 'lsp-diagnostics-updated-hook #'lsp-treemacs-errors-list--refresh)
        (add-hook 'kill-buffer-hook 'lsp-treemacs--kill-buffer nil t)
        (bury-buffer buffer)
        (lsp-treemacs-error-list-mode 1)))))

(defun my-defaults-x-code1--setup-ibuffer ()
  "Set up ibuffer settings."
  ;; (add-hook 'ibuffer-mode-hook
  ;;           (lambda ()
  ;;             (ibuffer-filter-by-my-defaults-x-code1-ibuffer-files-only nil)))

  (add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
  (setq ibuffer-formats '((mark " " name)))
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-header-line nil)
  ;; not sure if we want this...
  ;; (setq ibuffer-default-shrink-to-minimum-size t)
  (when (get-buffer "*Ibuffer*")
    (kill-buffer "*Ibuffer*"))
  (save-selected-window
    (projectile-ibuffer nil)))

(defun my-defaults-x-code1--unset-ibuffer ()
  "Unset ibuffer settings."
  (remove-hook 'ibuffer-mode-hook
               (lambda ()
                 (ibuffer-filter-by-my-defaults-x-code1-ibuffer-files-only nil)))
  (remove-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)
  (setq ibuffer-formats '((mark modified read-only " "
                                (name 18 18 :left :elide)
                                " "
                                (size 9 -1 :right)
                                " "
                                (mode 16 16 :left :elide)
                                " " filename-and-process)
                          (mark " "
                                (name 16 -1)
                                " " filename)))
  (setq ibuffer-display-summary t)
  (setq ibuffer-use-header-line t))

(defun my-defaults-x-code1-update-dired ()
  "Update free dired window with current buffer's directory.
                  If a non-buffer-dedicated window with my-defaults `dired' exists,
                  display the directory of the current buffer in that window, using
                  `dired'.  If there is no window available, do nothing.  If
                  current buffer doesn't have a filename, do nothing."
  (when (and (buffer-file-name)
             (cl-delete-if #'window-dedicated-p
                           (purpose-windows-with-purpose 'dired)))
    (save-selected-window
      (let ((buffer (dired-noselect (file-name-directory (buffer-file-name)))))
        (with-current-buffer buffer
          (when (fboundp 'dired-hide-details-mode)
            (dired-hide-details-mode)))
        (display-buffer buffer))
      (bury-buffer (current-buffer)))))

(defun my-defaults-x-code1-update-changed ()
  "Update auxiliary buffers if frame/buffer had changed.
                  Uses `frame-or-buffer-changed-p' to determine whether the frame or
                  buffer was changed."
  (when (frame-or-buffer-changed-p 'my-defaults-x-code1-buffers-changed)
    (my-defaults-x-code1-update-dired)
    (imenu-list-update)))

(defun my-defaults-x-code1-lock-side-windows ()
  "Freezes the area of ​​the side windows of the layout, preventing them from being
   accidentally resized by packages like 'zoom or 'golden-ratio"
  (dolist (purpose '(dired buffers ilist lsp-error-list))
    ;; Gets first window from list returned`purpose-windows-with-purpose',
    ;; The sintax bellow destructuring an list, like javascript -> head, tail = [1, 2, 3]
    (-if-let (win (car (purpose-windows-with-purpose purpose)))
        (with-selected-window win
          (setq window-size-fixed 'width)))))

(defun my-defaults-x-code1-setup ()
  "Setup my-defaults-x-code1.
                  This setup includes 4 windows:
                  1. dedicated `edit' window
                  2. dedicated `dired' window.  This window shows the current buffer's
                  directory in a special window, using `dired' and
                  `dired-hide-details-mode' (if available).
                  3. dedicated `buffers' window.  This window shows the currently open
                  files, using `ibuffer'.
                  4. dedicated `ilist' window.  This window shows the current buffer's
                  imenu."
  (interactive)
  (purpose-set-extension-configuration :my-defaults-x-code1 my-defaults-x-code1-purpose-config)
  (my-defaults-x-code1--setup-ibuffer)
  (my-defaults-x-code1-update-dired)
  (imenu-list-minor-mode)
  (my-defaults-x-code1--setup-lsp-diagnostics-issues)
  (frame-or-buffer-changed-p 'my-defaults-x-code1-buffers-changed)
  (add-hook 'post-command-hook #'my-defaults-x-code1-update-changed)
  (purpose-set-window-layout my-defaults-x-code1--window-layout)
  (my-defaults-x-code1-lock-side-windows))

(defun my-defaults//find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun my-defaults//is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun my-defaults//is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (my-defaults//find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun my-defaults//is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun my-defaults//is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun my-defaults//list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun my-defaults//list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar my-defaults//hide-scheduled-and-waiting-next-tasks t)

(defun my-defaults//toggle-next-task-display ()
  (interactive)
  (setq my-defaults//hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if my-defaults//hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun my-defaults//skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (my-defaults//is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun my-defaults//skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (my-defaults//list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (my-defaults//is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun my-defaults//skip-non-projects ()
  "Skip trees that are not projects"
  ;; (my-defaults//list-sublevels-for-projects-indented)
  (if (save-excursion (my-defaults//skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((my-defaults//is-project-p)
            nil)
           ((and (my-defaults//is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun my-defaults//skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((my-defaults//is-task-p)
        nil)
       (t
        next-headline)))))

(defun my-defaults//skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((my-defaults//is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun my-defaults//skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and my-defaults//hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((my-defaults//is-project-p)
        next-headline)
       ((and (my-defaults//is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun my-defaults//skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((my-defaults//is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (my-defaults//is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (my-defaults//is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun my-defaults//skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((my-defaults//is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((my-defaults//is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun my-defaults//skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((my-defaults//is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (my-defaults//is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (my-defaults//is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun my-defaults//skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((my-defaults//is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun my-defaults//skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (my-defaults//is-subproject-p)
        nil
      next-headline)))
