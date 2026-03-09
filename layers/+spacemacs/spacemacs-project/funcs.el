;;; funcs.el --- Spacemacs Project Management Layer packages File  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Codruț Constantin Gușoi <codrut.gusoi@gmail.com>
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


(defun spacemacs--projectile-directory-path ()
  "Retrieve the directory path relative to project root.

If the buffer is not visiting a file, use the `list-buffers-directory'
variable as a fallback to display the directory, useful in buffers like the
ones created by `magit' and `dired'.

Returns:
  - A string containing the directory path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let* ((directory-name (if-let* ((file-name (buffer-file-name)))
                                  (file-name-directory file-name)
                                list-buffers-directory)))
    (file-relative-name
     (file-truename directory-name)
     (projectile-project-root))))

(defun spacemacs/projectile-project-root (&optional dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (let ((projectile-project-root
         (unless dir (bound-and-true-p projectile-project-root)))
        projectile-require-project-root)
    (projectile-project-root dir)))

(defun spacemacs/projectile-project-p (&optional dir)
  "Return t if DIR (defaults to `default-directory') is a valid project."
  (and (spacemacs/projectile-project-root dir)
       t))

(defun spacemacs--projectile-file-path ()
  "Retrieve the file path relative to project root.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
  (when-let* ((file-name (buffer-file-name)))
    (file-relative-name (file-truename file-name) (projectile-project-root))))

(defun spacemacs--projectile-file-path-with-line ()
  "Retrieve the file path relative to project root, including line number.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
  (when-let* ((file-path (spacemacs--projectile-file-path)))
    (concat file-path ":" (number-to-string (line-number-at-pos)))))

(defun spacemacs--projectile-file-path-with-line-column ()
  "Retrieve the file path relative to project root, including line and column number.

This function respects the `column-number-indicator-zero-based' value.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
  (when-let* ((file-path (spacemacs--projectile-file-path-with-line)))
    (format "%s:%s" file-path
            (+ (current-column) (if column-number-indicator-zero-based 0 1)))))


(defun spacemacs/projectile-copy-directory-path ()
  "Copy and show the directory path relative to project root.

If the buffer is not visiting a file, use the `list-buffers-directory'
variable as a fallback to display the directory, useful in buffers like the
ones created by `magit' and `dired'."
  (interactive)
  (if-let* ((directory-path (spacemacs--projectile-directory-path)))
      (progn
        (kill-new directory-path)
        (message "%s" directory-path))
    (message "WARNING: Current buffer does not have a directory!")))

(defun spacemacs/projectile-copy-file-path ()
  "Copy and show the file path relative to project root."
  (interactive)
  (if-let* ((file-path (spacemacs--projectile-file-path)))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not visiting a file!")))

(defun spacemacs/projectile-copy-file-path-with-line ()
  "Copy and show the file path relative to project root, including line number."
  (interactive)
  (if-let* ((file-path (spacemacs--projectile-file-path-with-line)))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not visiting a file!")))

(defun spacemacs/projectile-copy-file-path-with-line-column ()
  "Copy and show the file path relative to project root, including line and column number.

This function respects the value of the `column-number-indicator-zero-based'
variable."
  (interactive)
  (if-let* ((file-path (spacemacs--projectile-file-path-with-line-column)))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not visiting a file!")))


;; Toggles

;; File to save project state
(defvar spacemacs/lsp-projects-state-file
  (expand-file-name "lsp-disabled-projects" user-emacs-directory)
  "File where the state of projects with LSP disabled is saved.")

;; Variable to track projects with LSP disabled
(defvar spacemacs/lsp-disabled-projects nil
  "List of project root directories where LSP is disabled.")

;; Load saved state
(defun spacemacs/lsp-load-disabled-projects ()
  "Load list of projects with LSP disabled from file."
  (when (file-exists-p spacemacs/lsp-projects-state-file)
    (with-temp-buffer
      (insert-file-contents spacemacs/lsp-projects-state-file)
      (setq spacemacs/lsp-disabled-projects
            (read (current-buffer))))))

;; Save current state
(defun spacemacs/lsp-save-disabled-projects ()
  "Save list of projects with LSP disabled to file."
  (with-temp-file spacemacs/lsp-projects-state-file
    (prin1 spacemacs/lsp-disabled-projects (current-buffer))))

;; Load state on startup
(spacemacs/lsp-load-disabled-projects)

(defun spacemacs/project-root ()
  "Return the root directory of the current project using projectile."
  (and (fboundp 'projectile-project-root)
       (projectile-project-root)))

(defun spacemacs/lsp-enabled-for-project-p ()
  "Check if LSP is enabled for the current project."
  (let ((root (spacemacs/project-root)))
    (and root (not (member root spacemacs/lsp-disabled-projects)))))

(defun spacemacs/get-project-buffers ()
  "Return list of buffers belonging to the current project."
  (let ((root (spacemacs/project-root)))
    (when root
      (seq-filter
       (lambda (buf)
         (when-let ((file (buffer-file-name buf)))
           (string-prefix-p root (expand-file-name file))))
       (buffer-list)))))

(defun spacemacs/disable-lsp-for-project ()
  "Disable LSP for the current project."
  (interactive)
  (let ((root (spacemacs/project-root)))
    (unless root
      (user-error "Not in a projectile project"))

    (add-to-list 'spacemacs/lsp-disabled-projects root)
    (spacemacs/lsp-save-disabled-projects)


    ;; Turn off LSP in all project buffers
    (dolist (buf (spacemacs/get-project-buffers))
      (with-current-buffer buf
        (when (bound-and-true-p lsp-mode)
          (condition-case err
              (lsp-disconnect)
            (error
             (lsp-mode -1)
             (message "Warning: %s" (error-message-string err)))))))

    ;; Remove LSP workspace for the project
    (condition-case nil
        (when (fboundp 'lsp-workspace-folders-remove)
          (lsp-workspace-folders-remove root))
      (error nil))

    (message "LSP disabled for project: %s" root)))

(defun spacemacs/enable-lsp-for-project ()
  "Enable LSP for the current project."
  (interactive)
  (let ((root (spacemacs/project-root)))
    (unless root
      (user-error "Not in a projectile project"))

    (setq spacemacs/lsp-disabled-projects
          (delete root spacemacs/lsp-disabled-projects))
    (spacemacs/lsp-save-disabled-projects)

    ;; Add workspace folder if needed
    (when (fboundp 'lsp-workspace-folders-add)
      (lsp-workspace-folders-add root))

    ;; Activate LSP in all project buffers
    (dolist (buf (spacemacs/get-project-buffers))
      (with-current-buffer buf
        (when (and (buffer-file-name)
                   (not (bound-and-true-p lsp-mode)))
          (lsp-deferred))))

    (message "LSP enabled for project: %s" root)))

(defun spacemacs/toggle-lsp-for-project ()
  "Toggle LSP on/off for the current project."
  (interactive)
  (if (spacemacs/lsp-enabled-for-project-p)
      (spacemacs/disable-lsp-for-project)
    (spacemacs/enable-lsp-for-project)))

;; Advice to prevent LSP from starting in disabled projects
(defun spacemacs/lsp-check-project-enabled-advice (orig-fun &rest args)
  "Prevent LSP from starting if the project is disabled."
  (if (and (spacemacs/project-root)
           (not (spacemacs/lsp-enabled-for-project-p)))
      (message "LSP is disabled for this project")
    (apply orig-fun args)))

(advice-add 'lsp :around #'spacemacs/lsp-check-project-enabled-advice)
(advice-add 'lsp-deferred :around #'spacemacs/lsp-check-project-enabled-advice)

;; Custom keymap
                                        ; (defvar my/lsp-project-map (make-sparse-keymap)
                                        ;   "Keymap for controlling LSP per project.")
                                        ;
                                        ; (define-key spacemacs/lsp-project-map (kbd "t") #'spacemacs/toggle-lsp-for-project)
                                        ; (define-key spacemacs/lsp-project-map (kbd "e") #'spacemacs/enable-lsp-for-project)
                                        ; (define-key spacemacs/lsp-project-map (kbd "d") #'spacemacs/disable-lsp-for-project)

;; Alternativa: usar com projectile commander
;; (def-projectile-commander-method ?l
;;   "Toggle LSP for project"
;;   (spacemacs/toggle-lsp-for-project))
