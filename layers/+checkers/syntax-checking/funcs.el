;;; funcs.el --- Syntax Checking Layer functions File for Spacemacs  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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


(defun spacemacs/enable-flycheck (mode)
  "Use flycheck in MODE by default, if `syntax-checking-enable-by-default' t."
  (when (and syntax-checking-enable-by-default
             (listp flycheck-global-modes)
             (not (eq 'not (car flycheck-global-modes))))
    (add-to-list 'flycheck-global-modes mode)))

;; toggle flycheck window
(defun spacemacs/toggle-flycheck-error-list ()
  "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (if-let* ((window (flycheck-get-error-list-window)))
      (save-selected-window (quit-window nil window))
    (flycheck-list-errors)))

(defun spacemacs/goto-flycheck-error-list ()
  "Open and go to the error list buffer."
  (interactive)
  (if (flycheck-get-error-list-window)
      (switch-to-buffer flycheck-error-list-buffer)
    (flycheck-list-errors)
    (switch-to-buffer-other-window flycheck-error-list-buffer)))


;; TOGGLE

;; File to save project state
(defvar spacemacs/flycheck-projects-state-file
  (expand-file-name "flycheck-disabled-projects" user-emacs-directory)
  "File where the state of projects with Flycheck disabled is saved.")

;; Variable to track projects with Flycheck disabled
(defvar spacemacs/flycheck-disabled-projects nil
  "List of project root directories where Flycheck is disabled.")

;; Load saved state
(defun spacemacs/flycheck-load-disabled-projects ()
  "Load list of projects with Flycheck disabled from file."
  (when (file-exists-p spacemacs/flycheck-projects-state-file)
    (with-temp-buffer
      (insert-file-contents spacemacs/flycheck-projects-state-file)
      (setq spacemacs/flycheck-disabled-projects
            (read (current-buffer))))))

;; Save current state
(defun spacemacs/flycheck-save-disabled-projects ()
  "Save list of projects with Flycheck disabled to file."
  (with-temp-file spacemacs/flycheck-projects-state-file
    (prin1 spacemacs/flycheck-disabled-projects (current-buffer))))

;; Load state on startup
(spacemacs/flycheck-load-disabled-projects)

(defun spacemacs/project-root ()
  "Return the root directory of the current project using projectile."
  (and (fboundp 'projectile-project-root)
       (projectile-project-root)))

(defun spacemacs/flycheck-enabled-for-project-p ()
  "Check if Flycheck is enabled for the current project."
  (let ((root (spacemacs/project-root)))
    (and root (not (member root spacemacs/flycheck-disabled-projects)))))

(defun spacemacs/get-project-buffers ()
  "Return list of buffers belonging to the current project."
  (let ((root (spacemacs/project-root)))
    (when root
      (seq-filter
       (lambda (buf)
         (when-let ((file (buffer-file-name buf)))
           (string-prefix-p root (expand-file-name file))))
       (buffer-list)))))

(defun spacemacs/disable-flycheck-for-project ()
  "Disable Flycheck for the current project."
  (interactive)
  (let ((root (spacemacs/project-root)))
    (unless root
      (user-error "Not in a projectile project"))

    (add-to-list 'spacemacs/flycheck-disabled-projects root)
    (spacemacs/flycheck-save-disabled-projects)

    ;; Turn off Flycheck in all project buffers
    (dolist (buf (spacemacs/get-project-buffers))
      (with-current-buffer buf
        (when (bound-and-true-p flycheck-mode)
          (flycheck-mode -1))))

    (message "Flycheck disabled for project: %s" root)))

(defun spacemacs/enable-flycheck-for-project ()
  "Enable Flycheck for the current project."
  (interactive)
  (let ((root (spacemacs/project-root)))
    (unless root
      (user-error "Not in a projectile project"))

    (setq spacemacs/flycheck-disabled-projects
          (delete root spacemacs/flycheck-disabled-projects))
    (spacemacs/flycheck-save-disabled-projects)

    ;; Activate Flycheck in all project buffers
    (dolist (buf (spacemacs/get-project-buffers))
      (with-current-buffer buf
        (when (and (buffer-file-name)
                   (not (bound-and-true-p flycheck-mode)))
          (flycheck-mode 1))))

    (message "Flycheck enabled for project: %s" root)))

(defun spacemacs/toggle-flycheck-for-project ()
  "Toggle Flycheck on/off for the current project."
  (interactive)
  (if (spacemacs/flycheck-enabled-for-project-p)
      (spacemacs/disable-flycheck-for-project)
    (spacemacs/enable-flycheck-for-project)))

;; Advice to prevent Flycheck from starting in disabled projects
(defun spacemacs/flycheck-check-project-enabled-advice (orig-fun &rest args)
  "Prevent Flycheck from starting if the project is disabled."
  (if (and (spacemacs/project-root)
           (not (spacemacs/flycheck-enabled-for-project-p)))
      (message "Flycheck is disabled for this project")
    (apply orig-fun args)))

(advice-add 'flycheck-mode :around #'spacemacs/flycheck-check-project-enabled-advice)
