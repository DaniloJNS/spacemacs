;; keybindings.el --- Semantic Layer functions File for Spacemacs;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Ray Wang <rayw.public@gmail.com>
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

(defvar my-defaults-global-keybind-list '(
                                          ;; Windows navigation and resize
                                          ("C-l" . evil-window-right)
                                          ("C-h" . evil-window-left)
                                          ("C-j" . evil-window-down)
                                          ("C-k" . evil-window-up)
                                          ("C-o" . treemacs-select-window)
                                          ("C-p" . ace-window)

                                          ("M-l" . evil-window-increase-width)
                                          ("M-h" . evil-window-decrease-width)
                                          ("M-k" . 'evil-window-increase-height)
                                          ("M-j" . evil-window-decrease-height)
                                          ("C-<->" . zoom)

                                          ;; Better search experience
                                          ("gw" . spacemacs/symbol-overlay)
                                          ("gW" . symbol-overlay-remove-all)

                                          ("C-f" . symbol-overlay-remove-all)
                                          ("gC"  . symbol-overlay-remove-all)
                                          ("C-s" . save-buffer)

                                          ("/" . consult-line)

                                          ;; Workspace Management
                                          ("M-w" . spacemacs/eyebrowse-switch-to-new-window-config)
                                          ("M-e" . eyebrowse-next-window-config)
                                          ("M--" . eyebrowse-prev-window-config)
                                          ("M-q" . spacemacs/eyebrowse-close-window-config)
                                          ("M-TAB" . eyebrowse-last-window-config)

                                          ;; Toggles
                                          ("C-q t" . centaur-tabs-mode)
                                          ("C-q l" . display-line-numbers-mode)

                                          ;; Term
                                          ("C-d" . my-defaults//toggle-vterm-terminal)
                                          ))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "M-<tab>") 'eyebrowse-last-window-config)
  (evil-define-key 'normal 'global (kbd "C-<tab>") 'consult-buffer)
  (evil-define-key 'normal 'global (kbd "-") 'zoom)
  (evil-define-key 'normal 'global (kbd "C--") 'zoom)

  (dolist (bind my-defaults-global-keybind-list)
    (evil-define-key 'normal 'global (kbd (car bind)) (cdr bind))
    (define-key evil-evilified-state-map (kbd (car bind)) (cdr bind))
    (define-key evil-motion-state-map (kbd (car bind)) (cdr bind)))

  ;; Improve my personal workflow in travel by classes in large projects
  (when (configuration-layer/package-used-p 'lsp-mode)
    (evil-define-key 'normal prog-mode-map (kbd "gr") 'lsp-ui-peek-find-references))

  ;; TABS
  ;; BUG when running in terminal with ghost escaped input
  ;; (when (configuration-layer/package-used-p 'centaur-tabs)
  ;;   (evil-global-set-key 'normal (kbd "M-[") 'nil)
  ;;   (evil-global-set-key 'normal (kbd "M-]") 'nil))

  ;; Magit Keybinds
  (evil-define-key 'normal forge-topic-mode-map (kbd "C-c r") 'code-review-forge-pr-at-point)
  (evil-define-key 'normal forge-notifications-mode-map (kbd "C-c r") 'code-review-forge-pr-at-point)
  (evil-define-key 'normal magit-status-mode-map (kbd "C-c r") 'code-review-forge-pr-at-point)
  (setq code-review-new-buffer-window-strategy #'switch-to-buffer)

  ;; Lsp | Code Navigation
  ;; TODO: Must be define this keybing only in cases where we have ts-fold-mode enabled and the folding method is evi
  (evil-define-key 'normal prog-mode-map (kbd "<TAB>") 'toggle-fold)
  (evil-define-key 'normal prog-mode-map (kbd "gr") 'lsp-ui-peek-find-references)
  (evil-define-key 'visual prog-mode-map (kbd "gr") 'lsp-ui-peek-find-references)

  ;; Custom Org Mode Commands
  (evil-define-key 'normal org-mode-map (kbd "C-=") 'org-fill-paragraph)
  (evil-define-key 'normal org-agenda-mode-map (kbd "\\") 'org-agenda-filter-by-tag)

  ;; DevDocs
  (spacemacs/set-leader-keys "adp" 'devdocs-peruse)
  (spacemacs/set-leader-keys "adl" 'devdocs-lookup)

  ;; Dired keybinds
  (evil-define-key 'normal dired-mode-map (kbd "c") 'dired-create-empty-file)
  )


;; (defun debug-evil-commands ()
;;   (when (and (boundp 'evil-state) evil-state)
;;     (message "Evil State: %s | Command: %s | Keys: %s | Raw Keys: %s"
;;              evil-state
;;              this-command
;;              (key-description (this-command-keys))
;;              (this-command-keys))))

;; (add-hook 'pre-command-hook 'debug-evil-commands)
