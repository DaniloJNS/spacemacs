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
(with-eval-after-load 'evil
  ;; WINDOWS MANAGEMENT
  (evil-global-set-key 'normal (kbd "C-l") 'evil-window-right)
  (evil-global-set-key 'normal (kbd "C-h") 'evil-window-left)
  (evil-global-set-key 'normal (kbd "C-j") 'evil-window-down)
  (evil-global-set-key 'normal (kbd "C-k") 'evil-window-up)

  (evil-global-set-key 'normal (kbd "M-l") 'evil-window-increase-width)
  (evil-global-set-key 'normal (kbd "M-h") 'evil-window-decrease-width)
  (evil-global-set-key 'normal (kbd "M-k") 'evil-window-increase-height)
  (evil-global-set-key 'normal (kbd "M-j") 'evil-window-decrease-height)
  ;; (evil-global-set-key 'normal (kbd "C--") 'golden-ratio)
  (evil-global-set-key 'normal (kbd "C--") 'zoom)

  ;; Better search experience
  (evil-global-set-key 'normal (kbd "gw") 'spacemacs/symbol-overlay)
  (evil-global-set-key 'normal (kbd "gW") 'symbol-overlay-remove-all)

  (evil-global-set-key 'normal (kbd "C-f") 'symbol-overlay-remove-all)
  (evil-global-set-key 'normal (kbd "gC") 'symbol-overlay-remove-all)
  (evil-global-set-key 'normal (kbd "C-s") 'save-buffer)

  (when (configuration-layer/package-used-p 'helm-swoop)
    (evil-global-set-key 'normal (kbd "/") 'helm-swoop))

  ;; WORKSPACE MANAGEMENT
  (evil-global-set-key 'normal (kbd "M-w") 'spacemacs/eyebrowse-switch-to-new-window-config)
  (evil-global-set-key 'normal (kbd "M-e") 'eyebrowse-next-window-config)
  (evil-global-set-key 'normal (kbd "M--") 'eyebrowse-prev-window-config)
  (evil-global-set-key 'normal (kbd "M-q") 'spacemacs/eyebrowse-close-window-config)

  ;; Toggles
  (evil-global-set-key 'normal (kbd "C-q t") 'centaur-tabs-mode)
  (evil-global-set-key 'normal (kbd "C-q l") 'display-line-numbers-mode)
  (evil-global-set-key 'normal (kbd "M--") 'eyebrowse-prev-window-config)
  (evil-global-set-key 'normal (kbd "M-q") 'spacemacs/eyebrowse-close-window-config)
  ;; (evil-global-set-key 'normal (kbd "zw") 'my-defaults/toggle-fold)

  (when (configuration-layer/package-used-p 'treemacs)
    (spacemacs|use-package-add-hook treemacs
      :post-config
      (evil-define-key 'treemacs treemacs-mode-map (kbd "C-l") #'evil-window-right)
      (evil-define-key 'treemacs treemacs-mode-map (kbd "C-j") #''evil-window-down)
      (evil-define-key 'treemacs treemacs-mode-map (kbd "C-'") #'treemacs-select-window)))


  (evil-global-set-key 'normal (kbd "C-d") 'spacemacs/shell-pop-vterm)
  (evil-global-set-key 'normal (kbd "C-'") 'treemacs-select-window)

  ;; Improve my personal workflow in travel by classes in rails project
  (when (configuration-layer/package-used-p 'lsp-mode)
    (evil-define-key 'normal prog-mode-map (kbd "gr") 'lsp-ui-peek-find-references))


  ;; TABS
  (when (configuration-layer/package-used-p 'centaur-tabs)
    (evil-global-set-key 'normal (kbd "M-[") 'centaur-tabs-forward)
    (evil-global-set-key 'normal (kbd "M-]") 'centaur-tabs-backward))

  ;; Magit Keybinds
  (evil-define-key 'normal forge-topic-mode-map (kbd "C-c r") 'code-review-forge-pr-at-point)
  (evil-define-key 'normal forge-notifications-mode-map (kbd "C-c r") 'code-review-forge-pr-at-point)
  (evil-define-key 'normal magit-status-mode-map (kbd "C-c r") 'code-review-forge-pr-at-point)
  (setq code-review-new-buffer-window-strategy #'switch-to-buffer)

  ;; Lsp | Code Navigation
  ;; TODO: We should define this keybing only in cases where we have ts-fold-mode enabled and the folding method is evi
  (evil-define-key 'normal prog-mode-map (kbd "<TAB>") 'toggle-fold)
  (evil-define-key 'normal prog-mode-map (kbd "gr") 'lsp-ui-peek-find-references)
  (evil-define-key 'normal prog-mode-map (kbd "gr") 'lsp-ui-peek-find-references)

  ;; Custom Org Mode Commands
  (evil-define-key 'normal org-mode-map (kbd "C-=") 'org-fill-paragraph)
  )
