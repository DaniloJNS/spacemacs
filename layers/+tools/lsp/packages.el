;;; packages.el --- Language Server Protocol Layer packages file for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Fangrui Song <i@maskray.me>
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


(defconst lsp-packages
  '(
    lsp-mode
    (lsp-ui :toggle lsp-use-lsp-ui)
    (consult-lsp :requires consult)
    (helm-lsp :requires helm)
    (lsp-ivy :requires ivy)
    (lsp-treemacs :requires treemacs)
    (lsp-origami :requires lsp-mode)
    (lsp-sonarlint :toggle lsp-sonarlint)
    popwin))

(defun lsp/init-lsp-sonarlint ()
  (use-package lsp-sonarlint
    :init
    (setq
     lsp-sonarlint-auto-download t)
    :defer t))

(defun lsp/init-lsp-mode ()
  (use-package lsp-mode
    :defer t
    :init
    (setq lsp-server-install-dir (concat spacemacs-cache-directory "lsp/")
          lsp-session-file (concat lsp-server-install-dir (file-name-nondirectory ".lsp-session-v1"))
          lsp-eslint-library-choices-file (concat lsp-server-install-dir ".lsp-eslint-choices")
          lsp-yaml-schema-store-local-db (concat lsp-server-install-dir "lsp-yaml-schemas.json")
          lsp-vetur-global-snippets-dir (concat spacemacs-start-directory "snippets/vetur")
          lsp-ui-sideline-diagnostic-max-lines 20
          lsp-enable-file-watchers nil
          lsp-imenu-index-function #'lsp-imenu-create-categorized-index)
    (advice-add (if (progn (require 'json)
                           (fboundp 'json-parse-buffer))
                    'json-parse-buffer
                  'json-read)
                :around
                #'spacemacs//lsp-booster--advice-json-parse)
    (advice-add 'lsp-resolve-final-command :around #'spacemacs//lsp-booster--advice-final-command)
    ;; If you find something else should be ignored, you could also set them here
    ;; :hook
    ;; (lsp-completion-mode . spacemacs//lsp-mode-setup-completion)
    :config
    (if lsp-use-upstream-bindings
        (spacemacs/lsp-bind-upstream-keys)
      (spacemacs/lsp-bind-keys))
    (setq lsp-completion-provider (if (or (equal :all lsp-manage-backends-manually)
                                          (member major-mode lsp-manage-backends-manually)
                                          (equal 'company auto-completion-front-end))
                                      :none
                                    :capf))
    ;; This sets the lsp indentation for all modes derived from web-mode.
    (add-to-list 'lsp--formatting-indent-alist '(web-mode . web-mode-markup-indent-offset))
    (add-hook 'lsp-after-open-hook (lambda ()
                                     "Setup xref jump handler"
                                     (spacemacs//setup-lsp-jump-handler)))))

(defun lsp/init-lsp-ui ()
  (use-package lsp-ui
    :defer t
    :config
    (if lsp-remap-xref-keybindings
        (progn (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
               (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))

    (define-key lsp-ui-imenu-mode-map (kbd "=") 'spacemacs/lsp-ui--imenu-toggle-window-size)

    (spacemacs/lsp-define-key
     lsp-ui-peek-mode-map
     "h" #'lsp-ui-peek--select-prev-file
     "j" #'lsp-ui-peek--select-next
     "k" #'lsp-ui-peek--select-prev
     "l" #'lsp-ui-peek--select-next-file)
    ;; My Custom configs
    ;; diagnostics
    (setq lsp-ui-sideline-show-diagnostics t)
    (setq lsp-ui-sideline-show-hover nil)
    ;; docs
    (setq lsp-ui-doc-enable t)
    (setq lsp-ui-doc-position "top")
    (setq lsp-ui-doc-side "right")
    (setq lsp-ui-doc-delay 0.2)
    (setq lsp-ui-doc-show-with-cursor nil)
    (setq lsp-ui-doc-show-with-mouse t)
    ;; imenu
    (setq lsp-ui-imenu-kind-position "top")
    (setq lsp-ui-imenu-buffer-position "right")
    (setq lsp-ui-imenu-window-width 35)
    (setq lsp-ui-imenu-window-fix-width t)
    (setq lsp-ui-imenu--custom-mode-line-format nil)
    (setq lsp-ui-imenu-auto-refresh t)
    (setq lsp-ui-imenu-auto-refresh-delay 1.0)
    ))



(defun lsp/init-helm-lsp ()
  (use-package helm-lsp :defer t))

(defun lsp/init-lsp-ivy ()
  (use-package lsp-ivy :defer t))

(defun lsp/init-consult-lsp ()
  (use-package consult-lsp
    :defer t
    :after (lsp-mode)))

;; Reminder: Clean lsp-treemacs-errors-list--refresh function before starting LSP in a project
(defun lsp/init-lsp-treemacs ()
  (use-package lsp-treemacs
    :defer t
    :config
    ;; Advice the internal function `lsp-treemacs-errors-list--refresh' to avoid excessive updates
    ;; when modifying buffers. This function is called every time LSP diagnostics are updated.
    ;; Modify the variable `spacemacs/lsp-treemacs-errors-list-refresh-debounce-time' to define
    ;; the debounce time.
    (spacemacs/debounce-lsp-treemacs-errors-list-refresh)))


(defun lsp/init-lsp-origami ()
  (use-package lsp-origami
    :defer t
    :init
    (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable)))

(defun lsp/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*lsp-help*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
          popwin:special-display-config)))
