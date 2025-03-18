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

(defun my-defaults//toggle-vterm-terminal ()
  (interactive)
  (if (projectile-project-p) (call-interactively 'spacemacs/projectile-shell-pop) (spacemacs/shell-pop-vterm)))
