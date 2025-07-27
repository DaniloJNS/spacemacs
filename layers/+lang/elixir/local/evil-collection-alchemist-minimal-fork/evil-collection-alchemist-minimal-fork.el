;;; evil-collection-alchemist-minimal-fork.el --- Evil bindings for `alchemist-minimal-fork' -*- lexical-binding: t -*-

;; Copyright (C) 2025 Danilo Nascimento

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for `alchemist'.

;;; Code:
(require 'evil-collection)
(require 'alchemist-minimal-fork)

(defconst evil-collection-alchemist-minimal-fork-maps '(alchemist-compile-mode-map
                                                        alchemist-test-report-mode-map
                                                        alchemist-eval-mode-map
                                                        alchemist-execute-mode-map
                                                        alchemist-message-mode-map
                                                        alchemist-help-minor-mode-map
                                                        alchemist-mix-mode-map
                                                        alchemist-iex-mode-map
                                                        alchemist-hex-mode-map
                                                        alchemist-macroexpand-mode-map
                                                        alchemist-refcard-mode-map))

;;;###autoload
(defun evil-collection-alchemist-minimal-fork-setup ()
  "Set up `evil' bindings for `alchemist-minimal-fork'."
  (evil-set-initial-state 'alchemist-test-report-mode-map 'normal)
  (evil-collection-set-readonly-bindings 'alchemist-test-report-mode-map)

  (dolist (keymap evil-collection-alchemist-minimal-fork-maps)

    (evil-collection-define-key nil keymap
      "g" nil)

    (evil-collection-define-key 'normal keymap
      "q" 'quit-window))

  (evil-collection-define-key 'normal 'alchemist-test-report-mode-map
    ;; (kbd "RET") 'compile-goto-error

    ;; "go" 'compilation-display-error
    ;; (kbd "S-<return>") 'compilation-display-error

    (kbd "TAB") 'alchemist-test-next-result
    (kbd "S-TAB") 'alchemist-test-previous-result

    "gf" 'find-file-at-point
    "t" 'toggle-truncate-lines
    "gj" 'alchemist-test-next-result
    "gk" 'alchemist-test-previous-result
    (kbd "C-j") 'alchemist-test-next-result
    (kbd "C-k") 'alchemist-test-previous-result
    "[[" 'alchemist-test-previous-stacktrace-file
    "]]" 'alchemist-test-next-stacktrace-file
    "gr" 'alchemist-mix-rerun-last-test)
  )

(provide 'evil-collection-alchemist-minimal-fork)
;;; evil-collection-alchemist-minimal-fork.el ends here
