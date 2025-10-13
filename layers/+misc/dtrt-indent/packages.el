;;; packages.el --- dtrt-indent layer packages file for Spacemacs.  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Kevin Doherty <kjd@csail.mit.edu>
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


(defconst dtrt-indent-packages '(dtrt-indent))

(defun dtrt-indent/init-dtrt-indent ()
  (use-package dtrt-indent
    :hook (prog-mode .
                     (lambda ()
                       (dtrt-indent-mode)
                       (dtrt-indent-adapt)))
    :config
    (spacemacs|hide-lighter dtrt-indent-mode)

    ;; Enable dtrt-indent even in smie modes so that it can update `tab-width',
    ;; `standard-indent' and `evil-shift-width' there as well.
    (setq dtrt-indent-run-after-smie t)
    ;; Reduced from the default of 5000 for slightly faster analysis
    (setq dtrt-indent-max-lines 2000)

    ;; always keep tab-width up-to-date
    ;; (push '(t tab-width) dtrt-indent-hook-generic-mapping-list)
    ))

;;; packages.el ends here
