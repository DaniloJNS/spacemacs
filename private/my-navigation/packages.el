;;; packages.el --- my-navigation layer packages file for Spacemacs. -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
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

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-navigation-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-navigation/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-navigation/pre-init-PACKAGE' and/or
;;   `my-navigation/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-navigation-packages
  '((powerthesaurus :location (recipe :fetcher github
                                      :repo "doomelpa/powerthesaurus"))
    wordnut
    synosaurus
    ))

;; HACK
;; Add lib directory to `load-path'.
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "lib" dir)))

(require 'lookup)    ; autoloads
(require 'online)    ; autoloads

(add-hook 'prog-mode-hook #'spacemacs//dumb-jump-before-etags-h)

(defun my-navigation/init-powerthesaurus ()
  (use-package powerthesaurus
    :defer t
    :ensure t))

(defun my-navigation/init-wordnut ()
  (use-package wordnut
    :defer t
    :ensure t))

(defun my-navigation/init-synosaurus ()
  (use-package synosaurus
    :defer t
    :ensure t))
