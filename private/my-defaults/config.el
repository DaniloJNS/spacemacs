;;; config.el --- Agda2 Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2024 Danilo Nascimento
;;
;; Author: DaniloJNS <dan.silva.13.zx@gmail.com>
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

;; (setq treesit-language-source-alist
;;       '((bash "https://github.com/tree-sitter/tree-sitter-bash")
;;         (cmake "https://github.com/uyha/tree-sitter-cmake")
;;         (css "https://github.com/tree-sitter/tree-sitter-css")
;;         (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;         (go "https://github.com/tree-sitter/tree-sitter-go")
;;         (html "https://github.com/tree-sitter/tree-sitter-html")
;;         (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;         (json "https://github.com/tree-sitter/tree-sitter-json")
;;         (make "https://github.com/alemuller/tree-sitter-make")
;;         (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;         (python "https://github.com/tree-sitter/tree-sitter-python")
;;         (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
;;         (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;         (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;         (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;         (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; (setq treesit-language-source-alist nil)

;; (setq treesit-load-name-override-list '((js "libtree-sitter-js" "tree_sitter_javascript")))
;; (setq treesit-load-name-override-list '((ruby "libtree-sitter-ruby" "tree_sitter_ruby")))

;; Mapping ruby for treesitter allow list
;; (add-to-list 'tree-sitter-major-mode-language-alist '(ruby-ts-mode . ruby))
;; (add-to-list 'tree-sitter-major-mode-language-alist '(ruby-mode . libtree-sitter-ruby))

;; ;; reset alist
;; (setq tree-sitter-major-mode-language-alist '())

;; (when-let ((language-name (alist-get 'ruby-mode
;;                                      tree-sitter-major-mode-language-alist)))
;;   (add-to-list 'tree-sitter-major-mode-language-alist
;;                (cons 'enh-ruby-mode language-name)))
;; (tree-sitter-require 'ruby)
;; (tree-sitter-require 'libtree-sitter-ruby "tree_sitter_ruby")
;; (ruby-ts-mode)
;; (tree-sitter-load 'ruby)
;; (add-to-list 'tree-sitter-load-path "/home/danilo/.emacs.d/tree-sitter")

;; (setq native-symbol-name "tree_sitter_ruby")
;; (setq file nil)
;; (setq lang-symbol 'libtree-sitter-ruby)
;; (let* ((lang-name (symbol-name 'libtree-sitter-ruby))
;;        ;; Example: c-sharp -> c_sharp.
;;        (fallback-name (replace-regexp-in-string "-" "_" lang-name))
;;        (native-symbol-name (or native-symbol-name
;;                                (format "tree_sitter_%s"
;;                                        fallback-name)))
;;        ;; List of base file names to search for.
;;        (files (if file
;;                   ;; Use only FILE, if it's given.
;;                   (list file)
;;                 ;; Otherwise use LANG-SYMBOL. First, as-is. Then, with hyphens
;;                 ;; replaced by underscores.
;;                 (cons lang-name
;;                       (unless (string= lang-name fallback-name)
;;                         (list fallback-name)))))
;;        (full-path (seq-some (lambda (base-name)
;;                               (locate-file base-name
;;                                            tree-sitter-load-path
;;                                            tree-sitter-load-suffixes))
;;                             files))
;;        )
;;   fallback-name
;;   files
;;   full-path
;;   native-symbol-name
;;   (tsc--load-language full-path native-symbol-name lang-symbol)
;;   )
