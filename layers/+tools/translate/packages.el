;;; packages.el --- Translate Layer packages File for Spacemacs
;;
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


(defconst translate-packages
  '(translate-mode
    go-translate
    ;; treemacs-evil
    ))

(defun translate/init-translate-mode ()
  "Initialize required packages."
  (use-package translate-mode
    :defer t
    :hook (translate-mode . translate//set-translate-mode-paragraph-functions)))

(defun translate/init-go-translate ()
  (use-package go-translate
    ;; :commands (gts-translate)
    :config
    ;; (defclass translate//reference-paragraph-texter (gts-texter) ())
    ;; (cl-defmethod gts-text ((_ translate//reference-paragraph-texter))
    ;;   (translate-get-reference-paragraph-text-at-point))
    ;; (defclass translate//reference-paragraph-picker (gts-picker)
    ;;   ((texter :initarg :texter :initform (translate//reference-paragraph-texter))))
    ;; (cl-defmethod gts-pick ((o translate//reference-paragraph-picker))
    ;;   (let ((text (gts-text (oref o texter))))
    ;;     (when (= 0 (length (if text (string-trim text) "")))
    ;;       (user-error "Make sure there is any word at point, or selection exists"))
    ;;     (let ((path (gts-path o text)))
    ;;       (cl-values text path))))
    ;; (defun translate//check-and-get-render (render)
    ;;   (if (equal render 'posframe)
    ;;       (if (featurep 'posframe)
    ;;           (gts-posframe-pop-render)
    ;;         (display-warning 'translate "Missing package `posframe', back to use default `gts-buffer-render'.")
    ;;         (gts-buffer-render))
    ;;     (gts-buffer-render)))
    ;; (defconst translate//paragraph-translator
    ;;   (gts-translator
    ;;    :picker (translate//reference-paragraph-picker)
    ;;    :engines (list (gts-google-engine) (gts-google-rpc-engine) (gts-bing-engine))
    ;;    :render (translate//check-and-get-render translate/paragraph-render))
    ;;   "Paragraph translator for `go-translate'.")
    ;; (defconst translate//word-translator
    ;;   (gts-translator
    ;;    :picker (gts-noprompt-picker)
    ;;    :engines (list (gts-google-engine) (gts-google-rpc-engine) (gts-bing-engine))
    ;;    :render (translate//check-and-get-render translate/word-render))
    ;;   "Word translator for `go-translate'.")
    (setq gt-preset-translators
          `((ts-1 . ,(gt-translator
                      :taker (gt-taker :langs '(en pt) :text 'word)
                      :engines (gt-bing-engine)
                      :render (gt-overlay-render)))
            (ts-2 . ,(gt-translator
                      :taker (gt-taker :langs '(en pt) :text 'sentence)
                      :engines (gt-google-engine)
                      :render (gt-insert-render)))
            (ts-3 . ,(gt-translator
                      :taker (gt-taker :langs '(en pt) :text 'buffer
                                       :pick 'word :pick-pred (lambda (w) (length> w 6)))
                      :engines (gt-google-engine)
                      :render (gt-overlay-render :type 'help-echo)))))
    (defconst translate//replace-selection-translator
      (gt-translator
       :taker (gt-taker :text 'word :pick nil :langs '(pt en))
       :engines (gt-google-engine)
       :render (gt-insert-render :type 'replace)))
    (defconst translate//replace-selection-translator-inverse
      (gt-translator
       :taker (gt-taker :text 'word :pick nil :langs '(en pt))
       :engines (gt-google-engine)
       :render (gt-insert-render :type 'replace)))
    (setq gt-langs '(en pt))
    (setq gt-ignore-target-history-p t)
    (setq gt-default-translator
          (gt-translator
           :taker   (list (gt-taker :pick nil :if 'selection)
                          (gt-taker :text 'paragraph :if '(help-mode org-mode))
                          (gt-taker :text 'buffer :pick 'paragraph :if 'Info-mode)
                          (gt-taker :text 'buffer :pick 'fresh-word :if 'read-only)
                          (gt-taker :text 'word))
           :engines (list (gt-google-engine :if 'word)
                          (gt-google-engine :if 'no-word))
           :render  (list (gt-posframe-pop-render :if 'selection)
                          (gt-overlay-render :if '(read-only Info-mode org-mode))
                          (gt-insert-render :if (lambda (translator) (member (buffer-name) '("COMMIT_EDITMSG"))))
                          (gt-alert-render :if '(and xxx-mode (or not-selection (and read-only parts))))
                          (gt-buffer-render))))
    )
  )

(defun translate/pre-init-posframe ()
  (spacemacs|use-package-add-hook posframe
    :post-config (translate/init-go-translate)))

;; (defun translate/pre-init-treemacs-evil ()
;;   (spacemacs|use-package-add-hook treemacs-evil
;;     :post-config
;;     (evil-define-key 'treemacs treemacs-mode-map (kbd "C-l") #'evil-window-right)))
