;;; funcs.el --- Semantic Layer functions File for Spacemacs
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

(when (and (configuration-layer/package-used-p 'translate-mode)
           (configuration-layer/package-used-p 'go-translate))

  (defun translate/translate-current-reference-paragraph ()
    "Mostrar todas as traduções disponíveis do parágrafo de referência em um quadro pop-up."
    (interactive)
    (gts-translate translate//paragraph-translator))

  (defun translate/translate-word-at-point ()
    "Pop-up translations of the word at point."
    (interactive)
    (gts-translate translate//word-translator))

  (defun translate//set-translate-mode-paragraph-functions ()
    (cond ((eq major-mode 'markdown-mode)
           (setq translate-forward-paragraph-function 'markdown-forward-paragraph
                 translate-backward-paragraph-function 'markdown-backward-paragraph))
          ((eq major-mode 'org-mode)
           (setq translate-forward-paragraph-function 'org-forward-paragraph
                 translate-backward-paragraph-function 'org-backward-paragraph))))

  (defun translate//translate-and-replcace-text()
    "Translate the selected word or text at the current cursor position and rewrite it by the translation result"
    (interactive)
    ;; Prevents the last target from being reused for language sets
    (setq gt-last-target nil)
    (gt-start translate//replace-selection-translator))

  (defun translate//translate-and-replcace-text-inverse()
    "Translate the selected word or text at the current cursor position and rewrite it by the translation result"
    (interactive)
    ;; Prevents the last target from being reused for language sets
    (setq gt-last-target nil)
    (gt-start translate//replace-selection-translator-inverse))

  (defun translate//soft-gt-do-translate()
    (interactive)
    (setq gt-last-target nil)
    (gt-do-translate)
    )

  (defun translate//gt-reset-last-target()
    (interactive)
    (setq gt-last-target nil)
    ))


;; (oset translate//replace-selection-translator target '(en pt))
;; (oset translate//replace-selection-translator-inverse target '(en ru))

;; (oref translate//replace-selection-translator taker)
;; (oref translate//replace-selection-translator target)

;; (gt-reset translate//replace-selection-translator-inverse)
;; (oref translate//replace-selection-translator-inverse taker)
;; (oref translate//replace-selection-translator-inverse target
;; translate//replace-selection-translator-inverse

;; (defun build-translator(text langs)
;;   (make-instance `gt-translator
;;                  :taker (gt-taker :text text :pick nil :langs langs)
;;                  :engines (gt-google-engine)
;;                  :render (gt-posframe-pop-render)))

;; (defun build-translator-with-defaults()
;;   (build-translator "Translate the selected word" '(en pt)))

;; (defun debug(translator)
;;   (with-slots (text bounds taker target keep) translator
;;     (message "%s" (gt-target taker translator)))
;;   )

;; (defun do-gt-start(text langs)
;;   (setq gt-last-target nil)
;;   (gt-start (build-translator text langs))
;;   )



;; (setq gt-ignore-target-history-p t)
;; (setq gt-debug-p nil)
;; (gt-available-langs '(en pt) "Translate the selected word")
;; (gt-start (build-translator-with-detauls))

;; (do-gt-start "Traduz a palavra na posicao atual do cursor" '(pt en))
;; (do-gt-start "Translate the selected word" '(en pt))
;; (debug (build-translator-with-defaults))

;; ;; (gt-langs-maybe "Translate the selected word" '(en pt))

;; (let ((translator (build-translator-with-defaults)))
;;   (with-slots (text bounds taker target keep) translator
;;     (gt-target taker translator)
;;     )
;;   )
