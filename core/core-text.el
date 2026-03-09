;;; lisp/lib/text.el -*- lexical-binding: t; -*-

(defun spacemacs/region-active-p ()
  "Return non-nil if selection is active.
Detects evil visual mode as well."
  (declare (side-effect-free t))
  (or (use-region-p)
      (and (bound-and-true-p evil-local-mode)
           (evil-visual-state-p))))

(defun spacemacs/region-beginning ()
  "Return beginning position of selection.
Uses `evil-visual-beginning' if available."
  (declare (side-effect-free t))
  (or (and (bound-and-true-p evil-local-mode)
           (evil-visual-state-p)
           (markerp evil-visual-beginning)
           (marker-position evil-visual-beginning))
      (region-beginning)))

(defun spacemacs/region-end ()
  "Return end position of selection.
Uses `evil-visual-end' if available."
  (declare (side-effect-free t))
  (or (and (bound-and-true-p evil-local-mode)
           (evil-visual-state-p)
           (markerp evil-visual-end)
           (marker-position evil-visual-end))
      (region-end)))

(defun spacemacs/thing-at-point-or-region (&optional thing prompt)
  "Grab the current selection, THING at point, or xref identifier at point.

Returns THING if it is a string. Otherwise, if nothing is found at point and
PROMPT is non-nil, prompt for a string (if PROMPT is a string it'll be used as
the prompting string). Returns nil if all else fails.

NOTE: Don't use THING for grabbing symbol-at-point. The xref fallback is smarter
in some cases."
  (declare (side-effect-free t))
  (cond ((stringp thing)
         thing)
        ((spacemacs/region-active-p)
         (buffer-substring-no-properties
          (spacemacs/region-beginning)
          (spacemacs/region-end)))
        (thing
         (thing-at-point thing t))
        ((require 'xref nil t)
         ;; Eglot, nox (a fork of eglot), and elpy implementations for
         ;; `xref-backend-identifier-at-point' betray the documented purpose of
         ;; the interface. Eglot/nox return a hardcoded string and elpy prepends
         ;; the line number to the symbol.
         (if (memq (xref-find-backend) '(eglot elpy nox))
             (thing-at-point 'symbol t)
           ;; A little smarter than using `symbol-at-point', though in most
           ;; cases, xref ends up using `symbol-at-point' anyway.
           (xref-backend-identifier-at-point (xref-find-backend))))
        (prompt
         (read-string (if (stringp prompt) prompt "")))))

(defun spacemacs/pcre-quote (str)
  "Like `reqexp-quote', but for PCREs."
  (let ((special '(?. ?^ ?$ ?* ?+ ?? ?{ ?\\ ?\[ ?\| ?\())
        (quoted nil))
    (mapc (lambda (c)
            (when (memq c special)
              (push ?\\ quoted))
            (push c quoted))
          str)
    (concat (nreverse quoted))))

;; Make this module available for require
(provide 'core-text)
