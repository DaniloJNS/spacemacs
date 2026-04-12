;;; alchemist-hexdocs.el --- HexDocs reader inside Emacs via shr. -*- lexical-binding: t -*-

;; Copyright © 2014-2016 Samuel Tonini

;; Author: Samuel Tonini <tonini.samuel@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  Browse HexDocs documentation inside Emacs using shr, similar to devdocs.el.
;;  Renders pages in a dedicated buffer with back/forward navigation and
;;  internal link handling.

;;; Code:

(require 'shr)
(require 'dom)
(require 'url)
(require 'alchemist-hex)

;;; Module cache

(defvar alchemist-hexdocs--cache (make-hash-table :test #'equal)
  "Cache of module lists keyed by \"package@version\".
Each value is an alist of (module-name . absolute-url).")

;;; Buffer-local navigation state

(defvar-local alchemist-hexdocs--stack nil
  "Navigation history. Each entry is a plist (:url URL :title TITLE).")

(defvar-local alchemist-hexdocs--forward-stack nil
  "Forward navigation stack for `alchemist-hexdocs-go-forward'.")

;;; URL helpers

(defun alchemist-hexdocs--url-p (url)
  "Return non-nil if URL is a hexdocs.pm URL."
  (and (stringp url)
       (string-match-p "\\`https://hexdocs\\.pm/" url)))

(defun alchemist-hexdocs--parse-url-info (url)
  "Return (package . version) from a hexdocs.pm URL, or nil."
  (when (string-match
         "\\`https://hexdocs\\.pm/\\([^/]+\\)/\\([0-9][^/]*\\)"
         url)
    (cons (match-string 1 url) (match-string 2 url))))

(defun alchemist-hexdocs--latest-version (package-name)
  "Return the latest version string for PACKAGE-NAME via the Hex API."
  (let* ((infos (alchemist-hex--fetch-package-info package-name))
         (releases (cdr (assoc 'releases infos))))
    (cdr (assoc 'version (aref releases 0)))))

;;; HTML fetching

(defun alchemist-hexdocs--fetch-dom (url)
  "Fetch URL and return a parsed HTML DOM, or nil on failure.
Signals an error with a descriptive message on HTTP error responses."
  (let* ((inhibit-message t)
         (url-buf (url-retrieve-synchronously url t)))
    (unless url-buf
      (user-error "Could not connect to %s" url))
    (unwind-protect
        (with-current-buffer url-buf
          (goto-char (point-min))
          ;; Skip past HTTP headers (blank line separator)
          (unless (re-search-forward "^\r?$" nil t)
            (user-error "Unexpected response from %s" url))
          (forward-char)
          (when (looking-at "[ \t\r\n]*\\'")
            (user-error "Empty response body from %s" url))
          (libxml-parse-html-region (point) (point-max)))
      (kill-buffer url-buf))))

(defun alchemist-hexdocs--page-title (dom)
  "Return the <title> text of DOM, or nil."
  (when-let* ((title-node (car (dom-by-tag dom 'title))))
    (string-trim (dom-text title-node))))

;;; Rendering

;; Elements to skip when rendering hexdocs pages with shr.
;; This avoids rendering the sidebar navigation that appears before the content.
(defun alchemist-hexdocs--shr-ignore (_node)
  "Rendering function that produces no output, used to skip elements.")

(defun alchemist-hexdocs--render (url)
  "Fetch URL and render it in the *alchemist-hexdocs* buffer."
  (message "Loading %s…" url)
  (let ((dom (alchemist-hexdocs--fetch-dom url)))
    (let* ((title (or (alchemist-hexdocs--page-title dom) url))
           (entry (list :url url :title title)))
      (with-current-buffer (get-buffer-create "*alchemist-hexdocs*")
        (unless (eq major-mode 'alchemist-hexdocs-mode)
          (alchemist-hexdocs-mode))
        (let* ((inhibit-read-only t)
               (shr-base (url-generic-parse-url url))
               (shr-width (and (> (window-width) 0) (window-width)))
               (shr-external-rendering-functions
                ;; Skip sidebar/navigation elements; keep actual content
                (append '((nav    . alchemist-hexdocs--shr-ignore)
                           (aside  . alchemist-hexdocs--shr-ignore))
                         shr-external-rendering-functions)))
          (erase-buffer)
          (condition-case err
              (shr-insert-document dom)
            (error (insert (format "Error rendering page: %s" err)))))
        (set-buffer-modified-p nil)
        (goto-char (point-min))
        (push entry alchemist-hexdocs--stack)
        (setq alchemist-hexdocs--forward-stack nil))
      (pop-to-buffer "*alchemist-hexdocs*")
      (message "%s" title))))

(defun alchemist-hexdocs--revert-buffer (&rest _)
  "Re-render the current page."
  (when-let* ((entry (car alchemist-hexdocs--stack))
              (url (plist-get entry :url)))
    (pop alchemist-hexdocs--stack)
    (alchemist-hexdocs--render url)))

(defun alchemist-hexdocs--header-line ()
  "Return a header line string for the current hexdocs buffer."
  (when-let* ((entry (car alchemist-hexdocs--stack)))
    (let* ((title (plist-get entry :title))
           (url (plist-get entry :url))
           (info (alchemist-hexdocs--parse-url-info url))
           (pkg (car info))
           (ver (cdr info)))
      (if (and pkg ver)
          (format " %s %s  »  %s" pkg ver title)
        (format " %s" title)))))

;;; Internal link handler

(defun alchemist-hexdocs--url-handler (url &rest _)
  "Open URL in the alchemist-hexdocs buffer (internal link handler)."
  (alchemist-hexdocs--render url))

;;; Major mode

(defvar alchemist-hexdocs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab]       #'forward-button)
    (define-key map [backtab]   #'backward-button)
    (define-key map "l"         #'alchemist-hexdocs-go-back)
    (define-key map "r"         #'alchemist-hexdocs-go-forward)
    (define-key map "m"         #'alchemist-hexdocs-browse-module)
    (define-key map "s"         #'alchemist-hexdocs-search-in-package)
    (define-key map "o"         #'alchemist-hexdocs-open-external)
    (define-key map "g"         #'revert-buffer)
    (define-key map "q"         #'quit-window)
    map)
  "Keymap for `alchemist-hexdocs-mode'.")

(define-derived-mode alchemist-hexdocs-mode special-mode "HexDocs"
  "Major mode for browsing HexDocs documentation.
Similar to `devdocs-mode': renders HTML with shr, handles internal
links within the same buffer, and maintains a navigation history.

\\{alchemist-hexdocs-mode-map}"
  :interactive nil
  (setq-local
   buffer-undo-list t
   header-line-format '(:eval (alchemist-hexdocs--header-line))
   revert-buffer-function #'alchemist-hexdocs--revert-buffer
   truncate-lines t)
  ;; Intercept hexdocs.pm links so they render in-buffer.
  (if (boundp 'browse-url-handlers)
      (setq-local browse-url-handlers
                  `((alchemist-hexdocs--url-p
                     . alchemist-hexdocs--url-handler)
                    ,@browse-url-handlers))
    (setq-local browse-url-browser-function
                `(("hexdocs\\.pm" . alchemist-hexdocs--url-handler)
                  ,@(if (functionp browse-url-browser-function)
                        `(("" . ,browse-url-browser-function))
                      browse-url-browser-function)))))

;;; Navigation

;;;###autoload
(defun alchemist-hexdocs-go-back ()
  "Go to the previously viewed page."
  (interactive)
  (unless (cadr alchemist-hexdocs--stack)
    (user-error "No previous page"))
  (let ((current (pop alchemist-hexdocs--stack)))
    (push current alchemist-hexdocs--forward-stack))
  (let* ((entry (car alchemist-hexdocs--stack))
         (url (plist-get entry :url)))
    (pop alchemist-hexdocs--stack)
    (alchemist-hexdocs--render url)))

;;;###autoload
(defun alchemist-hexdocs-go-forward ()
  "Go forward to the next page."
  (interactive)
  (unless alchemist-hexdocs--forward-stack
    (user-error "No next page"))
  (let* ((entry (pop alchemist-hexdocs--forward-stack))
         (url (plist-get entry :url)))
    (alchemist-hexdocs--render url)))

;;; Module list

(defun alchemist-hexdocs--fetch-modules (package version)
  "Return an alist of (module-name . url) for PACKAGE at VERSION.
Results are cached under the key \"package@version\"."
  (let ((cache-key (format "%s@%s" package version)))
    (or (gethash cache-key alchemist-hexdocs--cache)
        (let* ((api-url (format "%s/%s/%s/api-reference.html"
                                alchemist-hexdoc-url package version))
               (dom (alchemist-hexdocs--fetch-dom api-url))
               modules)
          (when dom
            (let* ((full-list (car (dom-by-id dom "full-list")))
                   (links (dom-by-tag full-list 'a)))
              (dolist (link links)
                (let ((href (dom-attr link 'href))
                      (text (string-trim (dom-text link))))
                  (when (and href (not (string-empty-p text)))
                    (push (cons text
                                (if (string-match-p "\\`https?://" href)
                                    href
                                  (format "%s/%s/%s/%s"
                                          alchemist-hexdoc-url
                                          package version href)))
                          modules))))))
          (let ((result (nreverse modules)))
            (puthash cache-key result alchemist-hexdocs--cache)
            result)))))

;;; Public interactive commands

;;;###autoload
(defun alchemist-hexdocs-open (package-name &optional version)
  "Open HexDocs for PACKAGE-NAME in a dedicated shr buffer.
Without prefix arg, use the latest version.
With prefix arg (\\[universal-argument]), prompt for VERSION."
  (interactive
   (list (read-string "Package: ")
         (when current-prefix-arg
           (read-string "Version: "))))
  (when (string-empty-p package-name)
    (user-error "Package name must not be empty"))
  (let* ((ver (or (and (stringp version) (not (string-empty-p version)) version)
                  (alchemist-hexdocs--latest-version package-name)))
         (url (format "%s/%s/%s" alchemist-hexdoc-url package-name ver)))
    (alchemist-hexdocs--render url)))

;;;###autoload
(defun alchemist-hexdocs-open-at-point ()
  "Open HexDocs for the dependency name at point."
  (interactive)
  (alchemist-hexdocs-open (alchemist-hex--deps-name-at-point)))

;;;###autoload
(defun alchemist-hexdocs-browse-module (&optional package-name version)
  "Browse modules for a package with completing-read, then render the selection.
When called inside the *alchemist-hexdocs* buffer the current package and
version are used automatically."
  (interactive)
  (let* ((current-entry (car alchemist-hexdocs--stack))
         (current-url (and current-entry (plist-get current-entry :url)))
         (current-info (and current-url (alchemist-hexdocs--parse-url-info current-url)))
         (pkg (or package-name
                  (car current-info)
                  (read-string "Package: ")))
         (ver (or version
                  (cdr current-info)
                  (alchemist-hexdocs--latest-version pkg)))
         (modules (alchemist-hexdocs--fetch-modules pkg ver)))
    (if (null modules)
        (message "No modules found for %s %s" pkg ver)
      (let* ((cands (mapcar #'car modules))
             (choice (completing-read
                      (format "Module (%s %s): " pkg ver)
                      cands nil t))
             (url (cdr (assoc choice modules))))
        (when url
          (alchemist-hexdocs--render url))))))

;;;###autoload
(defun alchemist-hexdocs-search-in-package (query &optional package-name)
  "Search for QUERY in HexDocs for PACKAGE-NAME and render the results."
  (interactive (list (read-string "Search query: ")))
  (let* ((current-entry (car alchemist-hexdocs--stack))
         (current-url (and current-entry (plist-get current-entry :url)))
         (current-info (and current-url (alchemist-hexdocs--parse-url-info current-url)))
         (pkg (or package-name
                  (car current-info)
                  (read-string "Package: ")))
         (url (format "%s/%s/search.html?q=%s"
                      alchemist-hexdoc-url pkg
                      (url-hexify-string query))))
    (alchemist-hexdocs--render url)))

;;;###autoload
(defun alchemist-hexdocs-open-external ()
  "Open the current HexDocs page in the external browser."
  (interactive)
  (if-let* ((entry (car alchemist-hexdocs--stack))
            (url (plist-get entry :url)))
      (browse-url url)
    (user-error "No current page")))

(provide 'alchemist-hexdocs)

;;; alchemist-hexdocs.el ends here
