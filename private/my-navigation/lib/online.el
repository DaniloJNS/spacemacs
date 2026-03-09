;;; my-navigation/lib/online.el -*- lexical-binding: t; -*-

(defvar +lookup--last-provider nil)

(defun spacemacs/lookup--online-provider (&optional force-p namespace)
  (let ((key (or namespace major-mode)))
    (or (and (not force-p)
             (cdr (assq key +lookup--last-provider)))
        (when-let (provider
                   (completing-read
                    "Search on: "
                    (mapcar #'car spacemacs/lookup-provider-url-alist)
                    nil t))
          (setf (alist-get key +lookup--last-provider) provider)
          provider))))

(defun spacemacs/lookup-online-backend-fn (identifier)
  "Open the browser and search for IDENTIFIER online.
When called for the first time, or with a non-nil prefix argument, prompt for
the search engine to use."
  (spacemacs/lookup-online
   identifier
   (spacemacs/lookup--online-provider (not current-prefix-arg))))

(defun spacemacs/lookup-online (query provider)
  "Look up QUERY in the browser using PROVIDER.
When called interactively, prompt for a query and, when called for the first
time, the provider from `spacemacs/lookup-provider-url-alist'. In subsequent calls, reuse
the previous provider. With a non-nil prefix argument, always prompt for the
provider.

QUERY must be a string, and PROVIDER must be a key of
`spacemacs/lookup-provider-url-alist'."
  (interactive
   (list (if (use-region-p) (spacemacs/thing-at-point-or-region))
         (spacemacs/lookup--online-provider current-prefix-arg)))
  (let ((backends (cdr (assoc provider spacemacs/lookup-provider-url-alist))))
    (unless backends
      (user-error "No available online lookup backend for %S provider"
                  provider))
    (catch 'done
      (dolist (backend backends)
        (cl-check-type backend (or string function))
        (cond ((stringp backend)
               (funcall spacemacs/lookup-open-url-fn
                        (format backend
                                (url-encode-url
                                 (or query
                                     (read-string (format "Search for (on %s): " provider)
                                                  (thing-at-point 'symbol t)))))))
              ((condition-case-unless-debug e
                   (and (fboundp backend)
                        (funcall backend query))
                 (error
                  (setf (alist-get major-mode +lookup--last-provider nil t) nil)
                  (signal (car e) (cdr e))))
               (throw 'done t)))))))

;;;###autoload
(defun +lookup/online-select ()
  "Run `+lookup/online', but always prompt for the provider to use."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'+lookup/online)))


;;
;;; Special provider frontends

(defvar ivy-initial-inputs-alist)
(defvar counsel-search-engine)
;;;###autoload
(defun +lookup--online-backend-google (query)
  "Search Google, starting with QUERY, with live autocompletion."
  (cond ((and (bound-and-true-p ivy-mode) (fboundp 'counsel-search))
         (let ((ivy-initial-inputs-alist `((t . ,query)))
               (counsel-search-engine 'google))
           (call-interactively #'counsel-search)
           t))
        ((and (bound-and-true-p helm-mode) (require 'helm-net nil t))
         (helm :sources 'helm-source-google-suggest
               :buffer "*helm google*"
               :input query)
         t)))

;;;###autoload
(defun +lookup--online-backend-duckduckgo (query)
  "Search DuckDuckGo, starting with QUERY, with live autocompletion."
  (cond ((and (bound-and-true-p ivy-mode) (fboundp 'counsel-search))
         (let ((ivy-initial-inputs-alist `((t . ,query)))
               (counsel-search-engine 'ddg))
           (call-interactively #'counsel-search)
           t))))

(provide 'online)
