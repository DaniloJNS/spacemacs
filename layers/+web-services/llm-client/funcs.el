;; -*- lexical-binding: nil; -*-
(defun spacemacs//gptel-send-wrapper ()
  "Wrapper function for gptel-send that sets the flag."
  (interactive)
  (call-interactively 'gptel-send)
  (setq llm-client--gptel-send-called t))

(defun spacemacs//gptel-abort-wrapper ()
  "Wrapper function for gptel-abort that checks if gptel-send has been called."
  (interactive)
  (if llm-client--gptel-send-called
      (call-interactively 'gptel-abort)))

(defun spacemacs//open-project-llm-chat()
  (interactive)
  (let* ((root (projectile-project-root))
         (llm-path (expand-file-name "LLM.org" root)))
    (cond
     ((not root) (user-error "Not in a project"))
     ((not (file-exists-p llm-path)) (user-error "LLM.org does not exists in project"))
     (t (find-file llm-path)))))
