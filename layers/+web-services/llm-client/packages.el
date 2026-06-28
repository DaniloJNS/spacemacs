;;; packages.el --- Large Language Model Client for Spacemacs  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Codruț Constantin Gușoi <mail+spacemacs@codrut.pro>
;; Author: Alexander Matyasko <alexander.matyasko@gmail.com>
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


(defconst llm-client-packages
  '((ellama :toggle llm-client-enable-ellama)
    (gptel :toggle llm-client-enable-gptel)
    (gptel-agent :toggle llm-client-enable-gptel-agent)
    org
    window-purpose))

(defun llm-client/init-agent-shell ()
  (use-package agent-shell
    :defer t
    :ensure t
    :custom
    (agent-shell-preferred-agent-config 'claude-code)
    ;; the display buffer behavior is controlled by popwin in spacemacs-visual
    (agent-shell-display-action nil)
    (agent-shell-prefer-session-resume nil)
    (agent-shell-session-strategy 'prompt)
    :init
    (require 'transient)
    (transient-define-prefix llm-client/agent-shell-menu ()
      "Agent Shell"
      [["Session"
        ("a" "Open/start"     agent-shell)
        ("t" "Toggle"         agent-shell-toggle)
        ("A" "New session"    agent-shell-new-shell)
        ("r" "Resume session" agent-shell-resume-session)
        ;; ("c" "Clear buffer" agent-shell-clear-buffer)
        ("i" "Interrupt"      agent-shell-interrupt)]
       ["Send"
        ("p" "Compose prompt"     agent-shell-prompt-compose)
        ("d" "Send dwim"          agent-shell-send-dwim)
        ("R" "Send region"        agent-shell-send-region)
        ("f" "Send file..."       agent-shell-send-file)
        ("F" "Send current file"  agent-shell-send-current-file)]
       ["View"
        ("h" "Help menu"      agent-shell-help-menu)
        ("T" "Transcript"     agent-shell-open-transcript)]])
    (spacemacs/set-leader-keys
      "aa" #'llm-client/agent-shell-menu)
    :config
    ;; Claude CLI login authentication
    (require 'agent-shell-anthropic)
    (setq agent-shell-anthropic-authentication
          (agent-shell-anthropic-make-authentication :login t))

    ;; Evil: insert mode = newline, normal mode = send
    (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
    (evil-define-key 'normal agent-shell-mode-map (kbd "RET") #'comint-send-input)

    ;; Evil navigation: ]/[ instead of n/p (which conflict with evil search-next/prev)
    (evil-define-key 'normal agent-shell-mode-map (kbd "n") nil)
    (evil-define-key 'normal agent-shell-mode-map (kbd "p") nil)
    (evil-define-key 'normal agent-shell-mode-map (kbd "]") #'agent-shell-next-item)
    (evil-define-key 'normal agent-shell-mode-map (kbd "[") #'agent-shell-previous-item)
    ;; Within-buffer bindings (normal-state)
    (evil-define-key 'normal agent-shell-mode-map (kbd "C") #'agent-shell-clear-buffer)
    (evil-define-key 'normal agent-shell-mode-map (kbd "gT") #'agent-shell-open-transcript)
    (evil-define-key 'normal agent-shell-mode-map (kbd "gh") #'agent-shell-help-menu)

    ;; Auto-approve read permissions (write and execute remain manual)
    (setq agent-shell-permission-responder-function
          (lambda (permission)
            (when-let (((equal (map-elt (map-elt permission :tool-call) :kind) "read"))
                       (choice (seq-find (lambda (option)
                                           (equal (map-elt option :kind) "allow_once"))
                                         (map-elt permission :options))))
              (funcall (map-elt permission :respond) (map-elt choice :option-id))
              t)))

    ;; Configure *agent-shell-diff* buffers to start in Emacs state
    (add-hook 'diff-mode-hook
              (lambda ()
                (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
                  (evil-emacs-state))))))

(defun llm-client/init-ellama ()
  "Initialize the `ellama` package and set up keybindings."
  (use-package ellama
    :defer t
    :init
    (spacemacs/declare-prefix "$" "AI")
    (spacemacs/declare-prefix "$e" "Ellama")
    ))


(defun llm-client/init-gptel ()
  "Initialize the `gptel` package and set up keybindings."
  (use-package gptel
    :defer t
    :init
    ;; evilify gptel-context-buffer-mode-map
    (evilified-state-evilify-map gptel-context-buffer-mode-map
      :eval-after-load gptel-context
      :mode gptel-context-buffer-mode
      :bindings
      "C-c C-c" #'gptel-context-confirm
      "C-c C-k" #'gptel-context-quit
      "RET"     #'gptel-context-visit
      "n"       #'gptel-context-next
      "p"       #'gptel-context-previous
      "d"       #'gptel-context-flag-deletion)
    ;; set up keybindings
    (spacemacs/declare-prefix "$g" "gptel")
    (spacemacs/set-leader-keys
      "$gg" 'gptel                          ; Start a new gptel session
      "$gs" 'spacemacs//gptel-send-wrapper  ; Send a message to gptel
      "$gq" 'spacemacs//gptel-abort-wrapper ; Abort any active gptel process
      "$gm" 'gptel-menu                     ; Open the gptel menu
      "$gc" 'gptel-add                      ; Add context
      "$gf" 'gptel-add-file                 ; Add a file
      "$go" 'gptel-org-set-topic            ; Set topic in Org-mode
      "$gp" 'gptel-org-set-properties       ; Set properties in Org-mode
      "$gr" 'gptel-rewrite)))               ; Rewrite or refactor test region

(defun llm-client/init-gptel-agent ()
  "Initialize the `gptel-agent` package and set up keybindings."
  (use-package gptel-agent
    :defer t
    :init
    ;; evilify gptel-context-buffer-mode-map
    (evilified-state-evilify-map gptel-context-buffer-mode-map
      :eval-after-load gptel-context
      :mode gptel-context-buffer-mode)
    ;; set up keybindings
    (spacemacs/set-leader-keys
      "$ga" 'gptel-agent                          ; Start a new gptel-agent session
      "$gu" 'gptel-agent-update)                  ; Updates the gptel-agent database
    ;; Config for =gptel-agent=
    :config (gptel-agent-update)))

(defun llm-client/post-init-org ()
  "Set up Org-mode keybindings for gptel."
  (spacemacs/declare-prefix-for-mode 'org-mode "m$g" "gptel")
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "$go" 'gptel-org-set-topic
    "$gp" 'gptel-org-set-properties))

(defun llm-client/post-init-window-purpose ()
  ;; TODO: Temporary fix to avoid the error when using window-purpose
  ;; see https://github.com/karthink/gptel/issues/237 for details
  ;; (purpose-set-extension-configuration
  ;;  :llm-client-layer
  ;;  (purpose-conf :mode-purposes '((gptel-mode . chat))))
  (defun llm-client/disable-purpose-mode-around-for-gptel (orig-func &rest args)
    "Advice function to disable purpose-mode before calling ORIG-FUNC with ARGS."
    (let ((purpose-mode-was-enabled (bound-and-true-p purpose-mode)))
      (when purpose-mode-was-enabled
        (purpose-mode -1))
      (unwind-protect
          (apply orig-func args)
        (when purpose-mode-was-enabled
          (purpose-mode 1)))))
  (advice-add 'gptel :around #'llm-client/disable-purpose-mode-around-for-gptel))
