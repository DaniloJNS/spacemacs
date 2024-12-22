;;; funcs.el --- my-defaults layer functions file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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

;; (defun my-defaults//my-evil-treemacs-binds()
;;   ((evil-define-key 'treemacs treemacs-mode-map (kbd "C-l") #'treemacs-RET-action)))

(defun toggle-fold ()
  (interactive)
  (save-excursion
    (end-of-line)
    (ts-fold-toggle)))



;; (setq terminal-main-process nil)

;; ;; Basic process routines
;; 

;; ;; The 'command' argument is a list, containing the complete command to create a subprocess, where the first element
;; ;; is the name of the program and the remaining elements are its arguments
;; ;; eg.: in shell `kitty -T emacs` -> list format '("kitty" "-T" "emacs")
;; (defun spawn-async-process (command &optional output dir)
;;   (let* ((process-name (car command))
;;          (default-directory (or dir default-directory))
;;          (process-output (or output (format "*%s*" process-name))))
;;     (apply #'start-process process-name process-output command)))

;; (defun spawn-sync-process (command &optional output dir)
;;   (let* ((program (car command))
;;          (args (cdr command))
;;          (default-directory (or dir default-directory))
;;          (process-output (or output (format "*%s*" program))))
;;     (apply #'process-file program  nil process-output nil args)))


;; (defun spawn-external-terminal ()
;;   (let* ((kitty-cmd '("kitty" "-T" "emacs-managed" "--listen-on=unix:@emacs-kitty-session"))
;;          ;; (let* ((kitty-cmd '("kitty" "-T" "emacs-managed" "--listen-on=unix:@emacs-kitty-session" "-e" "tmux" "new" "-s" "default"))
;;          (terminal-proc (spawn-async-process kitty-cmd nil "/home/danilo/workspace/youse")))
;;     (setq terminal-process-running t)
;;     (setq terminal-process terminal-proc)
;;     (set-process-sentinel
;;      terminal-proc
;;      (lambda (process event)
;;        (message "Process: %s had the event '%s'" process event)
;;        (when (eq (process-status process) 'exit)
;;          (clean-state-variables))))
;;     (set-process-filter terminal-proc #'ordinary-insertion-filter)))

;; (defun clean-state-variables ()
;;   (setq terminal-process-running nil)
;;   (setq terminal-tty-id nil))

;; (spawn-external-terminal)

;; ;; Kitten Commands
;; 

;; (defun kitten-run-remote-command-async (command-to-send)
;;   (let* (
;;          (remote-command-sender '("kitten" "@" "--to=unix:@emacs-kitty-session"))
;;          (command (append remote-command-sender command-to-send)))
;;     (message "Kitten command: %s" command)
;;     (spawn-async-process command nil default-directory)))

;; (defun kitten-run-remote-command-sync (command-to-send &optional output)
;;   (let* (
;;          (remote-command-sender '("kitten" "@" "--to=unix:@emacs-kitty-session"))
;;          (command (append remote-command-sender command-to-send)))
;;     (message "Kitten command: %s" command)
;;     (spawn-sync-process command output default-directory)))

;; (defun kitten-run-remote-command--send-text (text)
;;   (kitten-run-remote-command (list "send-text" text)))

;; (defun kitten-run-remote-command--send-key (text)
;;   (kitten-run-remote-command (list "send-key" text)))



;; ;; The client of tmux is o tty root of kitty windows UAU!
;; (defun kitten-run-remote-command--get-tty-id ()
;;   (kitten-run-remote-command-sync (list "send-text" (format "echo \"shell-tty $(tty)\" > %s\n" (process-tty-name terminal-process))))
;;   (with-temp-buffer
;;     (kitten-run-remote-command-sync (list "get-text" "--extent" "last_cmd_output") (buffer-name))
;;     (goto-char (point-min))
;;     (re-search-forward "^\\(.+\\)$" nil t)
;;     (match-string-no-properties 1)))

;; (defun kitten-run-remote-command--async-get-tty ()
;;   (kitten-run-remote-command-sync (list "send-text" (format "echo \"shell-tty $(tty)\" > %s\n" (process-tty-name terminal-process)))))

;; (defun kitten-run-remote-command--exit-tmux-session (text)
;;   (kitten-run-remote-command (list "send-key" "ctrl+a"))
;;   (kitten-run-remote-command (list "send-key" "d")))

;; (defun kitten-run-remote-command--attach-tmux-session (session-name)
;;   (kitten-run-remote-command (list "send-text" (format "tmux attach-session -t %s\n" session-name))))

;; (defun kitten-run-remote-command--start-tmux-session (session-name &optional attach)
;;   (let* ((attach-session-flag (or (when attach "-A") ""))
;;          (command (format "tmux new -s %s %s\n" session-name attach-session-flag)))
;;     (kitten-run-remote-command-async (list "send-text" command))))

;; ;; The client of tmux is o tty root of kitty windows UAU!
;; (defun kitten-run-remote-command--get-tmux-client-name ()
;;   (kitten-run-remote-command (list "send-text" "tmux display -p '#{client_tty}'\n")))

;; ;; Tmux interface commands
;; 

;; (defun run-tmux-command (tmux-cmd &optional output)
;;   (let* ((base-c (list "tmux"))
;;          (p-command (append base-c tmux-cmd)))
;;     (spawn-sync-process p-command output)))

;; (defun tmux:exits-session-p (session-name)
;;   (member session-name (tmux:get-sessions)))


;; (defun tmux:get-sessions ()
;;   (with-temp-buffer
;;     (run-tmux-command '("list-sessions") (buffer-name))
;;     (goto-char (point-min))
;;     (let (sessions)
;;       (while (re-search-forward "^\\([^:]+\\):" nil t)
;;         (push (match-string-no-properties 1) sessions))
;;       sessions)))

;; (defun tmux:get-current-client-name ()
;;   (with-temp-buffer
;;     (run-tmux-command '("list-clients" "-F" "#{client_name}") (buffer-name))
;;     (goto-char (point-min))
;;     (re-search-forward "^\\(.+\\)$" nil t)
;;     (match-string-no-properties 1)))

;; (defun tmux:get-clients ()
;;   (with-temp-buffer
;;     (run-tmux-command '("display" "-p" "#{client_tty}") (buffer-name))
;;     (goto-char (point-min))
;;     (let (clients)
;;       (while (re-search-forward "^\\(.+\\)$" nil t)
;;         (push (match-string-no-properties 1) clients))
;;       clients)))

;; (defun tmux:create-session(name)
;;   (run-tmux-command (list "new" "-s" name "-d")))

;; (defun tmux:change-attached-session(session-name)
;;   (if (not (tmux:exits-session-p session-name))
;;       (tmux:create-session session-name))
;;   (run-tmux-command (list "switch-client" "-c" (tmux:get-current-client-name) "-t" session-name)))


;; (defun terminal-tmux-session-connected()
;;   (cl-some (lambda (client) (equal client tmux-client-name)) (tmux:get-clients)))

;; (defun connect-tmux-session-p (session-name)
;;   (if (terminal-tmux-session-connected)
;;       (tmux:change-attached-session session-name)
;;     (kitten-run-remote-command--start-tmux-session session-name t)))

;; ;; Next Steps
;;                                         ;:  1. Resgatar o indentificador tty do terminal kitty. Parece rasuavel executar essa acao apos o terminal ser criado.
;; ;;     O valor de tty do terminal eh muito importante. O tmux usa esse valor para indentificar os clients de cada sessao.
;; ;;     Com esse valor, podemos, por exemplo, mudar a sessao tmux do terminal kitty que criamos de forma indireta atraves do
;; ;;     subcomando do tmux `switch-client'.
;; ;;     Comecei uma funcao para executar essa acao, o nome dela eh kitten-run-remote-command--get-tty-id. Parei de trabalhar nela
;; ;;     quando me de parei com desafio de executar o comando no kitty aberto atraves do kitten de `get-text' apos executar `send-text
;; ;;     "tty\n". Executar esse comando pelo metood `kitten-run-remote-command` parece nao ser viavel por que ele eh executado
;; ;;     async, logo nao temos acesso imediato ao output do comando passado chamar essa funcao.
;; ;;     Possivel saida, Criar um versao sync de `kitten-run-remote-command`
;; ;;

;; (defun open-external-terminal()
;;   (if (not terminal-process-running)
;;       (spawn-external-terminal))
;;   (sit-for 1)
;;   (kitten-run-remote-command--async-get-tty)
;;   (connect-tmux-session-p "pricing"))

;; (open-external-terminal)
;; (process-tty-name terminal-process)
;; (spawn-external-terminal)

;; (defun match-tty-name (string)
;;   (when (string-match "^\\([-a-z]+\\) \\([\/a-z1-9]+\\)$" string)
;;     (list (match-string 1 string) (match-string 2 string))))

;; (defun fill-terminal-tty-id (tty-id)
;;   (message "Filling terminal-tty-id with %s" tty-id)
;;   (setq terminal-tty-id tty-id))

;; (defun extract-tty-from-output-process (string)
;;   (let ((command (match-tty-name string)))
;;     (when (equal (car command) "shell-tty")
;;       (fill-terminal-tty-id (custom:last-list-element command)))
;;     command))

;; (sit-for)
;; (accept-process-output)

;; (waiting-for-user-input-p)

;; (defun custom:last-list-element (list)
;;   (nth (- (length list) 1) list))

;; (defun ordinary-insertion-filter (proc string)
;;   (message "Message received from process %s: %s" proc string)
;;   (when (buffer-live-p (process-buffer proc))
;;     (with-current-buffer (process-buffer proc)
;;       (let ((moving (= (point) (process-mark proc))))
;;         (save-excursion
;;           (goto-char (process-mark proc))
;;           (insert string)
;;           (extract-tty-from-output-process string)
;;           (set-marker (process-mark proc) (point)))
;;         (if moving (goto-char (process-mark proc)))))))
