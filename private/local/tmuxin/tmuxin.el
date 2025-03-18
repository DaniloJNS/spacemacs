;; (setq terminal-main-process nil)

;; ;; Basic process routines
;; 


(defcustom tmuxin-terminal-up nil
  "Base terminal state"
  :group 'tmuxin
  :type 'boolean)

(defcustom tmuxin-terminal-wm-class "tmuxin"
  "Base terminal state"
  :group 'tmuxin
  :type 'string)

(defcustom tmuxin-term-shell-tty-id nil
  "TODO describe this var"
  :group 'tmuxin
  :type 'string)

(defcustom tmuxin-default-session "default"
  "TODO describe this var"
  :group 'tmuxin
  :type 'string)

(defvar tmuxin-project-test-command-map
  (make-hash-table :test 'equal)
  "TODO describe this variable")

;; ;; The 'command' argument is a list, containing the complete command to create a subprocess, where the first element
;; ;; is the name of the program and the remaining elements are its arguments
;; ;; eg.: in shell `kitty -T emacs` -> list format '("kitty" "-T" "emacs")
(defun spawn-async-process (command &optional output dir)
  (let* ((process-name (car command))
         (default-directory (or dir default-directory))
         (process-output (or output (format "*%s*" process-name))))
    (apply #'start-process process-name process-output command)))

(defun spawn-sync-process (command &optional output dir)
  (let* ((program (car command))
         (args (cdr command))
         (default-directory (or dir default-directory))
         (process-output (or output (format "*%s*" program))))
    (apply #'process-file program  nil process-output nil args)))


(defun spawn-external-terminal ()
  (let* ((kitty-cmd '("kitty" "--class" "tmuxin" "--listen-on=unix:@emacs-kitty-session"))
         ;; (let* ((kitty-cmd '("kitty" "-T" "emacs-managed" "--listen-on=unix:@emacs-kitty-session" "-e" "tmux" "new" "-s" "default"))
         (terminal-proc (spawn-async-process kitty-cmd nil "/home/danilo/workspace/youse")))
    (setq tmuxin-terminal-up t)
    (setq tmuxin-term-proc terminal-proc)
    (set-process-sentinel
     terminal-proc
     (lambda (process event)
       (message "Process: %s had the event '%s'" process event)
       (when (eq (process-status process) 'exit)
         (clean-state-variables))))
    (set-process-filter terminal-proc #'ordinary-insertion-filter)))

(defun clean-state-variables ()
  (setq tmuxin-terminal-up nil)
  (setq tmuxin-term-shell-tty-id nil))


;; ;; Kitten Commands
;; 

(defun kitten:exec-async (command-to-send)
  (let* (
         (remote-command-sender '("kitten" "@" "--to=unix:@emacs-kitty-session"))
         (command (append remote-command-sender command-to-send)))
    (message "Kitten command: %s" command)
    (spawn-async-process command nil default-directory)))

(defun kitten:exec-sync (command-to-send &optional output)
  (let* (
         (remote-command-sender '("kitten" "@" "--to=unix:@emacs-kitty-session"))
         (command (append remote-command-sender command-to-send)))
    (message "Kitten command: %s" command)
    (spawn-sync-process command output default-directory)))

(defun kitten:send-text (text)
  (kitten:exec-sync (list "send-text" (concat text "\n"))))

(defun kitten:send-keybind (text)
  (kitten:exec-sync (list "send-key" text)))

;; The client of tmux is o tty root of kitty windows UAU!
(defun kitten:get-tty-id ()
  (kitten:exec-sync (list "send-text" (format "echo \"shell-tty $(tty)\" > %s\n" (process-tty-name tmuxin-term-proc))))
  (with-temp-buffer
    (kitten:exec-sync (list "get-text" "--extent" "last_cmd_output") (buffer-name))
    (goto-char (point-min))
    (re-search-forward "^\\(.+\\)$" nil t)))

(defun kitten:get-tty ()
  ;; Perform via remote command (Kitten) a Shell command that sends through the terminal Stdin the active interactive shell identifier. Via Terminal Sentinel rescued this identifier
  ;; And we recorded in variabel tmuxin-term-shell-tty-id. For more details look at the 'Ordinary-Inserti-Filter
  ;; Extra: The TTY of the terminal process is different from the interactive shell.
  (kitten:exec-sync (list "send-text" (format "echo \"shell-tty $(tty)\" > %s\n" (process-tty-name tmuxin-term-proc)))))


(defun kitten:exit-tmux-session (text)
  (kitten-run-remote-command (list "send-key" "ctrl+a"))
  (kitten-run-remote-command (list "send-key" "d")))

(defun kitten:attach-tmux-session (session-name)
  (kitten-run-remote-command (list "send-text" (format "tmux attach-session -t %s\n" session-name))))

(defun kitten:start-tmux-session (session-name &optional attach)
  (let* ((attach-session-flag (or (when attach "-A") ""))
         (command (format "tmux new -s %s %s\n" session-name attach-session-flag)))
    (kitten:exec-async (list "send-text" command))))

;; The client of tmux is o tty root of kitty windows UAU!
(defun kitten:get-tmux-client-name ()
  (kitten-run-remote-command (list "send-text" "tmux display -p '#{client_tty}'\n")))

;; Tmux interface commands
;; 

(defun tmux:exec (tmux-cmd &optional output)
  (let* ((base-c (list "tmux"))
         (p-command (append base-c tmux-cmd)))
    (spawn-sync-process p-command output)))

(defun tmux:exits-session-p (session-name)
  (member session-name (tmux:get-sessions)))

(defun tmux:get-sessions ()
  (with-temp-buffer
    (tmux:exec '("list-sessions") (buffer-name))
    (goto-char (point-min))
    (let (sessions)
      (while (re-search-forward "^\\([^:]+\\):" nil t)
        (push (match-string-no-properties 1) sessions))
      sessions)))

(defun tmux:get-clients ()
  (with-temp-buffer
    (tmux:exec '("list-clients" "-F" "#{client_name}") (buffer-name))
    (goto-char (point-min))
    (let (clients)
      (while (re-search-forward "^\\(.+\\)$" nil t)
        (push (match-string-no-properties 1) clients))
      clients)))

(defun tmux:create-session(name)
  (tmux:exec (list "new" "-s" name "-d")))

(defun tmux:change-attached-session(session-name)
  (if (not (tmux:exits-session-p session-name))
      (tmux:create-session session-name))
  (tmux:exec (list "switch-client" "-c" tmuxin-term-shell-tty-id "-t" session-name)))

(defun tmux:term-connected()
  (cl-some (lambda (client) (equal client tmuxin-term-shell-tty-id)) (tmux:get-clients)))

(defun tmux:connect-session-p (session-name)
  (if (tmux:term-connected)
      (tmux:change-attached-session session-name)
    (kitten:start-tmux-session session-name t)))

;; Utils functions
;; 
(defun match-tty-name (string)
  (when (string-match "^\\([-a-z]+\\) \\([\/a-z1-9]+\\)$" string)
    (list (match-string 1 string) (match-string 2 string))))

(defun fill-terminal-tty-id (tty-id)
  (message "Filling terminal-tty-id with %s" tty-id)
  (setq tmuxin-term-shell-tty-id tty-id))

(defun extract-tty-from-output-process (string)
  (let ((command (match-tty-name string)))
    (when (equal (car command) "shell-tty")
      (fill-terminal-tty-id (custom:last-elem command)))
    command))


(defun custom:last-elem (list)
  (nth (- (length list) 1) list))

(defun ordinary-insertion-filter (proc string)
  (message "Message received from process %s: %s" proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (extract-tty-from-output-process string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

;; Test integration
;; TODO: Create a new package based on the functions below
;; 


(defun tmuxin--project-test-command()
  (or (gethash (projectile-project-name) tmuxin-project-test-command-map)
      (puthash (projectile-project-name) (read-shell-command "Test command: ") tmuxin-project-test-command-map)))

;; REFERENCE (defun compilation-read-command (command)
;;             (read-shell-command "Test command: " command
;;                       (if (equal (car compile-history) command)
;;                           '(compile-history . 1)
;;                         'compile-history)))

(defun test:run-in-tmux-session(&optional args)
  (if (projectile-project-p)
      (tmux:change-attached-session (projectile-project-name)))
  (test:run args))

(defun test:run(&optional test-args)
  (let* ((test-args (if test-args (concat " " test-args) ""))
         (test-command (concat (tmuxin--project-test-command) test-args)))
    (kitten:send-text "hyprctl dispatch focuswindow class:tmuxin\n")
    (kitten:send-text test-command)
    (setq test-last-comand-executed test-command)))

;; Function copied from .emacs.d/layers/+spacemacs/spacemacs-defaults/funcs.el:766
(defun tmuxin--file-path ()
  "Retrieve the file path of the current buffer.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (buffer-file-name))
    (file-truename file-path)))

;; Function copied from .emacs.d/layers/+spacemacs/spacemacs-defaults/funcs.el:775
(defun tmuxin--file-path-with-line ()
  "Retrieve the file path of the current buffer, including line number.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (tmuxin--file-path))
    (concat file-path ":" (number-to-string (line-number-at-pos)))))


;; Next Steps
;;     Resgatar o indentificador tty do terminal kitty. Parece rasuavel executar essa acao apos o terminal ser criado
;;     O valor de tty do terminal eh muito importante. O tmux usa esse valor para indentificar os clients de cada sessao.
;;     Com esse valor, podemos, por exemplo, mudar a sessao tmux do terminal kitty que criamos de forma indireta atraves do
;;     subcomando do tmux `switch-client'.
;;     Comecei uma funcao para executar essa acao, o nome dela eh kitten-run-remote-command--get-tty-id. Parei de trabalhar nela
;;     quando me deparei com desafio de executar o comando no kitty aberto atraves do kitten de `get-text' apos executar `send-text
;;     "tty\n". Executar esse comando pelo metood `kitten-run-remote-command` parece nao ser viavel por que ele eh executado
;;     async, logo nao temos acesso imediato ao output do comando passado ao chamar essa funcao.
;;     Possivel alternativa eh criar um versao sync de `kitten-run-remote-command`
(defun start-tmuxin()
  (interactive)
  (if (not tmuxin-terminal-up)
      (spawn-external-terminal))
  (sit-for 1)
  (kitten:get-tty)
  (tmux:connect-session-p tmuxin-default-session))

(defun test:run-current-scenario()
  (interactive)
  (test:run-in-tmux-session (tmuxin--file-path-with-line)))

(defun test:run-current-file()
  (interactive)
  (test:run-in-tmux-session (tmuxin--file-path)))

(start-tmuxin)
;; (open-external-terminal)
;; (process-tty-name tmuxin-term-proc)
;; (spawn-external-terminal)

;; (sit-for)
;; (accept-process-output)

;; (waiting-for-user-input-p)
