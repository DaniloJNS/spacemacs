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
    (elysium :toggle llm-client-enable-gptel)
    (evedel :toggle llm-client-enable-gptel)
    (ai-org-chat :location (recipe :fetcher github :repo "ultronozm/ai-org-chat.el"))
    aidermacs
    ;; (gptel-quick :toggle llm-client-enable-gptel)
    org
    window-purpose))

;; (defun llm-client/init-gptel-quick ()
;;   (use-package gptel-quick
;;     :defer t
;;     :ensure t))


(defun llm-client/init-aidermacs()
  (use-package aidermacs
    :bind (("C-c a" . aidermacs-transient-menu))
    :init
    (spacemacs/set-leader-keys "aa" 'aidermacs-transient-menu) ; Start aider contentx
    :custom
    ;; Chustes models config
    ;; (aidermacs-default-chat-mode 'architect)
    ;; (aidermacs-default-model "openai/nvidia/Llama-3_3-Nemotron-Super-49B-v1")
    ;; (aidermacs-architect-model "openai/deepseek-ai/DeepSeek-R1-0528")
    ;; (aidermacs-editor-model "openai/Qwen/Qwen2.5-Coder-32B-Instruct")

    ;; Openrouter models config
    ;; (setq aidermacs-architect-model "openrouter/deepseek/deepseek-r1-0528:free")
    ;; (setq aidermacs-editor-model "openrouter/deepseek/deepseek-r1-0528:free")
    ;; (setq aidermacs-default-model "openrouter/deepseek/deepseek-r1-0528:free")

    ;; (setq aidermacs-default-model "openrouter/moonshotai/kimi-k2:free")
    ;; (setq aidermacs-architect-model "openrouter/moonshotai/kimi-k2:free")
    ;; (setq aidermacs-editor-model "openai/moonshotai/kimi-k2:free")

    (aidermacs-default-model "openrouter/qwen/qwen3-coder:free")
    (aidermacs-architect-model "openrouter/qwen/qwen3-coder:free")
    (aidermacs-editor-model "openrouter/qwen/qwen3-coder:free")

    ;; (aidermacs-default-model "openrouter/moonshotai/kimi-k2:free")
    ;; (aidermacs-architect-model "openrouter/moonshotai/kimi-k2:free")
    ;; (aidermacs-editor-model "openai/moonshotai/kimi-k2:free")
    :config

    ;; (setenv "DEEPSEEK_API_BASE" "https://openrouter.ai/api/v1/chat/completions")
    (setenv "OPENROUTER_API_KEY" "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")
    ;; (setenv "OPENAI_API_BASE" "https://llm.chutes.ai/v1")
    ;; (setenv "OPENAI_API_KEY" "cpk_b9a0034e78794e68b2660a4fdc81b04d.200768288fa15e29b2a6f7bc4dc510bd.d2Q7CuqL3dhF9yub5WPFvgC0aTxswV01")
    )
  )

(defun llm-client/init-ai-org-chat ()
  (use-package ai-org-chat
    :bind
    (:map global-map
          ("C-c /" . ai-org-chat-new))
    (:map ai-org-chat-minor-mode-map
          ("C-c <return>" . ai-org-chat-respond))
    :custom
    (ai-org-chat-user-name "Paul")
    (ai-org-chat-dir "~/ai-chats")  ; Directory for saving chat files
    :config
    (setq ai-org-chat-models (cons '("deepseek-r1"
                                     :package llm-openai
                                     :provider (lambda (&rest args)
                                                 (apply #'make-llm-openai-compatible
                                                        :url "https://llm.chutes.ai/v1"
                                                        args))
                                     :key-env "CHUTES_KEY"
                                     :chat-model "deepseek-ai/DeepSeek-R1")
                                   ai-org-chat-models))

    (ai-org-chat-select-model "deepseek-r1")))

(defun llm-client/init-evedel ()
  (use-package evedel
    :defer t
    :config
    (customize-set-variable 'evedel-empty-tag-query-matches-all nil)
    :bind (("C-c e r" . evedel-create-reference)
           ("C-c e d" . evedel-create-directive)
           ("C-c e s" . evedel-save-instructions)
           ("C-c e l" . evedel-load-instructions)
           ("C-c e p" . evedel-process-directives)
           ("C-c e m" . evedel-modify-directive)
           ("C-c e C" . evedel-modify-reference-commentary)
           ("C-c e k" . evedel-delete-instructions)
           ("C-c e c" . evedel-convert-instructions)
           ("C->"     . evedel-next-instruction)
           ("C-<"     . evedel-previous-instruction)
           ("C-."     . evedel-cycle-instructions-at-point)
           ("C-c e t" . evedel-add-tags)
           ("C-c e T" . evedel-remove-tags)
           ("C-c e D" . evedel-modify-directive-tag-query)
           ("C-c e P" . evedel-preview-directive-prompt)
           ("C-c e /" . evedel-directive-undo)
           ("C-c e ?" . (lambda ()
                          (interactive)
                          (evedel-directive-undo t))))))

(defun llm-client/init-ellama ()
  "Initialize the `ellama` package and set up keybindings."
  (use-package ellama
    :defer t
    :ensure t
    :init
    (spacemacs/declare-prefix "$" "AI")
    (spacemacs/declare-prefix "$e" "Ellama")
    ))

(defun llm-client/init-elysium ()
  (use-package elysium
    :defer t
    :ensure t
    :init
    (spacemacs/declare-prefix "ale" "Elysium - AI change made easy")
    (spacemacs/set-leader-keys
      "ales" 'elysium-query                          ; Send a query to the gptel backend
      "alek" 'elysium-keep-all-suggested-changes     ; Keep all of the AI-suggested changes
      "aled" 'elysium-discard-all-suggested-changes  ; Discard all of the AI-suggested changes
      "alec" 'elysium-clear-buffer                   ; Clear the elysium buffer
      "aled" 'elysium-add-context                    ; add the contents of a region to the elysium buffer
      "alet" 'elysium-toggle-window)))               ; toggle the chat window

(defun llm-client/init-gptel ()
  "Initialize the `gptel` package and set up keybindings."
  (use-package gptel
    :defer t
    :ensure t
    :init
    ;; evilify gptel-context-buffer-mode-map
    (require 'gptel-context)
    (evil-set-initial-state 'gptel-context-buffer-mode 'evilified)
    (evilified-state-evilify-map gptel-context-buffer-mode-map
      :mode gptel-context-buffer-mode
      :bindings
      "C-c C-c" #'gptel-context-confirm
      "C-c C-k" #'gptel-context-quit
      "RET"     #'gptel-context-visit
      "n"       #'gptel-context-next
      "p"       #'gptel-context-previous
      "d"       #'gptel-context-flag-deletion)
    ;; set up keybindings
    (spacemacs/declare-prefix "al" "LLM Client")
    (spacemacs/set-leader-keys
      "all" 'gptel                          ; Start a new GPTel session
      "als" 'spacemacs//gptel-send-wrapper  ; Send a message to GPTel
      "alq" 'spacemacs//gptel-abort-wrapper ; Abort any active GPTel process
      "alm" 'gptel-menu                     ; Open the GPTel menu
      "alc" 'gptel-add                      ; Add context
      "alf" 'gptel-add-file                 ; Add a file
      "alo" 'gptel-org-set-topic            ; Set topic in Org-mode
      "alp" 'gptel-org-set-properties
      "pw"  'spacemacs//open-project-llm-chat); Set properties in Org-mode
    :config
    (setq gptel-model   'deepseek-ai/DeepSeek-R1
          gptel-default-mode 'org-mode
          gptel-org-branching-context nil
          gptel-backend (gptel-make-openai "Chustes"               ;Any name you want
                          :host "llm.chutes.ai"
                          :endpoint "/v1/chat/completions"
                          :stream t
                          :key "cpk_b9a0034e78794e68b2660a4fdc81b04d.200768288fa15e29b2a6f7bc4dc510bd.d2Q7CuqL3dhF9yub5WPFvgC0aTxswV01"                   ;can be a function that returns the key
                          :models '(
                                    (nvidia/Llama-3_3-Nemotron-Super-49B-v1
                                     :description
                                     "open-source LLM optimized for reasoning tasks and efficient inference, making it suitable for AI agents, chatbots, and RAG applications"
                                     :request_params
                                     '(:top_p 0.90 :temperature 0.6))
                                    (deepseek-ai/DeepSeek-R1-0528
                                     :description
                                     "DeepSeek-R1 is a 671B parameter (37B activated) language model trained with reinforcement learning that excels at mathematical reasoning, coding, and complex problem-solving tasks, achieving performance comparable to OpenAI 01")
                                    (Qwen/Qwen2.5-Coder-32B-Instruct
                                     :description
                                     "code-focused language model that excels at programming tasks like code generation, reasoning, and fixing while maintaining strong general capabilities")
                                    (Qwen/Qwen3-235B-A22B
                                     :description
                                     "Qwen3-235B-A22B is a 235B parameter MoE language model (with 22B active parameters) that can switch between "thinking" and "non-thinking" modes, making it versatile for both complex reasoning tasks and efficient general"
                                     :request_params
                                     '(:top_p 0.95 :top_k 20 :min_p 0 :temperature 0.6))
                                    (chutesai/Mistral-Small-3.1-24B-Instruct-2503
                                     :description
                                     "Mistral Small 3.1 is a 24B parameter open-source language model with vision capabilities and 128k context that excels at reasoning, conversation, and programming tasks while being compact enough to run on consumer")
                                    (deepseek-ai/DeepSeek-V3-0324
                                     :description
                                     "DeepSeek-V3-0324 is an improved version of DeepSeek's language model with enhanced reasoning capabilities, stronger coding abilities, and better Chinese language proficiency, making it particularly suitable for complex")
                                    (nvidia/Llama-3_1-Nemotron-Ultra-253B-v1
                                     :description
                                     "Llama-3.1-Nemotron-Ultra-253B is a 253 billion parameter reasoning-focused language model optimized for efficiency that excels at math, coding, and general instruction-following tasks while running on a single 8xH100"
                                     :request_params
                                     '(:top_p 0.95))
                                    deepseek-ai/DeepSeek-R1-Zero
                                    tngtech/DeepSeek-R1T-Chimera
                                    (chutesai/Llama-4-Maverick-17B-128E-Instruct-FP8
                                     :description
                                     "Meta's Llama 4 is a 17 billion parameter multimodal AI model (with up to 400B total parameters using mixture-of-experts architecture) that can understand both text and images while generating text responses, making it useful")
                                    chutesai/Llama-3.1-405B-FP8
                                    ;; Code tasks
                                    (agentica-org/DeepCoder-14B-Preview
                                     :description
                                     "Code reasoning LLM fine-tuned from DeepSeek-R1-Distilled-Qwen-14B using distributed reinforcement learning (RL) to scale up to long context lengths")
                                    (microsoft/MAI-DS-R1-FP8
                                     :description
                                     "MAI-DS-R1 is a DeepSeek-R1 reasoning model that has been post-trained by the Microsoft AI team to improve its responsiveness on blocked topics and its risk profile, while maintaining its reasoning capabilities and competitive performance.")
                                    Qwen/Qwen2.5-VL-32B-Instruct)))
    (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
    (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
    (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "** Prompt\n"
          (alist-get 'org-mode gptel-response-prefix-alist) "=Response=\n")))

;;

(defun llm-client/post-init-org ()
  "Set up Org-mode keybindings for GPTel."
  (spacemacs/declare-prefix-for-mode 'org-mode "m$g" "Gptel")
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
      (apply orig-func args)
      (when purpose-mode-was-enabled
        (purpose-mode 1))))
  (advice-add 'gptel :around #'llm-client/disable-purpose-mode-around-for-gptel))
