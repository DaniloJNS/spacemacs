;;; packages.el --- my-fonts layer packages file for Spacemacs.
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

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-fonts-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-fonts/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-fonts/pre-init-PACKAGE' and/or
;;   `my-fonts/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-fonts-packages
  '(
    fontaine
    face-remap
    mixed-pitch
    doom-themes
    ligature
    olivetti
    (everforest-theme :location (recipe
                                 :fetcher github
                                 :repo "theorytoe/everforest-emacs"))
    catppuccin-theme
    modus-themes
    ef-themes)
  "The list of Lisp packages required by the my-fonts layer.")

(defun my-fonts/init-everforest-theme()
  (use-package everforest-theme
    :ensure t
    :defer t
    :config
    (load-theme 'everforest-hard-dark t)
    ))

(defun my-fonts/init-catppuccin-theme()
  (use-package catppuccin-theme
    :ensure t
    :config
    (setq catppuccin-flavor 'mocha)))

(defun my-fonts/init-ef-themes()
  (use-package ef-themes
    :ensure t
    :init
    ;; This makes the Modus commands listed below consider only the Ef
    ;; themes.  For an alternative that includes Modus and all
    ;; derivative themes (like Ef), enable the
    ;; `modus-themes-include-derivatives-mode' instead.  The manual of
    ;; the Ef themes has a section that explains all the possibilities:
    ;;
    ;; - Evaluate `(info "(ef-themes) Working with other Modus themes or taking over Modus")'
    ;; - Visit <https://protesilaos.com/emacs/ef-themes#h:6585235a-5219-4f78-9dd5-6a64d87d1b6e>
    (ef-themes-take-over-modus-themes-mode 1)
    :bind
    (("<f5>" . modus-themes-rotate)
     ("C-<f5>" . modus-themes-select)
     ("M-<f5>" . modus-themes-load-random))
    :config
    ;; All customisations here.
    (setq modus-themes-mixed-fonts t)
    (setq modus-themes-italic-constructs t)

    ;; Finally, load your theme of choice (or a random one with
    ;; `modus-themes-load-random', `modus-themes-load-random-dark',
    ;; `modus-themes-load-random-light').
    (modus-themes-load-theme 'ef-dark)) )

(defun my-fonts/init-olivetti()
  (use-package olivetti
    :ensure t
    :defer t
    :custom
    (olivetti-body-width 110)
    ;; :config
    ;; (defun my-setup-org-zen-mode ()
    ;;   "Configurações de visual para Org-mode em monitores Ultrawide."
    ;;   (interactive)

    ;;   ;; 2. Ativa o Olivetti (centralização)
    ;;   (olivetti-mode 1)

    ;;   ;; 4. Esconde a linha de números (opcional, deixa mais limpo)
    ;;   (display-line-numbers-mode -1))

    ;; ;; Aplica a função sempre que abrir um arquivo Org
    ;; (add-hook 'org-mode-hook 'my-setup-org-zen-mode)
    )
  )

(defun my-fonts/init-ligature()
  (use-package ligature
    :config
    ;; Habilita as ligaduras clássicas da MonoLisa para todos os modos de programação
    (ligature-set-ligatures 'prog-mode
                            '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
                              "/>" "/*" "//" "::" ":::" "::=" ":=" ":>" ":<" "===" "==" "=>" "=>>"
                              "=:=" "=/=" "!==" "!=" "!!" "!!." ">=" ">>=" ">>>" ">>" ">:" ">-"
                              ">=>" ">->" "<=" "<==" "<=>" "<=<" "<->" "<--" "<-" "<-<" "<<="
                              "<<<" "<<" "<~" "<~~" "<~>" "<*" "<*>" "<|" "<|>" "<||" "<|||"
                              "<$" "<$>" "<+" "<+>" "</" "</>")))
  (global-ligature-mode))

(defun my-fonts/init-doom-themes()
  (use-package doom-themes
    :ensure t
    :custom
    (doom-themes-enable-bold nil)
    (doom-themes-padded-modeline t)
    :config
    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; Enable custom neotree theme (nerd-icons must be installed!)
    ;; (doom-themes-neotree-config)
    ;; or for treemacs users
    ;; (doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)))

(defun my-fonts/init-mixed-pitch ()
  (use-package mixed-pitch
    :hook
    (org-mode           . mixed-pitch-mode)
    (org-roam-mode      . mixed-pitch-mode)
    :config
    ;; (setq mixed-pitch-set-height t
    ;;       variable-pitch-serif-font doom-variable-pitch-font)

    ;; 'font-latex-math-face
    (dolist (face '('org-drawer 'org-cite-key 'org-list-dt 'org-hide) mixed-pitch-fixed-pitch-faces)
      (cl-pushnew face mixed-pitch-fixed-pitch-faces :test #'equal))))

(defun my-fonts/init-fontaine ()
  ;; Fontaine (font configurations)
  ;; Read the manual: <https://protesilaos.com/emacs/fontaine>
  (use-package fontaine
    ;; Load only if emacs is running in GUI mode
    :if (display-graphic-p)
    :custom
    ;; This is defined in Emacs C code: it belongs to font settings.
    (x-underline-at-descent-line nil)
    ;; And this is for Emacs28.
    (text-scale-remap-header-line t)
    ;; This is the default value.  Just including it here for completeness.
    (fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))
    (fontaine-faces
     '( default fixed-pitch fixed-pitch-serif variable-pitch
        mode-line-active mode-line-inactive header-line
        line-number variable-pitch-text
        bold italic))

    ;; Set my custom font presets
    (fontaine-presets
     '(
       (regular
        :variable-pitch-text-height 1.1 ;; by defaults its height is 1.1x the value of variable-pitch. But text is very big

        ;; Space Mono was the only font I found installed that contains all the special characters used by doom modeline
        ;; :mode-line-active-family "SpaceMono Nerd Font"
        ;; :mode-line-active-font "SpaceMono Nerd Font"
        :mode-line-active-family "JetBrainsMono Nerd Font"
        :mode-line-active-weight regular
        :mode-line-active-slant normal
        :mode-line-active-width nil
        :mode-line-active-height 120

        :mode-line-inactive-family "JetBrainsMono Nerd Font"
        :mode-line-inactive-weight regular
        :mode-line-inactive-slant normal
        :mode-line-inactive-width nil
        :mode-line-inactive-height 120) ; like this it uses all the fallback values and is named `regular'
       (medium
        :default-family "Monolisa"
        :default-height 125
        :default-weight regular

        :fixed-pitch-family "Monolisa"
        :fixed-pitch-weight regular
        :fixed-pitch-height 125

        ;; :variable-pitch-text-height 1.1 ;; by defaults its height is 1.1x the value of variable-pitch. But text is very big
        ;; :variable-pitch-family "Iosevka Comfy Wide Duo"
        ;; :variable-pitch-family "IBM Plex Sans"
        ;; :variable-pitch-family "Source Sans Pro"
        :variable-pitch-family "Inter"
        :variable-pitch-weight regular
        :variable-pitch-height 125

        :inherit regular)

       (MonoLisa
        :default-family "MonoLisa"
        :default-height 120
        :default-weight regular

        :fixed-pitch-family "MonoLisa"
        :fixed-pitch-weight regular
        :fixed-pitch-height 120

        ;; :variable-pitch-text-height 1.1 ;; by defaults its height is 1.1x the value of variable-pitch. But text is very big
        ;; :variable-pitch-family "Iosevka Comfy Wide Duo"
        ;; :variable-pitch-family "IBM Plex Sans"
        ;; :variable-pitch-family "Source Sans Pro"
        :variable-pitch-family "Inter"
        :variable-pitch-weight regular
        :variable-pitch-height 125

        :inherit regular)

       (JetBrainsMono
        :default-family "JetBrainsMonoNL Nerd Font"
        :default-height 120
        :default-weight regular

        :fixed-pitch-family "JetBrainsMono Nerd Font"
        :fixed-pitch-weight regular
        :fixed-pitch-height 120

        :variable-pitch-family "Iosevka Comfy Wide Duo"
        :variable-pitch-height 120)

       (MonaspaceNeon
        :default-family "Monaspace Neon"
        :default-height 115
        :default-weight regular

        :fixed-pitch-family "Monaspace Neon"
        :fixed-pitch-weight regular
        :fixed-pitch-height 115

        :variable-pitch-family "Inter"
        :variable-pitch-weight regular
        :variable-pitch-height 120)

       (large
        :inherit medium
        :default-height 150)

       (live-stream
        :default-family "Iosevka Comfy Wide Motion"
        :default-height 150
        :default-weight medium

        :fixed-pitch-family "Iosevka Comfy Wide Motion"

        :variable-pitch-family "Iosevka Comfy Wide Duo"

        :bold-weight extrabold)

       (presentation
        :default-height 180)

       (jumbo
        :default-height 260)
       )
     )
    :hook
    ;; Persist the latest font preset when closing/starting Emacs and
    ;; while switching between themes.
    ((after-init . fontaine-mode)
     (after-init . (lambda ()
                     ;; Wrap in with-eval-after-load to guarantee fontaine-presets has been
                     ;; populated (via :custom) before calling fontaine-set-preset.  Without
                     ;; this, the lambda can run before fontaine loads, leaving fontaine-presets
                     ;; at its default value (which does not include the 'medium preset).
                     (with-eval-after-load 'fontaine
                       ;; Set last preset or fall back to desired style from `fontaine-presets'.
                       (fontaine-set-preset (or (fontaine-restore-latest-preset) 'medium))))))
    :bind (("C-c f" . fontaine-set-preset)
           ("C-c F" . fontaine-toggle-preset))
    :config
    ;; This sets the default font on all graphical frames created after restarting Emacs
    ;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
    ;; are not right unless I also add this method of setting the default font
    ;; (add-to-list 'default-frame-alist '(font . "Space Mono Nerd Font-11"))
    ;; Checks if is secure remove this function

    ;; Persist the latest font preset when closing/starting Emacs and
    ;; while switching between themes.
    ;; (add-hook 'enable-theme-functions #'fontaine-apply-current-preset)
    ;; (add-hook 'enable-theme-functions (lambda () (fontaine-set-preset 'medium)))

    ;; Re-apply theming-modifications after fontaine runs (after-init), so
    ;; vertical-border, window-divider, mode-line customizations etc. are not
    ;; overwritten by fontaine's face application.

    ;; Makes comments text and keywords italics
    ;; This is working in emacsclient but not emacs.
    ;; Your font must have ana italic available.
    ))

(defun my-fonts/init-face-remap()
  (use-package face-remap
    :ensure nil
    :if (display-graphic-p)
    :functions prot/enable-variable-pitch
    :bind ( :map ctl-x-x-map
            ("v" . variable-pitch-mode))
    :hook ((text-mode notmuch-show-mode elfeed-show-mode) . prot/enable-variable-pitch)
    :config
    ;; NOTE 2022-11-20: This may not cover every case, though it works
    ;; fine in my workflow.  I am still undecided by EWW.
    (defun prot/enable-variable-pitch ()
      (unless (derived-mode-p 'mhtml-mode 'nxml-mode 'yaml-mode)
        (variable-pitch-mode 0)))

    (setq-default truncate-lines t)
    ;;;;; Resize keys with global effect
    :bind
    ;; Emacs 29 introduces commands that resize the font across all
    ;; buffers (including the minibuffer), which is what I want, as
    ;; opposed to doing it only in the current buffer.  The keys are the
    ;; same as the defaults.
    (("C-x C-=" . global-text-scale-adjust)
     ("C-x C-+" . global-text-scale-adjust)
     ("C-x C-0" . global-text-scale-adjust))))

(defun my-fonts/init-modus-themes ()
  (use-package modus-themes
    :ensure t
    :init

    ;; The themes are highly customisable.  Read the manual:
    ;; <https://protesilaos.com/emacs/modus-themes>.
    (setq modus-themes-custom-auto-reload nil
          modus-themes-to-toggle '(modus-operandi modus-vivendi)
          ;; modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)
          ;; modus-themes-to-toggle '(modus-operandi-deuteranopia modus-vivendi-deuteranopia)
          ;; modus-themes-to-toggle '(modus-operandi-tritanopia modus-vivendi-tritanopia)
          modus-themes-mixed-fonts t
          modus-themes-variable-pitch-ui t
          modus-themes-italic-constructs t
          modus-themes-bold-constructs nil
          modus-themes-completions '((t . (extrabold)))
          modus-themes-prompts '(extrabold)
          modus-themes-common-palette-overrides nil
          modus-themes-headings
          '((agenda-structure . (variable-pitch light 2.2))
            (agenda-date . (variable-pitch regular 1.3))
            (t . (regular 1.15))))
    (setq modus-vivendi-tinted-palette-overrides
          '((bg-main "#1e1e1e")     ; Eleva o fundo para evitar smearing profundo
            (fg-main "#ffffff")     ; Garante contraste máximo para caracteres
            (comment "#a0a0a0")     ; Torna comentários mais visíveis
            (string "#70b900")))    ; Verde vibrante mas legível
    :config
    ;; (modus-themes-select 'modus-vivendi-tinted)
    ))
