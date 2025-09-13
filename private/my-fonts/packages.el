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
    modus-themes)
  "The list of Lisp packages required by the my-fonts layer.")

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
    ;; (text-scale-remap-header-line t)
    ;; This is the default value.  Just including it here for completeness.
    (fontaine-latest-state-file (locate-user-emacs-file "fontaine-latest-state.eld"))
    (fontaine-faces
     '( default fixed-pitch fixed-pitch-serif variable-pitch
        mode-line-active mode-line-inactive header-line
        line-number tab-bar tab-line variable-pitch-text
        bold italic))

    ;; Set my custom font presets
    (fontaine-presets
     '(
       (regular
        :variable-pitch-text-height 1.0 ;; by defaults its height is 1.1x the value of variable-pitch. But text is very big

        ;; Space Mono was the only font I found installed that contains all the special characters used by doom modeline
        :mode-line-active-family "SpaceMono Nerd Font"
        :mode-line-active-font "SpaceMono Nerd Font"
        :mode-line-active-weight regular
        :mode-line-active-slant normal
        :mode-line-active-width nil
        :mode-line-active-height 115

        :mode-line-inactive-family "SpaceMono Nerd Font"
        :mode-line-inactive-font "SpaceMono Nerd Font"
        :mode-line-inactive-weight regular
        :mode-line-inactive-slant normal
        :mode-line-inactive-width nil
        :mode-line-inactive-height 115) ; like this it uses all the fallback values and is named `regular'

       (medium
        :default-family "Monaspace Neon"
        :default-height 115
        :default-weight regular

        :fixed-pitch-family "Monaspace Neon"
        :fixed-pitch-weight regular
        :fixed-pitch-height 115

        :variable-pitch-family "Iosevka Comfy Wide Duo"
        :variable-pitch-height 120

        :inherit regular)

       (MonoLisa
        :default-family "MonoLisa Nerd Font Mono"
        :default-height 120
        :default-weight regular

        :fixed-pitch-family "MonoLisa Nerd Font Mono"
        :fixed-pitch-weight regular
        :fixed-pitch-height 120

        :variable-pitch-family "Iosevka Comfy Wide Duo"
        :variable-pitch-height 120)

       (DejavuSansMono
        :default-family "Dejavu Sans Mono"
        :default-height 120
        :default-weight regular

        :fixed-pitch-family "Dejavu Sans Mono"
        :fixed-pitch-weight regular
        :fixed-pitch-height 120

        :variable-pitch-family "Iosevka Comfy Wide Duo"
        :variable-pitch-height 120)

       (JetBrainsMono
        :default-family "JetBrainsMonoNL Nerd Font"
        :default-height 120
        :default-weight regular

        :fixed-pitch-family "JetBrainsMono Nerd Font"
        :fixed-pitch-weight regular
        :fixed-pitch-height 120

        :variable-pitch-family "Iosevka Comfy Wide Duo"
        :variable-pitch-height 120)

       (SourceCode
        :default-family "Source Code Pro"
        :default-height 120
        :default-weight regular

        :fixed-pitch-family "Source Code Pro"
        :fixed-pitch-weight regular
        :fixed-pitch-height 120

        :variable-pitch-family "Iosevka Comfy Wide Duo"
        :variable-pitch-height 120)

       (SourceCode
        :default-family "Source Code Pro"
        :default-height 120
        :default-weight regular

        :fixed-pitch-family "Source Code Pro"
        :fixed-pitch-weight regular
        :fixed-pitch-height 120

        :variable-pitch-family "Iosevka Comfy Wide Duo"
        :variable-pitch-height 120)

       (FiraCode
        :default-family "Fira Code"
        :default-height 120
        :default-weight regular

        :fixed-pitch-family "Fira Code"
        :fixed-pitch-weight regular
        :fixed-pitch-height 120

        :variable-pitch-family "Iosevka Comfy Wide Motion"
        :variable-pitch-height 110)

       (MonaspaceNeon
        :default-family "Monaspace Neon"
        :default-height 120
        :default-weight regular

        :fixed-pitch-family "Monaspace Neon"
        :fixed-pitch-weight regular
        :fixed-pitch-height 120

        :variable-pitch-family "Iosevka Comfy Wide Motion"
        :variable-pitch-height 120)

       (NaruMonoDemo
        :default-family "Naru Mono Demo"
        :default-height 120
        :default-weight bold

        :fixed-pitch-family "Naru Mono Demo"
        :fixed-pitch-weight bold
        :fixed-pitch-height 120

        :variable-pitch-family "Iosevka Comfy Wide Duo"
        :variable-pitch-height 100)

       (mode-line
        :default-family "MonoLisa Nerd Font Mono"
        :default-height 130
        :default-weight regular

        :fixed-pitch-family "MonoLisa Nerd Font Mono"
        :fixed-pitch-weight regular
        :fixed-pitch-height 130

        :variable-pitch-family "Iosevka Comfy Wide Duo"
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
                     ;; Set last preset or fall back to desired style from `fontaine-presets'.
                     (fontaine-set-preset (or (fontaine-restore-latest-preset) 'medium)))))
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
    (add-hook 'enable-theme-functions (lambda () (fontaine-set-preset 'medium)))

    ;; Makes comments text and keywords italics
    ;; This is working in emacsclient but not emacs.
    ;; Your font must have ana italic available.
    (set-face-attribute 'font-lock-comment-face nil
                        ;; :font "Iosevka Comfy Wide Motion-12"
                        :font "Iosevka Comfy Wide Duo"
                        :slant 'italic
                        :weight 'regular
                        ;; :foreground "#0ec9ea"
                        ;; :background "#1A1B26"
                        ) ;; Fundo claro para comentários (opcional)
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
    :config
    ;; (modus-themes-select 'modus-vivendi-tinted)

    ))
