;;; early-init.el --- Spacemacs Early Init File -*- no-byte-compile: t; lexical-binding: nil; -*-
;;
;; Copyright (c) 2020-2025 Sylvain Benner & Contributors
;;
;; Author: Miciah Dashiel Butler Masters <miciah.masters@gmail.com>
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


;; Before Emacs 27, the init file was responsible for initializing the package
;; manager by calling `package-initialize'. Emacs 27 changed the default
;; behavior: It now calls `package-initialize' before loading the init file.
;; This behavior would prevent Spacemacs's own package initialization from
;; running. However, Emacs 27 also loads the "early init" file (this file)
;; before it initializes the package manager, and Spacemacs can use this early
;; init file to prevent Emacs from initializing the package manager. (See
;; <http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=24acb31c04b4048b85311d794e600ecd7ce60d3b>.)
;;
;; Earlier Emacs versions do not load the early init file and do not initialize
;; the package manager before loading the init file, so this file is neither
;; needed nor loaded on those versions.
(setq package-enable-at-startup nil)


;; Enables the compilation of the lsp-mode with plist instead of hash-table(default).
;; plist provide better performance in deserialization
(setenv "LSP_USE_PLISTS" "true")

(load (concat (file-name-directory load-file-name)
              "core/core-early-funcs")
      nil (not init-file-debug))

;; Unfortunately the hooks below prevent users from customizing gui elements
;; within dotspacemacs/user-config function. Thus the hooks are commented out.
;; These should not be needed in any case since gui elements are turned off
;; within spacemacs/init function.  Original comment follows next.
;;
;; Remove GUI elements soon after GUI being initialized to avoid some possible
;; grapical glitches. This has to be done use these hooks, see
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html
;;
;; (add-hook 'window-setup-hook 'spacemacs/toggle-gui-elements-off)
;; (add-hook 'tty-setup-hook 'spacemacs/toggle-gui-elements-off)


;; Native compilation tuning
;;
;; Author: James Cherti
;; URL: https://www.jamescherti.com/compiling-emacs/
;; License: MIT
;;
;; `native-comp-compiler-options' are the flags passed directly to the C
;; compiler (via libgccjit) when it turns the Lisp-to-C output of the native
;; compiler into machine code for every .eln file, on top of whatever CFLAGS
;; the Emacs binary itself was built with.
;;
;; Deliberately avoid the literal "native" keyword for -march/-mtune here:
;; libgccjit is loaded in-process (not spawned as the gcc driver), and its
;; embedded host-detection logic doesn't always resolve "native" the same
;; way the standalone gcc binary does. So we ask gcc directly what "native"
;; resolves to on this machine and pass that concrete architecture instead.
(defun my-get-cpu-architecture ()
  "Return the CPU architecture GCC resolves `-march=native' to on this host.
The value is obtained by parsing the output of GCC's `--help=target'.
Return nil if GCC is unavailable or the architecture can't be parsed."
  (when (executable-find "gcc")
    (with-temp-buffer
      (let ((exit-code (call-process "gcc" nil t nil "-march=native"
                                      "-Q" "--help=target")))
        (when (zerop exit-code)
          (goto-char (point-min))
          (when (re-search-forward
                 "^[[:space:]]*-march=[[:space:]]+\\([^[:space:]]+\\)" nil t)
            (match-string 1)))))))

;; Auto-detect the CPU architecture so this keeps working if this config
;; moves to a different machine. x86 only: on aarch64 GCC reports an ISA
;; string (e.g. armv8.2-a+crypto) that `-mtune' rejects, which would make
;; every native compilation fail.
(when-let* (((string-prefix-p "x86_64" system-configuration))
            (cpu-architecture (my-get-cpu-architecture)))
  (setq native-comp-compiler-options
        `(;; The most meaningful optimizations.
          "-O2"
          ,(format "-mtune=%s" cpu-architecture)
          ,(format "-march=%s" cpu-architecture)
          ;; Reduce .eln size and compilation overhead.
          "-g0"
          ;; Good defensive choice for Emacs stability (see bug#76180).
          "-fno-omit-frame-pointer"
          "-fno-finite-math-only")))

(setq native-comp-driver-options
      '(;; Compresses relocation tables to reduce file size and slightly
        ;; improve load times.
        "-Wl,-z,pack-relative-relocs"
        ;; Standard linker-level optimizations (e.g. string merging) applied
        ;; to the generated shared object.
        "-Wl,-O2"
        ;; Don't record dependencies on libraries the .eln doesn't actually
        ;; use.
        "-Wl,--as-needed"))

;; NEXT STEP: bring in `compile-angel' to force byte+native compilation of
;; everything in `load-path', including the private/ layers.
