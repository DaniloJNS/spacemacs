# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Repository Is

This is a personal fork of [Spacemacs](https://spacemacs.org) — a community-driven Emacs distribution that layers configuration on top of Emacs. The `develop` branch is the main upstream branch; the `personal` branch contains user customizations.

## Running Tests

Tests use ERT (Emacs Regression Testing) and are run via `make` from a test subdirectory:

```sh
# Run all core tests
cd tests/core && make

# Run all tests for a specific layer
cd tests/layers/+lang/python && make

# Run tests for spacemacs-base distribution
cd tests/layers/+distribution/spacemacs-base && make
```

The `spacemacs.mk` file at the root defines the shared Makefile logic. Each test directory has its own `Makefile` that includes it and sets `TEST_DIR`, `LOAD_FILES`, `UNIT_TEST_FILES`, and `FUNC_TEST_FILES`.

Test files are named `*-utest.el` (unit tests) or `*-ftest.el` (functional tests).

## Architecture

### Core (`core/`)
The heart of Spacemacs. Key files:
- `core-configuration-layer.el` — layer loading, package management, the `configuration-layer/` API
- `core-spacemacs.el` — main entry point, `spacemacs/init`
- `core-dotspacemacs.el` — user configuration (`dotspacemacs-*` variables)
- `core-funcs.el` — shared utility functions (`spacemacs/` prefix)
- `core-keybindings.el` — base keybinding infrastructure
- `core-load-paths.el` — defines `spacemacs-*-directory` path constants

Startup flow: `init.el` → `core-load-paths` → `core-spacemacs` → `spacemacs/init` → `configuration-layer/load`

### Layers (`layers/`)
Organized by category with `+` prefix (e.g. `+lang/`, `+tools/`, `+checkers/`). Each layer directory contains:
- `packages.el` — declares packages (`<layer>/packages` list) and their `init`/`pre-init`/`post-init` functions
- `funcs.el` — helper functions used by the layer
- `config.el` — layer-scoped variables and configuration
- `keybindings.el` — keybindings (not tied to package loading)
- `layers.el` — layer metadata and dependencies

### Private Layers (`private/`)
User-specific layers not tracked by upstream git. This repo has:
- `my-defaults/` — personal defaults, extra packages (org-roam, dirvish, tree-sitter text objects, etc.)
- `my-fonts/` — font configuration (fontaine)
- `my-navigation/` — navigation customizations

### Layer Naming Conventions
- Functions: `<layer-name>/init-<package>`, `<layer-name>/pre-init-<package>`, `<layer-name>/post-init-<package>`
- Configuration variables: `<layer-name>-<variable-name>`
- Spacemacs core functions use `spacemacs/` or `spacemacs//` (private) prefixes

## Contribution Guidelines

- All PRs target the `develop` branch (never `master`)
- One topic per PR, one commit per PR for simple changes
- Commit messages: imperative present tense, capitalized, ≤72 chars summary
- Every Elisp file must include the GPLv3 license header with author/URL
- Add changelog entries to `CHANGELOG.develop` for non-trivial changes

## Key Design Patterns

**Package initialization:** Packages are never `require`'d directly in layer files. Use `use-package` inside `init-<package>` functions. Spacemacs autoloads everything possible.

**Layer variables:** Declared with `defvar` in `config.el` or at the top of `packages.el`. Use `dotspacemacs-configuration-layer-variables` in the user dotfile to customize them.

**Cross-layer hooks:** Use `spacemacs/add-to-hook` and `with-eval-after-load` for cross-layer dependencies rather than direct requires.

**Keybindings:** Use `spacemacs/set-leader-keys` for `SPC` bindings, `spacemacs/set-leader-keys-for-major-mode` for mode-specific `SPC m` bindings.
