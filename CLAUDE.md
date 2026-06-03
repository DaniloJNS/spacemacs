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
---

## ⚠️ MANDATORY RULES (MUST FOLLOW)

These rules are **absolute** and must be followed in ALL interactions:

### 0. 🔴 TOP PRIORITY: ALWAYS use Cortex MCP Tools

**This is the most important rule of all.** ALWAYS prefer Cortex MCP tools (`mcp__cortex__*`) over CLI, Bash, Glob, Grep, or any other tool for the following operations:

| Operation | MCP Tool (USE) | Alternative (DO NOT USE) |
|-----------|----------------|--------------------------|
| Create task | `task_create` | ~~cx add via Bash~~ |
| View task | `task_get` | ~~cx show via Bash~~ |
| List tasks | `task_list` | ~~cx ls via Bash~~ |
| Update task | `task_update` | ~~cx mv via Bash~~ |
| Project status | `mcp__cortex__status()` | ~~cx status via Bash~~ |
| Get memory by ID | `memory_get` | ~~nothing~~ |
| Save memory | `memory_save` | ~~cx memory diary via Bash~~ |
| Search memory | `memory_list` | ~~cx memory search via Bash~~ |
| Link memory | `mcp__cortex__memory(action="link")` | ~~cx memory via Bash~~ |
| Search learnings | `learnings_relevant` | ~~nothing~~ |
| List learnings | `learnings_list` | ~~cx learnings list via Bash~~ |
| Create branch | `git_branch` | ~~git checkout -b via Bash~~ |
| Create PR | `git_pr` | ~~gh pr create via Bash~~ |
| Merge PR | `git_merge` | ~~gh pr merge via Bash~~ |
| Changelog | `git_changelog` | ~~git log via Bash~~ |
| Index LSP | `lsp_index` | ~~cx lsp index via Bash~~ |
| Symbols in code | `lsp_symbols` | ~~Glob/Grep~~ |
| Global symbol search | `lsp_workspace_search` | ~~Grep/Glob~~ |
| Go to definition | `lsp_definition` | ~~Grep~~ |
| Find references | `lsp_references` | ~~Grep~~ |
| Symbol info | `lsp_hover` | ~~Read~~ |
| Replace code | `lsp_replace_symbol` | ~~Edit~~ |
| Rename symbol | `lsp_rename_symbol` | ~~Edit with replace_all~~ |
| Verify task | `mcp__cortex__verify_task(task_id="CX-N")` | ~~go build/test via Bash~~ |
| Extract rules | `business_rule_extract` | ~~manual analysis~~ |
| Epics | `epic_link_task` / `epic_tasks` | ~~nothing~~ |
| External DB | `db_query` / `db_schema` | ~~psql via Bash~~ |
| Plans | `plan_create` / `plan_get` / `plan_submit` / `plan_approve` / `plan_reject` | ~~nothing~~ |
| Brainstorm | `mcp__cortex__brainstorm(action="create/...")` | ~~nothing~~ |
| Phases | `phase_get` / `phase_update` / `phase_complete` | ~~nothing~~ |
| DoD | `dod_list` / `dod_add` / `dod_check` / `dod_init` | ~~nothing~~ |
| Agent orchestration | `agent_spawn` / `task_orchestrate` | ~~nothing~~ |
| Controller | `controller_init` / `controller_spawn` / `controller_status` | ~~nothing~~ |

**Reflection tools - USE at key moments:**
- `think_about_task_adherence` → Before significant code changes
- `think_about_collected_information` → After research/code reading
- `think_about_whether_you_are_done` → Before declaring task as complete

**Exceptions (when to use native Claude Code tools):**
- `Read` → To read file contents (MCP has no file reading equivalent)
- `Edit`/`Write` → To edit/create files when LSP replace is not appropriate
- `Glob` → For quick file search by pattern (MCP has no glob)
- `Bash` → For system commands that MCP does not cover (make, go install, etc.)

**NEVER use `cx` via Bash when an equivalent MCP tool exists.**

### 1. Task Management - ALWAYS use Cortex MCP

```
# CREATE task before any work
task_create(title="Task title", type="feature|bug|chore")

# START task before implementing
task_update(id="CX-N", status="progress")

# FINISH task when complete
task_update(id="CX-N", status="done")
```

**NEVER** work without an associated Cortex task.

### 2. Sync with Claude Code Internal Tasks

When using Claude Code's internal TaskCreate/TaskUpdate:
- **ALWAYS** create the corresponding task in Cortex first with `task_create`
- **ALWAYS** keep IDs aligned (use CX-N as reference)
- **ALWAYS** update both systems when changing status

Correct flow:
```
1. task_create(...)                          → Creates CX-N in Cortex
2. TaskCreate (internal)                     → Creates internal tracking referencing CX-N
3. task_update(id="CX-N", status="progress") → Marks in progress
4. TaskUpdate status=in_progress             → Marks in progress internally
5. [work...]
6. task_update(id="CX-N", status="done")     → Marks done in Cortex
7. TaskUpdate status=completed               → Marks done internally
```

### 3. Memory - Search BEFORE asking

```
# ALWAYS search context before asking user questions
memory_list(search="relevant term")
```

### 4. Git Workflow (Conventional Commits)

- **NEVER** commit without an associated task
- Branch: `git_branch(task_id="CX-N")` → creates branch automatically
- PR: `git_pr` → push + create PR + move task to `review`
- Merge: `git_merge` → squash merge + delete branch + move task to `done`
- Changelog: `git_changelog(from_tag="v0.1.0")` → generates grouped changelog

**Conventional Commits (required):**

Format: `type(scope): description (CX-N)`

| Task Type | Commit Type |
|-----------|-------------|
| `feature` | `feat` |
| `bug` | `fix` |
| `chore` | `chore` |
| `debt` | `refactor` |

Additional types: `docs`, `perf`, `test`, `ci`, `build`, `style`, `revert`

The MCP generates automatically:
- **PR title** in conventional commits format (via `task.FormatCommitMessage`)
- **Squash merge message** with `--subject` in the correct format
- **Changelog** grouped by type (Features, Bug Fixes, etc.)

Validation hook (cortex-plugins) warns if commit message doesn't follow the format.

```
# Task status flow (100% automated via MCP):
# backlog → progress → review → done
#    ↑         ↑          ↑       ↑
# task(create) task(update) git(pr) git(merge)
```

### 5. Session End

At the end of a significant work session:
```
mcp__cortex__memory(action="save", type="diary", title="Session ...", content="Summary of what was done")
```

### 6. Status Check

At the start of any work, check current state:
```bash
mcp__cortex__status()                 # See project overview
mcp__cortex__task(action="list")      # See existing tasks
```

### 7. 🧠 Development Workflow - Choose the right mode

**RULE:** Before implementing, evaluate complexity and choose the appropriate mode:

```
┌─────────────────────────────────────────────────────────────────┐
│                    DECISION TREE                                 │
└─────────────────────────────────────────────────────────────────┘
                           │
                           ▼
              ┌─────────────────────────┐
              │ Is the solution clear?  │
              └───────────┬─────────────┘
                    │           │
                   Yes          No
                    │           │
                    ▼           ▼
         ┌──────────────┐  ┌──────────────┐
         │ Is it        │  │ /brainstorm  │
         │ complex?     │  │ "title"      │
         └──────┬───────┘  └──────────────┘
           │         │           │
          No         Yes         ▼
           │         │     Explore ideas
           ▼         ▼     Vote, decide
    ┌──────────┐ ┌──────────┐  │
    │ cx add   │ │ /plan    │  ▼
    │ + impl   │ │ "title"  │ /plan or
    └──────────┘ └──────────┘ brainstorm_to_plan
                      │              │
                      ▼              ▼
               Document         ┌──────────┐
               approach         │ cx add   │
                    │           │ + impl   │
                    ▼           └──────────┘
              ┌──────────┐
              │ cx add   │
              │ + impl   │
              └──────────┘
```

#### When to use `/brainstorm`:
- 🧠 New feature **without clear design**
- 🤔 **Multiple approaches** possible (which DB? which lib?)
- ⚖️ Need to **explore trade-offs** before deciding
- 💡 Want to **capture ideas** before committing

```bash
/brainstorm "Authentication system"
# → Adds ideas: OAuth, JWT, Session
# → Votes and selects the best
# → Converts to Plan when ready
```

#### When to use `/plan`:
- 📝 Design **already defined**, needs documentation
- 🔄 Converting **brainstorm to executable plan**
- 📋 **Complex feature** that needs a spec before code
- 👥 Needs **review/approval** before implementing

```bash
/plan "Refactor payments module"
# → Writes design/approach in markdown
# → Adds comments if needed
# → Approves and creates tasks
```

#### When to go directly to Task + `/implement`:
- ✅ Bug fix with **known cause**
- ✅ **Small feature** with clear scope
- ✅ **Well-defined change** requested by user
- ✅ Following an **already approved plan**

```bash
cx add "Fix login timeout" --type bug
/implement CX-N
```

### 8. 🤖 Agent Workflow - USE `/implement` for code tasks

**CRITICAL RULE:** For ANY code implementation task, use the `/implement` skill:

```bash
/implement CX-N              # Runs workflow for existing task
/implement "Add new feature" # Creates task and runs workflow
```

**The 3-agent workflow is REQUIRED for:**
- ✅ Implementing new features
- ✅ Fixing bugs
- ✅ Significant refactorings
- ✅ Adding tests
- ✅ Any non-trivial code change

**DO NOT use the workflow for:**
- ❌ Small fixes (typos, formatting)
- ❌ Documentation updates
- ❌ Questions about code
- ❌ Analysis/exploration without implementation

**Workflow flow:**
```
┌──────────┐     ┌───────────┐     ┌────────┐
│ research │ ──▶ │ implement │ ──▶ │ verify │ ──▶ done
└──────────┘     └───────────┘     └────────┘
     │                │                 │
     ▼                ▼                 ▼
  Understands      Writes           Tests and
  codebase         code             validates
  + creates plan   + follows plan   + completes
```

**MCP commands used:**
```
agent_spawn(task_id=task_id)              # Start agent
agent_report(session_id=..., status=...)  # Report progress
agent_sessions()                          # List sessions
```

### 9. 🔍 LSP Tools - Use Cortex for code analysis

**RULE:** For code analysis (symbols, definitions, references), use **Cortex MCP** LSP tools:

```
lsp_symbols(file=file)                                         # Symbols in file
lsp_workspace_search(query=query)                              # Global symbol search
lsp_definition(file=file, line=line, column=column)            # Go to definition
lsp_references(file=file, line=line, column=column)            # Find references
lsp_hover(file=file, line=line, column=column)                 # Symbol info
lsp_replace_symbol(file=file, symbol_name=symbol)              # Replace code
lsp_rename_symbol(file=file, line=line, column=column, new_name=name)  # Rename
lsp_index(language=language)                                   # Trigger indexing
```

**DO NOT use external plugins** for LSP (serena, gopls-lsp, rust-analyzer-lsp) - Cortex already integrates gopls and rust-analyzer internally.

**Supported languages:** Go, Rust, TypeScript, Python, Elixir

**Initialization:** LSP starts automatically on first call.

```
lsp_index(language="elixir")  # Reindex Elixir project
lsp_index()                    # Reindex all supported languages
```

### 10. 📚 Learnings System - Continuous Improvement

Cortex extracts and applies learnings automatically to improve code quality over time.

**Agents MUST fetch learnings at the start of workflow:**
```
# Research - learn from the past
learnings_relevant(task_type="feature", domain="go")

# Implement - apply success patterns
learnings_relevant(task_type="feature")

# Verify - know failure patterns
learnings_list(type="failure_pattern", limit=5)
```

**Learning types:** `success_pattern` | `failure_pattern` | `domain_knowledge` | `user_feedback`

**Requires:** `OPENAI_API_KEY` for automatic extraction

---

<!-- cortex-rules -->
<!-- cortex-memory-rules -->
<!-- Rules will be auto-generated here by: cx memory export -->
<!-- /cortex-memory-rules -->
