# Doom Emacs Configuration

**Author:** Devin Davis
**Date:** 2025-10-14

This is a Doom Emacs configuration using a literate programming approach with `config.org`. This document outlines how my emacs configuration should be modified by LLMs.

## IMPORTANT: Maintaining This Documentation

**This file must be kept up-to-date whenever the configuration changes.**

When working with AI assistants (Claude Code, GPTel, etc.), if you make changes to:
- `config.org` - Update relevant sections describing customizations, functions, or settings
- `packages.el` - Update the "Key Packages" section
- `init.el` - Update the "Enabled Doom Modules" section
- Keybindings - Update the "Keybinding Conventions" section
- Directory structure - Update the "Directory Structure" section

AI assistants should proactively update this file when making configuration changes to ensure it remains an accurate reference for future sessions.

## Project Structure

- `config.org` - Main configuration source (literate org-mode file)
- `config.el` - Tangled output from config.org (auto-generated)
- `init.el` - Doom module configuration
- `packages.el` - Package declarations
- `.gitignore` - Git ignore rules
- `.tool-versions` - Version management (asdf)

## Key Configuration Patterns

### Literate Configuration
The primary configuration is written in `config.org` using org-babel code blocks with `:tangle ./config.el`. When saved, these blocks are tangled into `config.el`.

**Important**: Edit `config.org`, not `config.el` directly. The `.el` file is auto-generated.

### Doom Emacs Conventions
When adding new configuration, **always follow Doom Emacs conventions**:

- Use `use-package!` (not `use-package`) for package configuration
- Use `after!` for lazy-loading configuration hooks
- Use `map!` for keybindings (not `define-key` or `bind-key`)
- Use `setq!` for setting variables (not plain `setq`)
- Use `add-hook!` for adding hooks (not `add-hook`)
- Declare packages in `packages.el` before configuring them in `config.org`
- Use `:config` blocks within `use-package!` for package setup
- Prefer Doom's built-in modules over manual package configuration when available
- Check existing configuration patterns in `config.org` for consistency

Reference: [Doom Emacs Documentation](https://docs.doomemacs.org/latest/)

### Package Configuration Format Standard

All package configurations in `config.org` **must follow this consistent literate programming format**:

#### Structure Template

```
## Package Name

Brief introduction paragraph explaining what the package does, its primary
purpose, and how it fits into the workflow.

Reference: https://github.com/author/package-name

### Configuration

Explanatory prose about what this configuration accomplishes. This should explain
the "why" before showing the "what".

```elisp
;; Use inline comments to explain specific settings
(use-package! package-name
  :config
  ;; Configuration with clear, descriptive comments
  ...)
```

### Keybindings

(Optional section if the package has keybindings)

```elisp
(map! :leader
      (:prefix ("x" . "+prefix-name")
        "k" #'function-name))  ; Describe what this keybinding does
```
```

#### Key Requirements

1. **Documentation First**: Always write explanatory prose before code blocks (except for Keybindings sections, which can go directly to the code)
2. **Clear Structure**: Use level 2 heading (`##`) for package name, level 3 (`###`) for configuration sections
3. **Reference Links**: Include `Reference:` link to official repository after introduction
4. **Inline Comments**: Add comments for each setting, keybinding, and non-obvious code
5. **DRY Principle**: Extract constants and helper functions to avoid duplication
6. **Tangle Blocks**: All code must use `#+begin_src elisp :tangle ./config.el`
7. **Subsections**: Use level 4 headings (`####`) when grouping related configurations

#### Well-Structured Examples

See these package configurations as reference implementations:
- **Claude Code** (config.org:731) - Complex setup with Basic Setup and Keybindings sections
- **Monet** (config.org:783) - Simple integration with clear explanations
- **Prodigy** (config.org:811) - Advanced with helper functions, constants, and multiple subsections

#### What to Avoid

- **No code without context**: Never add code blocks without preceding prose explanation
- **No flat structure**: Don't skip heading levels or use inconsistent nesting
- **No undocumented functions**: Every custom function needs explanation
- **No magic values**: Extract constants with descriptive names
- **No missing references**: Always link to official documentation/repository

## Enabled Doom Modules

### Completion
- `company` - Code completion backend
- `vertico` - Modern completion UI

### UI
- `doom` - DOOM theme system
- `doom-dashboard` - Splash screen
- `treemacs` - Project file browser (width: 45)
- `minimap` - Code minimap
- `modeline` - Status line
- `popup` - Window management (with popwin configuration)
- `tabs` - Tab bar
- `vc-gutter` - Git diff in fringe

### Editor
- `evil` - Vim emulation
- `file-templates` - Auto-snippets for new files
- `format +onsave` - Auto-formatting on save
- `multiple-cursors` - Multi-cursor editing
- `snippets` - Code snippets

### Language Support
- **JavaScript/TypeScript** (`+lsp +tree-sitter`)
  - ESLint integration
  - Apheleia formatting (prettier)
  - Custom ESLint fix command (`SPC m e f`)
- **Ruby** (`+rails +lsp +tree-sitter`)
  - Robe mode for Ruby
- **Python** (`+lsp`)
- **C#** (`+lsp +pylsp`)
- **Solidity** (`+lsp`)
- **Org-mode** (`+roam +crypt`)
- **Markdown** (`+grip`)
- **Ledger** - Double-entry accounting
- **YAML** (`+lsp`)
- **Terraform** (`+lsp`)
- **Shell scripts**

### Tools
- `lsp` - Language Server Protocol
- `magit +forge` - Git interface with GitHub/GitLab support
- `tree-sitter` - Advanced syntax parsing
- `eval +overlay` - Code evaluation
- `lookup` - Code navigation
- `pdf` - PDF support
- `pass +auth` - Password manager

### Email
- `mu4e +org +gmail +offlineimap` - Email client configured for devin@devdeveloper.ca

### Other
- `rss +org` - RSS reader (Elfeed)
- `vterm` - Terminal emulator

## Key Packages

### AI/Completion
- `gptel` - AI chat interface (configured for Ollama + GitHub Copilot)
- `copilot` - GitHub Copilot integration
- `claude-code` - Claude Code integration
- `monet` - MCP server for Claude Code

### Org-mode Extensions
- `org-roam` - Zettelkasten note-taking system
- `org-chef` - Recipe management
- `org-jira` - JIRA integration
- `ox-hugo` - Hugo blog export
- `ox-gfm` - GitHub Flavored Markdown export
- `elfeed-score` - RSS feed scoring

### Development Tools
- `robe` - Ruby code intelligence
- `rubocop` - Ruby static code analyzer and formatter
- `rspec-mode` - RSpec testing integration for Ruby
- `rake` - Rake task runner for Ruby projects
- `web-mode` - EJS template support
- `kubernetes` - Kubernetes management
- `prodigy` - Service/process manager

### Utilities
- `dashboard` - Custom dashboard
- `popwin` - Popup window manager
- `nov` - EPUB reader
- `exec-path-from-shell` - macOS PATH integration

## Key Customizations

### Display Settings
- Font size: 16
- Theme: `doom-palenight`
- Relative line numbers enabled
- Treemacs width: 45 columns

### Org-mode
- **Agenda files**: Pulls from multiple org-roam directories
- **Capture templates**:
  - `c` - Cookbook recipe (via org-chef)
  - `g` - Generic note
  - `t` - Work ticket
  - `p` - Event Temple project
  - `i` - Personal project
  - `b` - Blog post
  - `s` - Source material
  - `P` - Person
- **Daily templates**:
  - `d` - Default daily
  - `e` - Event Temple standup

### LSP Configuration
- Increased idle delay (500ms) to reduce CPU usage
- Disabled on-type formatting
- Disabled auto-imports for TypeScript
- File watch threshold: 1000 files
- Ignored directories: `.git`, `node_modules`, `build`, `dist`
- Using Flycheck as diagnostics provider

## Keybinding Conventions

### Leader Key Prefixes
- `SPC c` - Code actions
- `SPC c f` - Code formatting
- `SPC c l` - LSP actions
- `SPC o` - Open applications
- `SPC l` - GPT/AI tools
- `SPC l c` - Claude Code
- `SPC r` - Prodigy (run services)
- `SPC d` - Dirvish (file manager)
- `SPC m` - Major mode (localleader)

### Common Bindings
- `SPC o c` - Open GPTel
- `SPC o r` - Open Elfeed
- `SPC o k` - Open Kubernetes
- `SPC o m` - Open mu4e (email)
- `SPC d` - Dirvish quick access
- `SPC c R` - Projectile replace

### Forge Bindings (`SPC g f`)
- `SPC g f f` - Forge dispatch
- `SPC g f p` - Browse pull requests
- `SPC g f i` - Browse issues
- `SPC g f m` - Smart merge PR (update branch + wait for CI)

#### Forge Topic Mode (PR view, localleader: `SPC m`)
- `SPC m m m` - Smart merge (update branch + wait for CI)
- `SPC m m c` - Complete merge (after CI passes)
- `SPC m m r` - Reset PR merge status

### Ruby Mode Bindings (localleader: `SPC m`)

#### RuboCop (`SPC m r`)
- `SPC m r p` - Run RuboCop on project
- `SPC m r f` - Run RuboCop on current file
- `SPC m r d` - Run RuboCop on directory
- `SPC m r a` - Auto-correct current file
- `SPC m r A` - Auto-correct project
- `SPC m r F` - Format current file (with buffer reload)

#### RSpec Testing (`SPC m t`)
- `SPC m t a` - Run all specs
- `SPC m t v` - Run current spec file
- `SPC m t s` - Run spec at point
- `SPC m t r` - Re-run last spec
- `SPC m t t` - Toggle between code and spec file
- `SPC m t f` - Find spec file
- `SPC m t p` - Toggle example pending status

#### Rake Tasks (`SPC m k`)
- `SPC m k k` - Run rake task
- `SPC m k r` - Rerun last rake task
- `SPC m k f` - Find and run rake task
- `SPC m k c` - Regenerate task cache

### Org-Jira Bindings (localleader: `SPC m j`)

#### Projects (`SPC m j p`)
- `SPC m j p g` - Get projects

#### Issues (`SPC m j i`)
- `SPC m j i b` - Browse issue
- `SPC m j i g` - Get issues
- `SPC m j i j` - Get issues from JQL
- `SPC m j i h` - Get issues (head only)
- `SPC m j i f` - Get issues by fix version
- `SPC m j i l` - Update issue labels
- `SPC m j i u` - Update issue
- `SPC m j i w` - Progress issue
- `SPC m j i n` - Progress issue next
- `SPC m j i a` - Assign issue
- `SPC m j i r` - Refresh issue
- `SPC m j i R` - Refresh issues in buffer
- `SPC m j i c` - Create issue
- `SPC m j i k` - Copy issue key

#### Subtasks (`SPC m j s`)
- `SPC m j s c` - Create subtask
- `SPC m j s g` - Get subtasks

#### Comments (`SPC m j c`)
- `SPC m j c c` - Add comment
- `SPC m j c u` - Update comment

#### Worklogs (`SPC m j w`)
- `SPC m j w u` - Update worklogs from org clocks

#### Todo Sync
- `SPC m j t` - Sync todo to Jira

## macOS-Specific Configuration
- `exec-path-from-shell` integration
- SSH_AUTH_SOCK environment variable forwarded
- Conditional loading based on window system

## Important Notes

### When Editing Configuration:
1. Edit `config.org`, NOT `config.el`
2. **DO NOT** prompt or ask to run `doom sync` - the user will run it manually when needed
