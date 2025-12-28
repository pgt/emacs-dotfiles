# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a personal Emacs configuration repository following a modular architecture with separate initialization files for different languages, packages, and features. The configuration uses both the traditional package.el system and the modern straight.el package manager.

## Architecture

### Core Structure
- `init.el` - Main entry point that loads all other configuration modules
- `lisp/` - Contains all modular configuration files (init-*.el pattern)
- Package management handled by both package.el (MELPA/GNU) and straight.el

### Configuration Modules

The configuration is split into focused modules:

**Core System:**
- `init-packages.el` - Package declarations and installations
- `init-defaults.el` - Base Emacs settings
- `init-appearance.el` - UI/theme configuration with lambda-themes and lambda-line
- `init-keybindings.el` - Global keybindings (Mac-centric with s- prefix)

**Language Support:**
- `init-ruby.el` - Ruby development with rbenv, rspec-mode, bundler
- `init-elixir.el` - Elixir with alchemist and auto-formatting on save
- `init-clojure.el` - Clojure with CIDER
- `init-go.el` - Go development
- `init-javascript.el` - JavaScript/JS2 mode
- `init-python.el` - Python development
- Language files for Rust, Dart/Flutter, Swift, Kotlin, PHP, etc.

**Tools & Utilities:**
- `init-navigation.el` - File and project navigation
- `init-lsp.el` - LSP configuration (currently disabled)
- `init-org.el` - Org-mode configuration
- `init-magit.el` - Git integration
- Package-specific configs for ag, git-gutter, multiple-cursors, etc.

### Key Features

**Package Management:**
- Uses straight.el for modern package management alongside package.el
- Comprehensive language support across multiple programming languages
- Modern UI with lambda-themes (atom-dark) and lambda-line status bar

**Development Tools:**
- Projectile for project management
- Magit for Git operations
- Language-specific tooling (rbenv, alchemist, CIDER, etc.)
- Search with ag/swiper
- Multiple cursors support

**Appearance:**
- Clean, minimalist UI with disabled toolbars/menubars
- SF Mono font at size 13 with internal borders
- Dimmer for inactive windows
- Bar cursor type
- Window dividers for clean separation

## Working with This Configuration

### File Patterns
- All configuration modules follow `init-*.el` naming convention
- Language-specific files: `init-[language].el`
- Package-specific files: `init-package-[package].el`
- Feature-specific files: `init-[feature].el`

### Adding New Configurations
1. Create new `init-[name].el` file in `lisp/` directory
2. Add `(require 'init-[name])` to `init.el` in appropriate section
3. Use `use-package` declarations for new packages
4. Follow existing patterns for keybindings and hooks

### Keybinding Conventions
- Mac Command key (`s-`) for system-level operations
- `C-c` prefix for custom commands
- Language-specific bindings in respective init files
- Project operations often use `s-` combinations (e.g., `s-o` for project switch)

### Package Installation
New packages should be declared in `init-packages.el` using `use-package` syntax, or can be installed via straight.el for GitHub packages.

## Notable Customizations

- Ruby development with custom indentation fixes and rbenv integration
- Elixir auto-formatting on save
- Mac-centric keybindings (Command key mappings)
- Compilation mode uses smaller fonts for better visibility
- Custom functions for buffer/file operations (rename, delete, duplicate)
- Integration with Claude Code IDE package for AI assistance