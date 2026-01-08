# emacs-dotfiles
My emacs dot files

## Emacs version

This configuration currently runs on GNU Emacs 30.2 installed from the `emacs-plus@30` Homebrew formula.

### Installing the same build

```bash
brew tap d12frosted/emacs-plus
brew install emacs-plus@30
```

After Homebrew finishes, you can create an alias inside `/Applications` so macOS treats it like a standard app:

```bash
osascript -e 'tell application "Finder" to make alias file to posix file "/opt/homebrew/opt/emacs-plus@30/Emacs.app" at posix file "/Applications" with properties {name:"Emacs.app"}'
```

Launch the app (or run `/Applications/Emacs.app/Contents/MacOS/Emacs --version`) to confirm it reports `GNU Emacs 30.2`.

## Emacs Workflow & Shortcut Guide

This document condenses the most important behaviors baked into this Emacs configuration so you can work quickly across languages. It is intended to be copy-pastable into an LLM prompt when you want the assistant to understand your environment.

### Configuration Architecture

- `init.el` loads every module in `lisp/`. The naming convention is `init-<feature>.el`. Add new modules under `lisp/` and `(require 'init-<feature>)` in `init.el` to enable them.
- `init-packages.el` declares packages through `use-package`/`straight.el`. When you add a new dependency, place the declaration there so it is installed on launch.
- Language- or tool-specific behavior lives in its own `init-*.el` file. When two modules touch the same tool, the more specific module (deeper directory) wins per the `AGENTS.md` guidance.
- Reload a module after editing with `M-x eval-buffer` inside the file or restart Emacs for a clean load.

### Global Navigation & Commands

#### Projects & Buffers

| Keys | Command | Notes |
| --- | --- | --- |
| `s-t` | `projectile-find-file` | Jump to files in the current project.
| `s-o`, `C-c o` | `projectile-switch-project` | Open project chooser; `projectile-project-search-path` is `~/code/`.
| `C-c p` | `projectile-command-map` | Prefix for project utilities (grep, cache, paths, etc.).
| `C-c p s` | `consult-grep` | Ripgrep inside the current project root.
| `C-c p w` / `C-c p W` | Custom copy relative path | Copies file path, optionally including line.
| `C-x b`, `M-l` | `consult-buffer` | Switch buffers with preview; honours Vertico/Orderless completion.
| `M-L` | `consult-project-buffer` | Filter buffers to the active project.
| `s-k` | `kill-this-buffer` | Close current buffer quickly.
| `C-*` | `close-all-buffers` | Custom helper to wipe the buffer list.
| `C-x 9` | `other-window-kill-buffer` | Close buffer in the next window.

#### Search, Selection & Command Palette

| Keys | Command | Notes |
| --- | --- | --- |
| `s-f` | `swiper` | Fuzzy search within the current buffer.
| `s-F` | `ag-project` | Project-wide search via `ag` (silversearcher).
| `M-s r` | `consult-ripgrep` | Another fast ripgrep entry point.
| `M-s d` | `consult-find` | File finder on a directory.
| `M-s g` | `consult-grep` | Plain grep fallback.
| `M-s k` / `M-s u` | `consult-keep-lines` / `consult-focus-lines` | Filter buffer lines.
| `C-c M-x` | `consult-mode-command` | Mode-aware command palette.
| `M-y`, `C-M-y` | `consult-yank-pop`, `consult-yank-from-kill-ring` | Browse kill ring entries.
| `M-s e` (in isearch) | `consult-isearch-history` | Reuse previous searches.

#### Windows & Layout

| Keys | Command | Notes |
| --- | --- | --- |
| `C-<tab>` | `other-window -1` | Cycle through windows in reverse.
| `M-2` / `M-3` | `split-window-vertically` / `split-window-horizontally` |
| `s-K` / `s-O` | `delete-window` / `delete-other-windows` |
| `s-U` / `s-P` | `vsplit-last-buffer` / `hsplit-last-buffer` | Split and show the last buffer automatically.
| `s-I` | `swap-buffers-in-windows` | Swap the two visible buffers.
| `s-Shift-<arrows>` | `windmove-*` | Move focus among windows.
| `M-i` | `other-frame` | Cycle frames.

#### Editing & Regions

| Keys | Command | Notes |
| --- | --- | --- |
| `s-/` | `comment-dwim-line` | Comment/uncomment current line.
| `C-q` | `comment-or-uncomment-region` | Region aware toggle.
| `C-j` / `C-k` | Custom word navigation | `backward-word` / `forward-word` remapped for easy reach.
| `M-d` / `M-h` | Custom word kill | Drop text without touching the kill ring.
| `M-n` / `M-p` | Jump 5 lines down/up | Handy when skimming long buffers.
| `C-c i` | `indent-region-or-buffer` | Indents selection or entire buffer; specialized per language.
| `C-:` | `join-line` | Join the current line with the previous.
| `s-d` | `duplicate-line` | Clone the current line below.
| `Control-Shift-RET` | `smart-open-line-above` | Insert a properly indented line above.
| `s-l` | `goto-line` | Prompt for line number.
| `s-u` | `revert-buffer` | Reload buffer from disk.
| `C-c d` / `C-c r` | Delete / rename current file & buffer | Wrapped helpers so VC state stays in sync.
| `M-s-¨` | `pgt-view-url-source-code` | Fetch and view remote HTML/JS source.

#### Multiple Cursors & Structural Editing

| Keys | Command | Notes |
| --- | --- | --- |
| `C->`, `C-<` | `mc/mark-next-like-this`, `mc/mark-previous-like-this` |
| `M-@` | `mc/mark-all-dispatch` | Smart dispatcher: add cursors, mark region, or select all instances.
| `M-'` | `mc/align` | Column-align the active cursors.
| `M-#` | `mc/insert-numbers` | Sequential numbering across cursors.
| `C-c C-t v/l/m/p/c` | Ruby refactor helpers | Extract to variable/let/method, add param, extract constant.

### Tree-sitter & Combobulate

- Emacs 30’s native Tree-sitter build is enabled everywhere via `treesit-auto`. Run `M-x treesit-auto-install-all` the first time to download grammars for Bash, CSS, Go, HTML, JavaScript/TypeScript/TSX, JSON, Python, Ruby, Rust, TOML, and YAML.
- `major-mode-remap-alist` makes those files open in their `*-ts-mode` equivalents automatically (e.g., `python-ts-mode`, `go-ts-mode`, `js-ts-mode`, `typescript-ts-mode`, `tsx-ts-mode`, `rust-ts-mode`, `ruby-ts-mode`). Language hooks were updated so formatter/test bindings keep working regardless of the backend.
- `combobulate-mode` turns on by default for Tree-sitter buffers it supports (Python, JS/TS/TSX, JSON, CSS, YAML). Use the `C-c o` prefix for structural commands like slurp/barf, wrap, splice, and move, and fall back to Parinfer for Lisp buffers.
- Lisp-family modes (Emacs Lisp, Clojure, Scheme, Common Lisp, plus their `*-ts-mode` variants) automatically enable `parinfer-mode`. Toggle it per buffer with `M-x parinfer-mode` or switch Parinfer’s behavior via `M-x parinfer-toggle-mode`.

#### Git & Tooling

| Keys | Command | Notes |
| --- | --- | --- |
| `C-x v a` | `git-add-current-buffer` | Stage file without leaving Emacs.
| `M-x magit-status` | Magit dashboard | One-stop Git UI.
| `C-c t m` | `git-timemachine` | Browse file history with timemachine view.
| `git-gutter-mode` / `diff-hl-mode` | Enabled globally | Visual margins for changes.
| `s-z`, `s-Z` | `undo-tree-undo`, `undo-tree-redo` | System-style undo/redo.

#### Terminals, AI & Utilities

| Keys | Command | Notes |
| --- | --- | --- |
| `C-c e` | `custom/projectile-eshell` | Launch project-scoped Eshell in a split.
| `C-c C-'` | `claude-code-ide-menu` | MCP tooling menu for Claude Code integration.
| `M-x gptel` | gptel session | Chat with configured MCP agents (`context7`, `serena`).
| `C-.`, `C-;`, `C-h B` | `embark-act`, `embark-dwim`, `embark-bindings` | Contextual actions on the thing at point.
| `C-<return>` (Copilot) | `copilot-accept-completion` | Accept GitHub Copilot suggestion in programming buffers.

- Run `M-x copilot-login` once to authorize with GitHub before relying on completions.

### Completion & Search Stack

- **Vertico + Orderless** provide flexible minibuffer completion with posframe popups for better focus. Use `TAB` to cycle candidates and `M-s` / `M-r` inside the minibuffer to browse history.
- **Consult** augments built-ins with preview-enabled navigation (`consult-buffer`, `consult-line`, `consult-imenu`). Most `C-x` bindings have been remapped accordingly.
- **Corfu** supplies in-buffer completions. It is global; press `TAB`/`RET` to accept and `M-n`/`M-p` to cycle proposals.
- **Embark** attaches contextual actions to completions and buffer text: `C-.` opens the action menu, `C-;` smartly picks the most relevant action, and `C-h B` reveals bindings.
- **Anzu** enhances incremental search/replace with live counters (`anzu-query-replace`, remapped from the stock commands).

### Language Playbooks

Each language module mirrors the Ruby workflow wherever possible: format before save, surface fast test commands, and provide quick navigation.

#### Ruby

- `init-ruby.el` enables `global-rbenv-mode`, automatically switches `inf-ruby` after running specs, and tweaks indentation to avoid noisy SMIE behavior.
- `rspec-mode` defaults give you a full test runner inside Emacs; output is displayed in a compilation buffer using the smaller font from `init-appearance.el`.
- Custom helpers: typing `#` inside double quotes auto-inserts interpolation braces, and `C-c , ,` jumps between source files and their specs (`senny-ruby-open-spec-other-buffer`).

| Keys / Commands | Workflow |
| --- | --- |
| `C-c , a` / `C-c , v` / `C-c , s` | Run entire suite, current file, or current spec example via `rspec-mode`.
| `C-c , r` | Rerun the last spec command.
| `C-c C-t v/l/m/p/c` | Ruby-refactor entry points shared with other languages.
| `M-x inf-ruby` or `C-c C-z` (in REPL) | Drop into an interactive console aligned with the current project.

#### Python

- `python-mode` is tuned for 4-space indentation, uses `/opt/homebrew/bin/bash` as the shell, and integrates with whichever project manager is detected (`uv`, `poetry`, or vanilla `pip`).
- `lsp-pyright` starts automatically when available; completion/diagnostics flow through Corfu and Flycheck.
- `python-pytest` keybindings mirror the Ruby cadence, and `python-black` (if installed) formats on save; otherwise it falls back to structural indentation.
- `compile-command` is set per buffer to run the current file through the detected project manager. Use `M-x python-run-buffer` to execute directly.

| Keys / Commands | Workflow |
| --- | --- |
| `C-c , a` | `python-pytest-dispatch` → full suite with project manager wrapper.
| `C-c , v` | `python-pytest-file-dwim` → tests for the current file.
| `C-c , s` | `python-pytest-function-dwim` → the closest test function/class.
| `C-c i` | `python-smart-indent-region-or-buffer` → run Black if available, else indent manually.
| `M-x python-run-buffer` / `compile` | Execute buffer with `uv run`, `poetry run`, or raw `python`.
| `M-x python-install-dependencies` | Sync dependencies based on project manager.

#### JavaScript & TypeScript

- `js-ts-mode` is the default major mode for `.js` when Tree-sitter is available, keeping indentation at 2 spaces. Falling back to `js-mode` still auto-enables `js2-minor-mode`/`js2-refactor` so legacy projects behave the same.
- `.ts` openings prefer `typescript-ts-mode` and `.tsx` uses `tsx-ts-mode`; older Emacs builds fall back to `typescript-mode` + `web-mode` automatically. Completion is still handled by `company` + `flycheck`, and Tide remains the default backend (set `(setq my/typescript-backend 'lsp)` to opt into `lsp-mode`).
- `combobulate-mode` attaches to all Tree-sitter JS/TS buffers, exposing structural edits behind `C-c o` (wrap/slurp/barf, move by syntax node, etc.). Tide continues to format before save via `tide-format-before-save`.
- For Node/React projects, rely on `projectile-test-command` or `M-x compile` with `npm test`, keeping parity with Ruby’s `compile` usage.

| Keys / Commands | Workflow |
| --- | --- |
| Save in `.ts`/`.tsx` | Runs Tide formatting before save.
| `M-x tide-rename-symbol`, `M-x tide-project-errors` | IDE-like refactors and diagnostics.
| Set `(setq my/typescript-backend 'lsp)` | Opt into `lsp-mode` instead of Tide, leaning on the `lsp-terraform`/`lsp` stack.

#### Go

- GOPATH/GOROOT paths wire up regardless of whether `go-mode` or `go-ts-mode` is active; Tree-sitter buffers get the same setup and formatter hooks.
- `company-go` feeds completions; `go-oracle-mode` is loaded (ensure the tool is installed under `$GOPATH/src/...`).
- `gotest` bindings imitate the Ruby/Python test layout.

| Keys / Commands | Workflow |
| --- | --- |
| `C-c C-r` / `C-c C-g` / `C-c C-f` / `C-c C-k` | Remove unused imports, jump to imports, format, or open docs respectively.
| `C-x f` / `C-x t` / `C-x p` | Run go-test on file, current test, or entire project.
| `C-x x` | `go-run` the current package; use `M-x go-run-buffer` for ad-hoc files.

#### Terraform & Infrastructure as Code

- `.tf` and `.tfvars` trigger `terraform-mode` with automatic `terraform-format-on-save` and `lsp-deferred` (via `lsp-terraform`).
- Launch `M-x terraform-plan` or `M-x terraform-apply` from buffers; diagnostics stream through LSP and Flycheck.
- Use `consult-ripgrep` to jump between modules quickly; `projectile` keeps state tied to each workspace directory.

| Keys / Commands | Workflow |
| --- | --- |
| `C-c C-r` | `terraform-format-buffer` before save ensures consistent formatting.
| `M-x terraform-plan` / `M-x terraform-apply` | Run plans/applies scoped to the current project.

#### Docker & Kubernetes

- `Dockerfile` buffers automatically load `dockerfile-mode` with helpful indentation and syntax highlighting.
- `M-x docker` opens the interactive Docker dashboard to inspect containers, images, volumes, and logs without leaving Emacs.
- `kubernetes` provides `M-x kubernetes-overview` (cluster browser) and contextual commands for logs, exec, and resource scaling.
- `k8s-mode` layers schema-backed completion atop Kubernetes YAML manifests. Enable it with `M-x k8s-mode` inside `yaml-mode` buffers to get validation and doc lookups.

#### Elixir

- Saving an Elixir buffer triggers `elixir-format` (same habit as Ruby’s formatter).
- Optional `alchemist` integration is scaffolded but commented out; enable the hooks if you need Mix/iex helpers (`C-c , a`, `C-c , s`, etc.).
- REPL integration mirrors the Ruby/inf-ruby feel when `alchemist` is active.

#### Clojure

- `clojure-mode` enables `subword-mode` and extra font-locking for Midje facts.
- `cider` launches with `M-x cider-jack-in`; REPL windows pop to the front on connect, and errors automatically surface.
- Custom commands: `C-c C-v` starts the HTTP server scaffold, `C-M-r` triggers `(user/reset)`, and `C-c u` sets the namespace to `user`.
- `.edn`, `.boot`, `.cljs`, and `lein-env` files are mapped to the right modes automatically.

#### Supporting Utilities

- **REST**: `.restclient` files open in `restclient-mode`.
- **SQL**: SQL REPLs avoid line truncation and include helpers to uppercase keywords before save.
- **SSH**: `ssh-config-mode` lights up SSH config files (`~/.ssh/config`, `known_hosts`, etc.) with syntax highlighting.
- **Org & Markdown**: `init-org.el` and `markdown-mode` provide rich editing with bullet helpers and preview integrations.
- **Appearance**: `dimmer` and the custom compilation font function keep focused buffers in view; use `M-x transparency` to adjust frame opacity.

### Git & Project Operations

- Use `M-x magit-status` for all Git actions—staging, commits, diffs, and rebasing—without leaving Emacs.
- `git-gutter` and `diff-hl` mark modified lines in the fringe. Toggle visibility with `M-x git-gutter-mode` if needed.
- `projectile` caches file lists per project; invalidate with `C-c p i` (inside the projectile map).
- `C-x v a` stages the active buffer, keeping terminal Git out of the loop.

### Translating Ruby Muscle Memory Elsewhere

- **Test cadence**: `C-c , a/v/s` is shared by Ruby (RSpec) and Python (pytest). For Go, `C-x f/t/p` mirrors “file/spec/project”. For JavaScript/TypeScript, lean on `M-x compile` using the project’s test script to maintain the same “focus → run” rhythm.
- **Formatting before save**: Ruby (`rubocop` via `bundle exec rubocop -A` if you wire it), Python (`python-black`), Go (`gofmt`), TypeScript (Tide/LSP), Terraform (`terraform fmt`), and Elixir (`mix format`) all hook into save events, so you can trust the buffer to stay tidy.
- **Spec/source toggling**: Use `C-c , ,` in Ruby; replicate the habit with Projectile’s `C-c p f` + `consult-line` in other languages.
- **Interactive consoles**: `inf-ruby`, `python-shell`, `cider-repl`, and `iex` (via alchemist) can all live in splits opened with `C-c e` for quick evaluation.

### Quick Reference: Tests & Formatting

| Language | Run All Tests | Run Current File/Test | Format Buffer |
| --- | --- | --- | --- |
| Ruby | `C-c , a` (`rspec-verify-all`) | `C-c , v` / `C-c , s` | `bundle exec rubocop -A` via `M-x compile` or external alias.
| Python | `C-c , a` (`python-pytest-dispatch`) | `C-c , v` / `C-c , s` | `C-c i` → Black / indent fallback.
| Go | `C-x p` (`go-test-current-project`) | `C-x f` / `C-x t` | `gofmt-before-save` (auto).
| TypeScript | `M-x compile` → `npm test` | `M-x compile` → `npm test -- <pattern>` | Tide/LSP auto-format on save.
| Terraform | `M-x terraform-plan` (workspace) | `M-x terraform-plan` with prefix to target modules | Auto `terraform fmt` on save.
| Docker/K8s | `M-x docker` dashboards | `M-x kubernetes-overview` | YAML formatting via `k8s-mode` + `yaml-mode`.
| Elixir | `M-x mix test` | `M-x mix test path:line` | `elixir-format` on save.
| Clojure | `C-c C-t a` (`cider-test-run-project-tests`) | `C-c C-t t` (test at point) | `clojure-mode` indentation helpers.

### Tips & Troubleshooting

- **Switch TypeScript backend**: `(setq my/typescript-backend 'lsp)` before loading a project to use `lsp-mode` + `typescript-language-server` instead of Tide.
- **Refresh projectile cache**: `C-c p i` (invalidate) if files seem missing.
- **Reload keybindings**: After editing `init-keybindings.el`, run `M-x eval-buffer` to reapply without restarting.
- **Compilation buffers**: Because `init-appearance.el` shrinks fonts in `compilation-mode`, keep test panes readable even on small screens.
- **Large projects**: Use `consult-ripgrep` (`M-s r`) combined with orderless search patterns to land on functions quickly.
- **AI tooling**: `C-c C-'` opens the Claude Code IDE menu; `M-x gptel` connects to the MCP stack defined in `opencode.json` (`context7`, `serena`).

This reference should give you enough muscle memory cues to operate every major feature consistently across languages. Consult the corresponding `init-*.el` module whenever you need to drill deeper or customize a workflow further.
