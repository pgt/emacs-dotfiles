;;; init-packages.el --- Declare, install and update Emacs packages.
;;; Commentary:
;;; Code:
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar my-packages
  '(
    ;;; General
    auto-package-update
    s ;; TODO: need search
    flycheck ;; Syntax checking for over 60 programming languages
    smartscan ;; Move between symbols (* of vim)
    helm ;; Help in navigation
    projectile ;; Project interaction
    helm-projectile ;; Navigation for Projectile
    neotree ;; Like NerdTree on Vim, F8
    google-this
    diminish
    paredit ;; TODO
    undo-tree
    guide-key
    restclient
    expand-region
    hl-anything
    company
    indent-guide
    back-button
    highlight-symbol

    ;;; Ruby
    rbenv
    inf-ruby
    rubocop
    rspec-mode
    bundler
    smartparens
    ruby-refactor

    ;;; Go-lang
    go-mode
    company-go
    gotest

    ;;; Search
    ag
    anzu ;; Better search mode of file with syntax highlight
    wgrep

    ido-vertical-mode ;; Easy navigation on commands
    fuzzy ;; Completion
    helm-ag ;; Help-ag search
    smex ;; Better interface for M-x
    wrap-region ;; Highlight the selected region

    ;;; YAML
    yaml-mode ;; Help on YAML editing

    ;;; UI
    popup
    gruvbox-theme ;; My theme
    telephone-line

    ;;; HTML
    rhtml-mode

    ;;; Javascript
    js2-mode
    js2-refactor
    tern ;;; PS: it's necessary install tern (npm install -g tern)
    tern-auto-complete

    ;;; Git tools
    magit
    git-timemachine ;; Travel for versions of a file on git
    git-gutter

    ;;; Markdown
    markdown-mode

    ;;; my-package ends here
    )
  "A list of packages to ensure are installed at launch.")

(defvar libs-to-require
  '(cl
    uniquify
    linum
    paredit
    whitespace
    wrap-region
    ffap
    recentf
    saveplace
    ansi-color
    dired-x
    s
    sh-script
    sgml-mode
    nxml-mode
    yaml-mode))

;; package loading
(setq packaged-contents-refreshed-p nil)
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (condition-case ex
     (package-install p)
      ('error (if packaged-contents-refreshed-p
            (error ex)
          (package-refresh-contents)
          (setq packaged-contents-refreshed-p t)
          (package-install p))))))

;; vendor loading
(dolist (lib libs-to-require)
  (require lib))

;; Automagically updating packages
(require 'auto-package-update)
(auto-package-update-maybe)

(provide `init-packages)
;;; init-packages.el ends here
