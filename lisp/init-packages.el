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
    google-this
    diminish
    paredit ;; TODO
    undo-tree
    restclient
    expand-region
    hl-anything
    company
    highlight-symbol

    ;;; Ruby
    rbenv
    inf-ruby
    rubocop
    rspec-mode
    bundler
    smartparens
    mutant

    ;;; Go-lang
    go-mode
    company-go
    gotest

    ;;; Search
    ag
    anzu ;; Better search mode of file with syntax highlight
    wgrep
    swiper

    ido-vertical-mode ;; Easy navigation on commands
    fuzzy ;; Completion
    helm-ag ;; Help-ag search
    wrap-region ;; Highlight the selected region

    ;;; YAML
    yaml-mode ;; Help on YAML editing

    ;;; UI
    popup
    gruvbox-theme ;; My theme
    gruvbox-theme ;; My theme
    solarized-theme ;; My theme
    atom-one-dark-theme
    atom-dark-theme

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
    diff-hl

    ;;; Markdown
    markdown-mode

    ;;; Org
    org-bullets

    ;;; Docker
    dockerfile-mode

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
    ffap ;; This is for go directly in files
    recentf
    saveplace
    ansi-color
    dired-x
    s
    sh-script
    sgml-mode
    nxml-mode
    yaml-mode
    anzu
    smartscan
    google-this
    undo-tree
    expand-region
    recentf
    saveplace
    wgrep
    highlight-symbol
    flycheck
    js2-refactor
    mutant
    dockerfile-mode))

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

;;; Enabling packages
(global-anzu-mode t) ;; Anzu
(smartscan-mode 1) ;;; Smartscan
(google-this-mode 1) ;; Google this
(undo-tree-mode 1) ;; Undo tree
(wrap-region-mode t) ;; Wrap-region
(hl-highlight-mode 1) ;; Highlight-mode
(global-hl-line-mode 1) ;; Highlight current line
(delete-selection-mode 1) ;; Delete on selected text
(winner-mode 1) ;; Undo and Redo window configuration
(global-company-mode) ;; Autocomplete

(provide 'init-packages)
;;; init-packages.el ends here
